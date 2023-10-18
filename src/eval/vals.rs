use std::{fmt::{self, Display, Debug}, rc::Rc, cell::RefCell, convert::Infallible, hint::unreachable_unchecked};
use thiserror::Error;

use crate::parser::ast::{FunctionArg, Statement, Expression};

use super::{scope::Scope, statements::eval_block};

#[derive(Clone, PartialEq)]
pub enum LuaVal {
    Number(f64),
    Bool(bool),
    Nil,
    Function(Function),
    String(Rc<str>),
    NativeFunction(NativeFunction),
}

#[derive(Clone)]
pub struct Function {
    args: Rc<[FunctionArg]>,
    body: Rc<[Statement]>,
    env: Rc<RefCell<Scope>>,
}

pub type LuaResult = Result<LuaVal, EvalError>;

#[derive(Clone)]
pub struct NativeFunction {
    func: Rc<dyn Fn(&FunctionContext) -> LuaResult>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LuaType {
    Number,
    Bool,
    Nil,
    Function,
    String,
}

#[derive(Debug, PartialEq)]
pub enum StmtResult {
    None,
    Return(LuaVal),
    Break,
}

pub trait LuaCallable {
    fn call(&self, args: &[Expression], args_env: &Rc<RefCell<Scope>>) -> LuaResult;
}

pub struct FunctionContext{
    pub args: Vec<LuaVal>,
}

impl LuaVal {
    pub fn into_bool(self) -> Result<bool, TypeError>{
        self.try_into()
    } 

    pub fn into_number(self) -> Result<f64, TypeError>{
        self.try_into()
    }

    pub fn as_func(&self) -> Result<&dyn LuaCallable, TypeError> {
        match self {
            Self::Function(f) => Ok(f),
            Self::NativeFunction(f) => Ok(f),
            v => Err(TypeError(LuaType::Function, v.get_type())),
        }
    }

    pub fn into_str(self) -> Result<Rc<str>, TypeError> {
        self.try_into()
    }

    pub const fn truthy(&self) -> bool {
        !matches!(self, Self::Bool(false) | Self::Nil)
    }

    pub const fn get_type(&self) -> LuaType {
        match self {
            Self::Number(..) => LuaType::Number,
            Self::Bool(..) => LuaType::Bool,
            Self::Nil => LuaType::Nil,
            Self::Function(..) |
                Self::NativeFunction(..) => LuaType::Function,
            Self::String(..) => LuaType::String,
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.func, &other.func)
    }
}

impl NativeFunction {
    pub fn new(func: Rc<dyn Fn(&FunctionContext) -> LuaResult>) -> Self {
        Self { func }
    }
}

impl Display for LuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Nil => write!(f, "nil"),
            Self::Function(..) |
                Self::NativeFunction(..) => write!(f, "function"),
            Self::String(s) => write!(f, "{s}"),
        }
    }
}

impl Display for LuaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number => write!(f, "number"),
            Self::Bool => write!(f, "boolean"),
            Self::Nil => write!(f, "nil"),
            Self::Function  => write!(f, "function"),
            Self::String => write!(f, "string"),
        }
    }
}

impl Debug for LuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Function {
    pub fn new(args: Rc<[FunctionArg]>, body: Rc<[Statement]>, env: Rc<RefCell<Scope>>) -> Self {
        Self { args, body, env }
    }
}

impl LuaCallable for Function {
    fn call(&self, args: &[Expression], args_env: &Rc<RefCell<Scope>>) -> LuaResult {
        let arg_vals: Vec<LuaVal> = args.iter().map(|arg| arg.eval(args_env.clone())).try_collect()?;

        let mut new_env = Scope::extend(self.env.clone());
        self.args.iter().zip(arg_vals).for_each(|(arg, val)| match arg {
            FunctionArg::Identifier(id) => new_env.set(*id, val),
            FunctionArg::Dotdotdot => todo!(),
        });

        match eval_block(&self.body, &RefCell::new(new_env).into())? {
            StmtResult::None => Ok(LuaVal::Nil),
            StmtResult::Return(v) => Ok(v),
            StmtResult::Break => todo!(),
        }
    }
}

impl LuaCallable for NativeFunction {
    fn call(&self, args: &[Expression], args_env: &Rc<RefCell<Scope>>) -> LuaResult {
        let arg_vals: Vec<LuaVal> = args.iter().map(|arg| arg.eval(args_env.clone())).try_collect()?;

        (self.func)(&FunctionContext {
            args: arg_vals,
        })
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum EvalError {
    #[error("TypeError: {0}")]
    TypeError(#[from] TypeError),
    #[error("Unknown identifier '{0}'")]
    UnknownId(Box<str>),
    #[error("bad argument #{0} to '{1}' (value expected)")]
    ExpectedArgument(u8, Box<str>),
    #[error("Too many arguments ({0}) passed to '{1}'")]
    TooManyArguments(u8, Box<str>),
    #[error("{0}")]
    Exception(Box<str>),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub struct TypeError(pub LuaType, pub LuaType);

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected {:?}, got {:?}", self.0, self.1)
    }
}

impl From<String> for LuaVal {
    fn from(val: String) -> Self {
        Self::String(val.into())
    }
}

impl From<Rc<str>> for LuaVal {
    fn from(val: Rc<str>) -> Self {
        Self::String(val)
    }
}

impl From<f64> for LuaVal {
    fn from(val: f64) -> Self {
        Self::Number(val)
    }
}

impl From<bool> for LuaVal {
    fn from(val: bool) -> Self {
        Self::Bool(val)
    }
}

impl From<()> for LuaVal {
    fn from((): ()) -> Self {
        Self::Nil
    }
}

impl TryInto<f64> for LuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Self::Number(n) => Ok(n),
            v => Err(TypeError(LuaType::Number, v.get_type())),
        }
    }
}

impl TryInto<bool> for LuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<bool, Self::Error> {
        match self {
            Self::Bool(b) => Ok(b),
            v => Err(TypeError(LuaType::Bool, v.get_type())),
        }
    }
}

impl TryInto<Rc<str>> for LuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<Rc<str>, Self::Error> {
        match self {
            Self::String(s) => Ok(s),
            v => Err(TypeError(LuaType::String, v.get_type())),
        }
    }
}

impl From<Infallible> for EvalError {
    fn from(_: Infallible) -> Self {
        unsafe { unreachable_unchecked() }
    }
}

pub trait TryIntoOpt<T> {
    type Error;
    fn try_into_opt(self) -> Result<Option<T>, Self::Error>;
}

// Cannot implement TryInto<Option<T>> for LuaVal, since it
// conflicts with the default implementation of TryInto<Option<LuaVal>>
impl<T> TryIntoOpt<T> for LuaVal where Self: TryInto<T, Error = TypeError>{
    type Error = TypeError;

    fn try_into_opt(self) -> Result<Option<T>, Self::Error> {
        Ok(match self {
            Self::Nil => None,
            v => Some(v.try_into()?),
        })
    }
}
