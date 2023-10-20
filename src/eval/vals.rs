use std::{
    cell::RefCell,
    convert::Infallible,
    fmt::{self, Debug, Display},
    hint::unreachable_unchecked,
    rc::Rc,
};
use thiserror::Error;

use crate::parser::ast::{FunctionArg, Statement};

use super::{scope::Scope, statements::eval_block};

#[derive(Clone, PartialEq)]
pub enum RuaVal {
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

pub type RuaResult = Result<RuaVal, EvalError>;

#[derive(Clone)]
pub struct NativeFunction {
    func: Rc<dyn Fn(&FunctionContext) -> RuaResult>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RuaType {
    Number,
    Bool,
    Nil,
    Function,
    String,
}

#[derive(Debug, PartialEq)]
pub enum StmtResult {
    None,
    Return(RuaVal),
    Break,
}

pub trait RuaCallable {
    fn call(&self, args: &[RuaVal]) -> RuaResult;
}

pub struct FunctionContext {
    pub args: Vec<RuaVal>,
}

impl FunctionContext {
    pub fn new(args: Vec<RuaVal>) -> Self {
        Self { args }
    }
}

impl RuaVal {
    pub fn into_bool(self) -> Result<bool, TypeError> {
        self.try_into()
    }

    pub fn into_number(self) -> Result<f64, TypeError> {
        self.try_into()
    }

    pub fn as_func(&self) -> Result<&dyn RuaCallable, TypeError> {
        match self {
            Self::Function(f) => Ok(f),
            Self::NativeFunction(f) => Ok(f),
            v => Err(TypeError(RuaType::Function, v.get_type())),
        }
    }

    pub fn into_str(self) -> Result<Rc<str>, TypeError> {
        self.try_into()
    }

    pub const fn truthy(&self) -> bool {
        !matches!(self, Self::Bool(false) | Self::Nil)
    }

    pub const fn get_type(&self) -> RuaType {
        match self {
            Self::Number(..) => RuaType::Number,
            Self::Bool(..) => RuaType::Bool,
            Self::Nil => RuaType::Nil,
            Self::Function(..) | Self::NativeFunction(..) => RuaType::Function,
            Self::String(..) => RuaType::String,
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
    pub fn new(func: Rc<dyn Fn(&FunctionContext) -> RuaResult>) -> Self {
        Self { func }
    }
}

impl Display for RuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Nil => write!(f, "nil"),
            Self::Function(..) | Self::NativeFunction(..) => write!(f, "function"),
            Self::String(s) => write!(f, "{s}"),
        }
    }
}

impl Display for RuaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number => write!(f, "number"),
            Self::Bool => write!(f, "boolean"),
            Self::Nil => write!(f, "nil"),
            Self::Function => write!(f, "function"),
            Self::String => write!(f, "string"),
        }
    }
}

impl Debug for RuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Function {
    pub fn new(args: Rc<[FunctionArg]>, body: Rc<[Statement]>, env: Rc<RefCell<Scope>>) -> Self {
        Self { args, body, env }
    }
}

impl RuaCallable for Function {
    fn call(&self, args: &[RuaVal]) -> RuaResult {
        let mut new_env = Scope::extend(self.env.clone());
        self.args.iter().zip(args).for_each(|(arg, val)| match arg {
            FunctionArg::Identifier(id) => new_env.set(*id, val.clone()),
            FunctionArg::Dotdotdot => todo!(),
        });

        match eval_block(&self.body, &RefCell::new(new_env).into())? {
            StmtResult::None => Ok(RuaVal::Nil),
            StmtResult::Return(v) => Ok(v),
            StmtResult::Break => todo!(),
        }
    }
}

impl RuaCallable for NativeFunction {
    fn call(&self, args: &[RuaVal]) -> RuaResult {
        (self.func)(&FunctionContext::new(args.to_vec()))
    }
}

#[derive(Error, Debug, PartialEq)]
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
    #[error("Assertion failed. Error: {0:?}")]
    AssertionFailed(Option<RuaVal>),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub struct TypeError(pub RuaType, pub RuaType);

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected {:?}, got {:?}", self.0, self.1)
    }
}

impl From<String> for RuaVal {
    fn from(val: String) -> Self {
        Self::String(val.into())
    }
}

impl From<Rc<str>> for RuaVal {
    fn from(val: Rc<str>) -> Self {
        Self::String(val)
    }
}

impl From<f64> for RuaVal {
    fn from(val: f64) -> Self {
        Self::Number(val)
    }
}

impl From<bool> for RuaVal {
    fn from(val: bool) -> Self {
        Self::Bool(val)
    }
}

impl From<()> for RuaVal {
    fn from((): ()) -> Self {
        Self::Nil
    }
}

impl TryInto<f64> for RuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Self::Number(n) => Ok(n),
            v => Err(TypeError(RuaType::Number, v.get_type())),
        }
    }
}

impl TryInto<bool> for RuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<bool, Self::Error> {
        match self {
            Self::Bool(b) => Ok(b),
            v => Err(TypeError(RuaType::Bool, v.get_type())),
        }
    }
}

impl TryInto<Rc<str>> for RuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<Rc<str>, Self::Error> {
        match self {
            Self::String(s) => Ok(s),
            v => Err(TypeError(RuaType::String, v.get_type())),
        }
    }
}

impl TryInto<Box<dyn RuaCallable>> for RuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<Box<dyn RuaCallable>, Self::Error> {
        match self {
            Self::Function(f) => Ok(Box::new(f)),
            Self::NativeFunction(f) => Ok(Box::new(f)),
            v => Err(TypeError(RuaType::Function, v.get_type())),
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

// Cannot implement TryInto<Option<T>> for RuaVal, since it
// conflicts with the default implementation of TryInto<Option<RuaVal>>
impl<T> TryIntoOpt<T> for RuaVal
where
    Self: TryInto<T, Error = TypeError>,
{
    type Error = TypeError;

    fn try_into_opt(self) -> Result<Option<T>, Self::Error> {
        Ok(match self {
            Self::Nil => None,
            v => Some(v.try_into()?),
        })
    }
}

impl TryIntoOpt<Self> for RuaVal {
    type Error = TypeError;

    fn try_into_opt(self) -> Result<Option<Self>, Self::Error> {
        Ok(Some(self))
    }
}
