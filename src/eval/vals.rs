use std::{fmt::{self, Display, Debug}, rc::Rc, cell::RefCell};
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
}

#[derive(Clone)]
pub struct Function {
    args: Rc<[FunctionArg]>,
    body: Rc<[Statement]>,
    env: Rc<RefCell<Scope>>,
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

impl LuaVal {
    pub const fn as_bool(&self) -> Result<bool, TypeError>{
        match self {
            Self::Bool(b) => Ok(*b),
            v => Err(TypeError(LuaType::Number, v.get_type())),
        }
    } 

    pub const fn as_number(&self) -> Result<f64, TypeError>{
        match self {
            Self::Number(n) => Ok(*n),
            v => Err(TypeError(LuaType::Number, v.get_type())),
        }
    }

    pub fn as_func(&self) -> Result<Function, TypeError> {
        match self {
            Self::Function(f) => Ok(f.clone()),
            v => Err(TypeError(LuaType::Function, v.get_type())),
        }
    }

    pub fn as_str(&self) -> Result<Rc<str>, TypeError> {
        match self {
            Self::String(s) => Ok(s.clone()),
            v => Err(TypeError(LuaType::String, v.get_type())),
        }
    }

    pub const fn truthy(&self) -> bool {
        !matches!(self, Self::Bool(false) | Self::Nil)
    }

    const fn get_type(&self) -> LuaType {
        match self {
            Self::Number(..) => LuaType::Number,
            Self::Bool(..) => LuaType::Bool,
            Self::Nil => LuaType::Nil,
            Self::Function(..) => LuaType::Function,
            Self::String(..) => LuaType::String,
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Display for LuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Nil => write!(f, "nil"),
            Self::Function(..) => write!(f, "function"),
            Self::String(s) => write!(f, "{s}"),
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

    pub fn call(&self, args: &[Expression], args_env: &Rc<RefCell<Scope>>) -> Result<LuaVal, EvalError> {
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

#[derive(Error, Debug, PartialEq, Eq)]
pub enum EvalError {
    #[error("TypeError: {0}")]
    TypeError(#[from] TypeError),
    #[error("Unknown identifier '{0}'")]
    UnknownId(Box<str>),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub struct TypeError(pub LuaType, pub LuaType);

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected {:?}, got {:?}", self.0, self.1)
    }
}

