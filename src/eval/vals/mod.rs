pub mod function;
pub mod number;
pub mod string;
pub mod table;

use std::{
    convert::Infallible,
    fmt::{self, Debug, Display},
    hint::unreachable_unchecked,
    rc::Rc,
};
use thiserror::Error;

use crate::eval::Vm;

use self::{
    function::{Function, NativeFunction},
    number::RuaNumber,
    string::RuaString,
    table::Table,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum RuaVal {
    Number(RuaNumber),
    Bool(bool),
    Nil,
    Function(Function),
    String(RuaString),
    NativeFunction(NativeFunction),
    Table(Table),
}

pub type RuaResult = Result<RuaVal, EvalError>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RuaType {
    Number,
    Bool,
    Nil,
    Function,
    String,
    Table,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtResult {
    None,
    Return(RuaVal),
    Break,
}

pub trait RuaCallable {
    fn call(&self, args: &[RuaVal], vm: &mut Vm) -> RuaResult;
}

impl RuaVal {
    pub fn into_bool(self) -> Result<bool, TypeError> {
        self.try_into()
    }

    pub fn into_number(self) -> Result<f64, TypeError> {
        self.try_into()
    }

    pub fn into_table(self) -> Result<Table, TypeError> {
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
            Self::Table(..) => RuaType::Table,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> Result<usize, TypeError> {
        match self {
            Self::String(s) => Ok(s.len()),
            Self::Table(t) => Ok(t.arr_size()),
            v => Err(TypeError(v.get_type(), RuaType::Table)),
        }
    }
}

impl Display for RuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n.val()),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Nil => write!(f, "nil"),
            Self::Function(..) | Self::NativeFunction(..) => write!(f, "function"),
            Self::String(s) => write!(f, "{s}"),
            Self::Table(t) => write!(f, "table: 0x{:x}", t.addr()),
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
            Self::Table => write!(f, "table"),
        }
    }
}

impl Debug for RuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum EvalError {
    #[error("TypeError: {0}")]
    TypeError(#[from] TypeError),
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

pub trait IntoRuaVal {
    fn into_rua(self, vm: &mut Vm) -> RuaVal;
}

impl<T: Into<RuaVal>> IntoRuaVal for T {
    fn into_rua(self, _: &mut Vm) -> RuaVal {
        self.into()
    }
}

impl IntoRuaVal for Rc<str> {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        RuaVal::String(vm.new_string(self))
    }
}

impl IntoRuaVal for &str {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        RuaVal::String(vm.new_string(self.into()))
    }
}

impl IntoRuaVal for String {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        RuaVal::String(vm.new_string(self.into()))
    }
}

// TODO convert these into IntoRuaVal
impl From<Table> for RuaVal {
    fn from(val: Table) -> Self {
        Self::Table(val)
    }
}

impl From<f64> for RuaVal {
    fn from(val: f64) -> Self {
        Self::Number(RuaNumber::new(val))
    }
}

impl From<bool> for RuaVal {
    fn from(val: bool) -> Self {
        Self::Bool(val)
    }
}

impl From<RuaString> for RuaVal {
    fn from(val: RuaString) -> Self {
        Self::String(val)
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
            Self::Number(n) => Ok(n.val()),
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
            Self::String(s) => Ok(s.inner()),
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

impl TryInto<Table> for RuaVal {
    type Error = TypeError;

    fn try_into(self) -> Result<Table, Self::Error> {
        match self {
            Self::Table(t) => Ok(t),
            v => Err(TypeError(RuaType::Table, v.get_type())),
        }
    }
}

impl From<Infallible> for EvalError {
    fn from(_: Infallible) -> Self {
        // SAFETY: Infallible cannot be instantiated
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

impl From<Option<Self>> for RuaVal {
    fn from(val: Option<Self>) -> Self {
        match val {
            Some(v) => v,
            None => Self::Nil,
        }
    }
}
