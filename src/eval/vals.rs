pub mod closure;
pub mod function;
pub mod number;
pub mod string;
pub mod table;

use either::Either;
use std::{
    cell::RefCell,
    convert::Infallible,
    fmt::{self, Debug, Display},
    hint::unreachable_unchecked,
    rc::Rc,
};
use thiserror::Error;

use crate::eval::Vm;

use self::{
    closure::Closure, function::NativeFunction, number::RuaNumber, string::RuaString, table::Table,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum RuaVal {
    Number(RuaNumber),
    Bool(bool),
    Nil,
    Closure(Rc<Closure>),
    String(RuaString),
    NativeFunction(NativeFunction),
    Table(Table),
}

pub type UpvalueObj = Rc<RefCell<Either<usize, RuaVal>>>;

pub type RuaResult = Result<RuaVal, EvalErrorTraced>;
pub type RuaResultUntraced = Result<RuaVal, EvalError>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RuaType {
    Number,
    Bool,
    Nil,
    Function,
    String,
    Table,
}

impl RuaVal {
    pub fn into_number(self) -> Result<f64, EvalError> {
        self.try_into()
    }

    pub fn into_table(self) -> Result<Table, EvalError> {
        self.try_into()
    }

    pub fn into_str(self) -> Result<Rc<str>, EvalError> {
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
            Self::NativeFunction(..) | Self::Closure(..) => RuaType::Function,
            Self::String(..) => RuaType::String,
            Self::Table(..) => RuaType::Table,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> Result<usize, EvalError> {
        match self {
            Self::String(s) => Ok(s.len()),
            Self::Table(t) => Ok(t.arr_size()),
            v => Err(EvalError::TypeError { expected: v.get_type(), got: RuaType::Table }),
        }
    }
}

impl Display for RuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n.val()),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Nil => write!(f, "nil"),
            Self::NativeFunction(..) | Self::Closure(..) => {
                write!(f, "function")
            }
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
        match self {
            Self::Closure(closure) => write!(f, "function ({})", closure.function().pretty_name()),
            Self::NativeFunction(_) => write!(f, "native function"),
            _ => write!(f, "{self}"),
        }
    }
}

pub type StackTrace = Vec<(Rc<str>, usize)>;

#[derive(Error, PartialEq, Eq)]
pub struct EvalErrorTraced(Box<(EvalError, StackTrace)>);

impl EvalErrorTraced {
    pub fn new(e: EvalError, stack_trace: StackTrace) -> Self {
        Self(Box::new((e, stack_trace)))
    }

    pub fn push_stack_trace(&mut self, name: Rc<str>, line: usize) {
        self.0 .1.push((name, line));
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum EvalError {
    #[error("TypeError: expected {expected}, got {got}")]
    TypeError { expected: RuaType, got: RuaType },
    #[error("bad argument #{0} to '{1}' (value expected)")]
    ExpectedArgument(u8, Box<str>),
    #[error("Too many arguments ({0}) passed to '{1}'")]
    TooManyArguments(u8, Box<str>),
    #[error("{0}")]
    Exception(Box<str>),
    #[error("Assertion failed. Error: {0:?}")]
    AssertionFailed(Option<RuaVal>),
}

impl Debug for EvalErrorTraced {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Display for EvalErrorTraced {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "error: {}", self.0 .0)?;
        for (func, line) in &self.0 .1 {
            writeln!(f, "{func} at {line}")?;
        }
        Ok(())
    }
}

impl From<EvalError> for EvalErrorTraced {
    fn from(err: EvalError) -> Self {
        Self::new(err, StackTrace::new())
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
    type Error = EvalError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Self::Number(n) => Ok(n.val()),
            v => Err(EvalError::TypeError { expected: RuaType::Number, got: v.get_type() }),
        }
    }
}

impl TryInto<bool> for RuaVal {
    type Error = EvalError;

    fn try_into(self) -> Result<bool, Self::Error> {
        match self {
            Self::Bool(b) => Ok(b),
            v => Err(EvalError::TypeError { expected: RuaType::Bool, got: v.get_type() }),
        }
    }
}

impl TryInto<Rc<str>> for RuaVal {
    type Error = EvalError;

    fn try_into(self) -> Result<Rc<str>, Self::Error> {
        match self {
            Self::String(s) => Ok(s.inner()),
            v => Err(EvalError::TypeError { expected: RuaType::String, got: v.get_type() }),
        }
    }
}

impl TryInto<Table> for RuaVal {
    type Error = EvalError;

    fn try_into(self) -> Result<Table, Self::Error> {
        match self {
            Self::Table(t) => Ok(t),
            v => Err(EvalError::TypeError { expected: RuaType::Table, got: v.get_type() }),
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
    Self: TryInto<T, Error = EvalError>,
{
    type Error = EvalError;

    fn try_into_opt(self) -> Result<Option<T>, Self::Error> {
        Ok(match self {
            Self::Nil => None,
            v => Some(v.try_into()?),
        })
    }
}

impl TryIntoOpt<Self> for RuaVal {
    type Error = EvalError;

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
