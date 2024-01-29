pub mod closure;
pub(crate) mod function;
pub mod number;
pub mod string;
pub mod table;

use nohash_hasher::IsEnabled;
use std::{
    cell::RefCell,
    convert::Infallible,
    fmt::{self, Debug, Display},
    hash::{Hash, Hasher},
    hint::unreachable_unchecked,
    num::NonZeroU32,
    rc::Rc,
};
use thiserror::Error;

use crate::eval::Vm;

use self::{
    closure::Closure, function::NativeFunction, number::RuaNumber, string::RuaString, table::Table,
};

use super::GcData;

#[derive(Clone, PartialEq, Eq)]
enum RuaValInner {
    Number(RuaNumber),
    Bool(bool),
    Nil,
    Closure(Rc<Closure>),
    String(RuaString),
    NativeFunction(Rc<NativeFunction>),
    Table(Rc<Table>),
}

impl std::hash::Hash for RuaValInner {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut ahash_state = ahash::AHasher::default();
        // Have to do this NoHashHasher shenanigans to avoid rehashing
        // the strings' hash every time, which is unnecessarily costly
        match self {
            Self::String(s) => return s.hash(state),
            Self::Number(v) => v.hash(&mut ahash_state),
            Self::Bool(v) => v.hash(&mut ahash_state),
            Self::Nil => 123.hash(&mut ahash_state),
            Self::Closure(v) => v.hash(&mut ahash_state),
            Self::NativeFunction(v) => v.hash(&mut ahash_state),
            Self::Table(v) => v.hash(&mut ahash_state),
        }
        state.write_u64(ahash_state.finish());
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RuaVal(RuaValInner);

impl IsEnabled for RuaVal {}

#[cfg(target_arch = "x86_64")]
static_assertions::assert_eq_size!(RuaVal, [u8; 16]);

#[derive(Debug)]
pub enum Upvalue {
    Open(usize),
    Closed(RuaVal),
}

pub type UpvalueObj = Rc<RefCell<Upvalue>>;

pub type RuaResultTraced = Result<RuaVal, EvalErrorTraced>;
pub type RuaResult = Result<RuaVal, EvalError>;

pub enum Callable {
    Closure(Rc<Closure>),
    Native(Rc<NativeFunction>),
}

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
    /// # Errors
    ///
    /// Returns `TypeError` if value is not a `Number`
    #[inline]
    pub const fn as_number(&self) -> Result<f64, EvalError> {
        match &self.0 {
            RuaValInner::Number(n) => Ok(n.val()),
            _ => Err(self.type_error(RuaType::Number)),
        }
    }

    /// # Errors
    ///
    /// Returns `TypeError` if value is not a `Table`
    pub const fn as_table(&self) -> Result<&Rc<Table>, EvalError> {
        match &self.0 {
            RuaValInner::Table(t) => Ok(t),
            _ => Err(self.type_error(RuaType::Table)),
        }
    }

    /// # Errors
    ///
    /// Returns `TypeError` if value is not a `String`
    pub fn as_str(&self) -> Result<Rc<[u8]>, EvalError> {
        match &self.0 {
            RuaValInner::String(s) => Ok(s.inner()),
            _ => Err(self.type_error(RuaType::String)),
        }
    }

    #[must_use]
    const fn type_error(&self, expected: RuaType) -> EvalError {
        EvalError::TypeError { expected, got: self.get_type() }
    }

    #[must_use]
    pub const fn truthy(&self) -> bool {
        !matches!(self.0, RuaValInner::Bool(false) | RuaValInner::Nil)
    }

    #[must_use]
    pub const fn is_nil(&self) -> bool {
        matches!(self.0, RuaValInner::Nil)
    }

    #[must_use]
    pub const fn get_type(&self) -> RuaType {
        self.0.get_type()
    }

    /// # Errors
    ///
    /// Returns `TypeError` if value is not a `String` or `Table`
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> Result<usize, EvalError> {
        match &self.0 {
            RuaValInner::String(s) => Ok(s.len()),
            RuaValInner::Table(t) => Ok(t.length()),
            _ => Err(self.type_error(RuaType::Table)),
        }
    }

    pub(super) fn mark(&self, gc_data: &mut GcData) {
        let add_grey = match &self.0 {
            RuaValInner::Closure(c) => c.mark(),
            RuaValInner::Table(t) => t.mark(),
            _ => return,
        };
        if add_grey {
            gc_data.add_grey(self.clone());
        }
    }

    pub(super) fn blacken(&self, gc_data: &mut GcData) {
        match &self.0 {
            RuaValInner::Closure(c) => c.blacken(gc_data),
            RuaValInner::Table(t) => t.blacken(gc_data),
            _ => (),
        };
    }

    #[must_use]
    #[inline]
    pub const fn nil() -> Self {
        Self(RuaValInner::Nil)
    }

    /// # Errors
    ///
    /// Returns `TypeError` if value is not a `Closure`
    pub fn into_closure(self) -> Result<Rc<Closure>, EvalError> {
        self.try_into()
    }

    /// # Errors
    ///
    /// Returns `TypeError` if value is not a `NativeFunction`
    pub fn into_native_fn(self) -> Result<Rc<NativeFunction>, EvalError> {
        self.try_into()
    }

    /// # Errors
    ///
    /// Returns `TypeError` if value is not a `Closure` or `NativeFunction`
    pub fn into_callable(self) -> Result<Callable, EvalError> {
        match self.0 {
            RuaValInner::Closure(c) => Ok(Callable::Closure(c)),
            RuaValInner::NativeFunction(f) => Ok(Callable::Native(f)),
            _ => Err(self.type_error(RuaType::Function)),
        }
    }

    /// # Safety
    /// An unregistered table will not be garbage collected
    pub fn from_table_unregistered(table: Rc<Table>, vm_id: NonZeroU32) -> Self {
        table.register_in(vm_id);
        Self(RuaValInner::Table(table))
    }

    /// # Safety
    /// An unregistered closure will not be garbage collected
    pub fn from_closure_unregistered(closure: Rc<Closure>, vm_id: NonZeroU32) -> Self {
        closure.register_in(vm_id);
        Self(RuaValInner::Closure(closure))
    }
}

impl Default for RuaVal {
    fn default() -> Self {
        Self(RuaValInner::Nil)
    }
}

impl RuaValInner {
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
}

impl Display for RuaValInner {
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

impl Debug for RuaValInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Closure(closure) => write!(f, "function ({})", closure.function().pretty_name()),
            Self::NativeFunction(_) => write!(f, "native function"),
            Self::String(s) => write!(f, "\"{s}\""),
            _ => write!(f, "{self}"),
        }
    }
}

impl Debug for RuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Display for RuaVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type StackTrace = Vec<(Rc<str>, usize)>;

#[derive(Error, PartialEq, Eq)]
pub struct EvalErrorTraced(Box<(EvalError, StackTrace)>);

impl EvalErrorTraced {
    pub(crate) fn new(e: EvalError, stack_trace: StackTrace) -> Self {
        Self(Box::new((e, stack_trace)))
    }

    pub(crate) fn push_stack_trace(&mut self, name: Rc<str>, line: usize) {
        self.0 .1.push((name, line));
    }

    pub(crate) fn stack_trace(&mut self) -> &mut StackTrace {
        &mut self.0 .1
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
    #[error("table index is {}", if *.0 {"nil"} else {"NaN"})]
    InvalidTableIndex(bool),
    #[error("Stack overflow")]
    StackOverflow,
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
        RuaVal(RuaValInner::String(vm.new_string(self.into())))
    }
}

impl IntoRuaVal for Rc<[u8]> {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        RuaVal(RuaValInner::String(vm.new_string(self)))
    }
}

impl IntoRuaVal for &[u8] {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        RuaVal(RuaValInner::String(vm.new_string(self.into())))
    }
}

impl IntoRuaVal for String {
    fn into_rua(self, vm: &mut Vm) -> RuaVal {
        RuaVal(RuaValInner::String(vm.new_string(self.as_bytes().into())))
    }
}

impl From<bool> for RuaVal {
    #[inline]
    fn from(val: bool) -> Self {
        Self(RuaValInner::Bool(val))
    }
}

impl From<RuaString> for RuaVal {
    #[inline]
    fn from(val: RuaString) -> Self {
        Self(RuaValInner::String(val))
    }
}

impl From<()> for RuaVal {
    #[inline]
    fn from((): ()) -> Self {
        Self::nil()
    }
}

impl From<Rc<NativeFunction>> for RuaVal {
    fn from(val: Rc<NativeFunction>) -> Self {
        Self(RuaValInner::NativeFunction(val))
    }
}

impl From<NativeFunction> for RuaVal {
    fn from(val: NativeFunction) -> Self {
        Self(RuaValInner::NativeFunction(val.into()))
    }
}

impl TryInto<f64> for RuaVal {
    type Error = EvalError;

    #[inline]
    fn try_into(self) -> Result<f64, Self::Error> {
        self.as_number()
    }
}

impl From<RuaVal> for bool {
    #[inline]
    fn from(val: RuaVal) -> Self {
        val.truthy()
    }
}

impl TryInto<Rc<[u8]>> for RuaVal {
    type Error = EvalError;

    fn try_into(self) -> Result<Rc<[u8]>, Self::Error> {
        match self.0 {
            RuaValInner::String(s) => Ok(s.into()),
            _ => Err(self.type_error(RuaType::String)),
        }
    }
}

impl TryInto<Rc<Table>> for RuaVal {
    type Error = EvalError;

    fn try_into(self) -> Result<Rc<Table>, Self::Error> {
        match self.0 {
            RuaValInner::Table(t) => Ok(t),
            _ => Err(self.type_error(RuaType::Table)),
        }
    }
}

impl TryInto<Rc<Closure>> for RuaVal {
    type Error = EvalError;

    fn try_into(self) -> Result<Rc<Closure>, Self::Error> {
        match self.0 {
            RuaValInner::Closure(c) => Ok(c),
            _ => Err(self.type_error(RuaType::Function)),
        }
    }
}

impl TryInto<Rc<NativeFunction>> for RuaVal {
    type Error = EvalError;

    fn try_into(self) -> Result<Rc<NativeFunction>, Self::Error> {
        match self.0 {
            RuaValInner::NativeFunction(f) => Ok(f),
            _ => Err(self.type_error(RuaType::Function)),
        }
    }
}

impl From<Infallible> for EvalError {
    fn from(_: Infallible) -> Self {
        // SAFETY: Infallible cannot be instantiated
        unsafe { unreachable_unchecked() }
    }
}

impl<T: Into<Self>> From<Option<T>> for RuaVal {
    fn from(val: Option<T>) -> Self {
        match val {
            Some(v) => v.into(),
            None => Self(RuaValInner::Nil),
        }
    }
}
