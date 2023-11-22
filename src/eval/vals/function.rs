#![allow(clippy::module_name_repetitions)]

use std::fmt::Debug;
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::{compiler::bytecode::Chunk, eval::Vm};

use super::string::RuaString;
use super::{RuaResult, RuaVal};

struct FunctionInner {
    chunk: Chunk,
    arity: u8,
    name: RuaString,
    upvalue_count: u8,
}

#[derive(Clone)]
pub struct Function {
    inner: Rc<FunctionInner>,
}

#[derive(Clone)]
pub struct NativeFunction {
    func: &'static dyn Fn(&mut FunctionContext) -> RuaResult,
}

pub struct FunctionContext<'vm> {
    pub args: Vec<RuaVal>,
    pub vm: &'vm mut Vm,
}

impl Function {
    pub fn new(chunk: Chunk, arity: u8, name: RuaString, upvalue_count: u8) -> Self {
        Self { inner: Rc::new(FunctionInner { chunk, arity, name, upvalue_count }) }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.inner.chunk
    }

    pub fn pretty_name(&self) -> Rc<str> {
        if self.inner.name.is_empty() {
            "<anonymous>".into()
        } else {
            String::from_utf8_lossy(&self.inner.name.inner()).into()
        }
    }

    pub fn arity(&self) -> u8 {
        self.inner.arity
    }

    pub fn upvalue_count(&self) -> u8 {
        self.inner.upvalue_count
    }
}

impl NativeFunction {
    pub fn new(func: &'static dyn Fn(&mut FunctionContext) -> RuaResult) -> Self {
        Self { func }
    }

    pub fn call(&self, args: &[RuaVal], vm: &mut Vm) -> RuaResult {
        (self.func)(&mut FunctionContext::new(args.to_vec(), vm))
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(
            (self.func as *const dyn Fn(&mut FunctionContext) -> RuaResult).cast::<u8>(),
            (other.func as *const dyn Fn(&mut FunctionContext) -> RuaResult).cast::<u8>(),
        )
    }
}

impl Eq for Function {}

impl Eq for NativeFunction {}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.inner) as usize).hash(state);
    }
}

impl Hash for NativeFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (std::ptr::addr_of!(self.func) as usize).hash(state);
    }
}

impl<'vm> FunctionContext<'vm> {
    pub fn new(args: Vec<RuaVal>, vm: &'vm mut Vm) -> Self {
        Self { args, vm }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.pretty_name();
        writeln!(f, "function {} (arity: {})", name, self.inner.arity)?;
        write!(f, "{:?}", self.inner.chunk)
    }
}
