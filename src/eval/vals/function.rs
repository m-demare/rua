#![allow(clippy::module_name_repetitions)]

use std::fmt::Debug;
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{compiler::bytecode::Chunk, eval::Vm};

use super::string::RuaString;
use super::{RuaResult, RuaVal};

struct FunctionInner {
    chunk: Chunk,
    arity: u8,
    name: RuaString,
    id: usize,
}

#[derive(Clone)]
pub struct Function {
    inner: Rc<FunctionInner>,
}

#[derive(Clone)]
pub struct NativeFunction {
    func: Rc<dyn Fn(&mut FunctionContext) -> RuaResult>,
    id: usize,
}

pub struct FunctionContext<'vm> {
    pub args: Vec<RuaVal>,
    pub vm: &'vm mut Vm,
}

impl Function {
    pub fn new(chunk: Chunk, arity: u8, name: RuaString) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self {
            inner: Rc::new(FunctionInner {
                chunk,
                arity,
                name,
                id: COUNTER.fetch_add(1, Ordering::Relaxed),
            }),
        }
    }

    pub fn chunk(&self) -> &Chunk {
        &self.inner.chunk
    }

    pub fn pretty_name(&self) -> Rc<str> {
        if self.inner.name.is_empty() {
            "<anonymous>".into()
        } else {
            self.inner.name.inner()
        }
    }
}

impl NativeFunction {
    pub fn new(func: Rc<dyn Fn(&mut FunctionContext) -> RuaResult>) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self { func, id: COUNTER.fetch_add(1, Ordering::Relaxed) }
    }

    pub fn call(&self, args: &[RuaVal], vm: &mut Vm) -> RuaResult {
        (self.func)(&mut FunctionContext::new(args.to_vec(), vm))
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.inner.id == other.inner.id
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Function {}

impl Eq for NativeFunction {}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.id.hash(state);
    }
}

impl Hash for NativeFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
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
