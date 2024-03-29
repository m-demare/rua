#![allow(clippy::module_name_repetitions)]

use std::fmt::Debug;
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::{compiler::bytecode::Chunk, eval::Vm};

use super::string::RuaString;
use super::{RuaResultTraced, RuaVal};

#[derive(PartialEq)]
struct FunctionInner {
    chunk: Chunk,
    arity: u8,
    name: RuaString,
    max_used_regs: u8,
    upvalue_count: u8,
}

#[derive(PartialEq, Clone)]
pub struct Function {
    inner: Rc<FunctionInner>,
}

#[derive(Clone)]
pub struct NativeFunction {
    func: &'static dyn Fn(&mut FunctionContext) -> RuaResultTraced,
}

pub struct FunctionContext<'vm> {
    args_start: usize,
    nargs: u8,
    pub vm: &'vm mut Vm,
}

impl Function {
    pub fn new(
        chunk: Chunk,
        arity: u8,
        name: RuaString,
        max_used_regs: u8,
        upvalue_count: u8,
    ) -> Self {
        Self { inner: Rc::new(FunctionInner { chunk, arity, name, max_used_regs, upvalue_count }) }
    }

    #[inline]
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

    pub fn max_used_regs(&self) -> u8 {
        self.inner.max_used_regs
    }
}

impl NativeFunction {
    pub fn new(func: &'static dyn Fn(&mut FunctionContext) -> RuaResultTraced) -> Self {
        Self { func }
    }

    pub fn call(&self, vm: &mut Vm, args_start: usize, nargs: u8) -> RuaResultTraced {
        (self.func)(&mut FunctionContext::new(vm, args_start, nargs))
    }
}

impl PartialEq for NativeFunction {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for NativeFunction {}

impl Hash for NativeFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (std::ptr::addr_of!(self.func) as usize).hash(state);
    }
}

impl<'vm> FunctionContext<'vm> {
    pub fn new(vm: &'vm mut Vm, args_start: usize, nargs: u8) -> Self {
        Self { args_start, nargs, vm }
    }

    pub fn get_arg(&self, i: usize) -> Option<&RuaVal> {
        if i < self.nargs as usize {
            Some(self.vm.stack_at_abs(self.args_start + i))
        } else {
            None
        }
    }

    pub fn args(&self) -> &[RuaVal] {
        &self.vm.stack[self.args_start..self.args_start + self.nargs as usize]
    }

    #[inline]
    pub const fn nargs(&self) -> u8 {
        self.nargs
    }

    #[inline]
    pub const fn args_start(&self) -> usize {
        self.args_start
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "function {} ({} instructions)",
            self.pretty_name(),
            self.inner.chunk.code().len()
        )?;
        writeln!(
            f,
            "{}+ params, {} upvalues, {} constants, {} functions",
            self.inner.arity,
            self.inner.upvalue_count,
            self.inner.chunk.nconstants(),
            self.inner.chunk.nfunctions()
        )?;
        write!(f, "{:?}", self.inner.chunk)
    }
}
