#![allow(clippy::module_name_repetitions)]

use std::fmt::Debug;
use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::eval::call_frame::CallFrame;
use crate::{compiler::bytecode::Chunk, eval::Vm};

use super::coroutine::Coroutine;
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

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "native function at {}", std::ptr::addr_of!(*self) as usize)
    }
}

pub enum CoroutineAction {
    None,
    Resume { coroutine: Rc<Coroutine>, args_start: usize, nargs: u8 },
    Yield { coroutine: Rc<Coroutine>, args_start: usize, nargs: u8 },
}

pub struct FunctionContext<'vm, 'cf> {
    base: u8,
    args_start: usize,
    nargs: u8,
    pub vm: &'vm mut Vm,
    coroutine_action: CoroutineAction,
    curr_ip: usize,
    pub curr_frame: &'cf mut CallFrame,
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

    pub fn call(
        &self,
        ctxt: &mut FunctionContext,
    ) -> Result<Option<RuaVal>, super::EvalErrorTraced> {
        let res = (self.func)(ctxt)?;
        match ctxt.coroutine_action() {
            CoroutineAction::None => Ok(Some(res)),
            CoroutineAction::Resume { coroutine, args_start, nargs } => {
                let coroutine = coroutine.clone();
                let ip = coroutine.resume(ctxt.vm, ctxt.curr_frame, ctxt.base, ctxt.curr_ip);
                ctxt.curr_ip = ip;
                ctxt.vm.current_coroutine = Some(coroutine);
                Ok(None)
            }
            CoroutineAction::Yield { coroutine, args_start, nargs } => todo!(),
        }
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

impl<'vm, 'cf> FunctionContext<'vm, 'cf> {
    pub fn new(
        vm: &'vm mut Vm,
        base: u8,
        args_start: usize,
        nargs: u8,
        curr_ip: usize,
        curr_frame: &'cf mut CallFrame,
    ) -> Self {
        Self {
            base,
            args_start,
            nargs,
            vm,
            coroutine_action: CoroutineAction::None,
            curr_ip,
            curr_frame,
        }
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

    pub const fn coroutine_action(&self) -> &CoroutineAction {
        &self.coroutine_action
    }

    pub fn set_coroutine_action(&mut self, coroutine_action: CoroutineAction) {
        self.coroutine_action = coroutine_action;
    }

    pub fn curr_ip(&self) -> usize {
        self.curr_ip
    }

    pub fn base(&self) -> u8 {
        self.base
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
