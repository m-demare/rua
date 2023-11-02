pub mod native_functions;
pub mod vals;

use std::{
    hash::BuildHasherDefault,
    rc::{Rc, Weak},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::eval::vals::IntoRuaVal;
use rua_identifiers::Trie;
use rustc_hash::FxHasher;
use weak_table::{weak_key_hash_map::Entry, WeakKeyHashMap};

use crate::{
    compiler::bytecode::Instruction,
    eval::{
        native_functions::default_global,
        vals::{string::RuaString, table::Table, EvalError, RuaResult, RuaVal},
    },
    lex::tokens::TokenType,
};

use self::{call_frame::CallFrame, vals::function::Function};

mod call_frame;
mod tests;

const STACK_SIZE: usize = u8::MAX as usize;

pub struct Vm {
    stack: Vec<RuaVal>,
    frames: Vec<CallFrame>,
    global: Table,
    identifiers: Trie<TokenType>,
    strings: WeakKeyHashMap<Weak<str>, StringId, BuildHasherDefault<FxHasher>>,
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(STACK_SIZE),
            frames: Vec::new(),
            global: Table::new(),
            identifiers: Trie::new(),
            strings: WeakKeyHashMap::default(),
        };
        let mut global = default_global(&mut vm);
        global.insert(Into::<Rc<str>>::into("_G").into_rua(&mut vm), RuaVal::Table(global.clone()));
        vm.global = global;
        vm
    }

    pub fn interpret(&mut self, function: Function) -> Result<RuaVal, EvalError> {
        let mut frame = CallFrame::new(function.clone(), 0);
        #[cfg(test)]
        println!("Started tracing {function:?}");
        loop {
            #[cfg(test)]
            self.trace(&frame);
            let instr = frame.curr_instr();
            match instr {
                Instruction::Return => {
                    return Ok(self.pop());
                }
                Instruction::ReturnNil => {
                    return Ok(RuaVal::Nil);
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Constant(c) => {
                    let constant = frame.read_constant(c);
                    self.push(constant)
                }
                Instruction::Neg => {
                    self.unary_op(|v| Ok((-v.into_number()?).into()))?;
                }
                Instruction::Add => self.number_binary_op(|a, b| a + b)?,
                Instruction::Sub => self.number_binary_op(|a, b| a - b)?,
                Instruction::Mul => self.number_binary_op(|a, b| a * b)?,
                Instruction::Div => self.number_binary_op(|a, b| a / b)?,
                Instruction::Pow => self.number_binary_op(|a, b| a.powf(b))?,
                Instruction::True => self.push(true.into()),
                Instruction::False => self.push(false.into()),
                Instruction::Nil => self.push(RuaVal::Nil),
                Instruction::Not => self.unary_op(|v| Ok((!v.truthy()).into()))?,
                Instruction::Eq => self.binary_op(|a, b| Ok((a == b).into()))?,
                Instruction::Lt => self.number_binary_op(|a, b| a < b)?,
                Instruction::Gt => self.number_binary_op(|a, b| a > b)?,
                Instruction::Neq => self.binary_op(|a, b| Ok((a != b).into()))?,
                Instruction::Le => self.number_binary_op(|a, b| a <= b)?,
                Instruction::Ge => self.number_binary_op(|a, b| a >= b)?,
                Instruction::StrLen => {
                    #[allow(clippy::cast_precision_loss)]
                    self.unary_op(|v| Ok((v.into_str()?.len() as f64).into()))?;
                }
                Instruction::StrConcat => {
                    self.str_concat()?;
                }
                Instruction::SetGlobal => {
                    let val = self.peek(0);
                    let key = self.peek(1);
                    self.global.insert(key, val);
                    self.pop();
                    self.pop();
                }
                Instruction::GetGlobal => {
                    let key = self.pop();
                    self.push(self.global.get(&key).into());
                }
                Instruction::Call(nargs) => {
                    let mut args = Vec::with_capacity(nargs as usize);
                    for i in (0..nargs).rev() {
                        args.push(self.peek(i as usize))
                    }
                    let val = self.peek(nargs as usize).as_func()?.call(&args, self)?;

                    self.drop(nargs as usize + 1);
                    self.push(val);
                }
                Instruction::SetLocal(idx) => {
                    let val = self.pop();
                    self.set_stack_at(frame.stack_start() + idx as usize, val);
                }
                Instruction::GetLocal(idx) => {
                    let val = self.stack_at(frame.stack_start() + idx as usize);
                    self.push(val)
                }
                Instruction::JmpIfFalsePop(offset) => {
                    let val = self.pop();
                    if !val.truthy() {
                        frame.rel_jmp(offset as isize - 1);
                    }
                }
                Instruction::JmpIfFalse(offset) => {
                    let val = self.peek(0);
                    if !val.truthy() {
                        frame.rel_jmp(offset as isize - 1);
                    }
                }
                Instruction::JmpIfTrue(offset) => {
                    let val = self.peek(0);
                    if val.truthy() {
                        frame.rel_jmp(offset as isize - 1);
                    }
                }
                Instruction::Jmp(offset) => {
                    frame.rel_jmp(offset as isize - 1);
                }
            }
        }
    }

    fn unary_op<F: Fn(RuaVal) -> RuaResult>(&mut self, f: F) -> Result<(), EvalError> {
        let a = self.pop();
        self.push(f(a)?);
        Ok(())
    }

    fn binary_op<F: Fn(RuaVal, RuaVal) -> RuaResult>(&mut self, f: F) -> Result<(), EvalError> {
        let b = self.pop();
        let a = self.pop();
        self.push(f(a, b)?);
        Ok(())
    }

    fn number_binary_op<T: Into<RuaVal>, F: Fn(f64, f64) -> T>(
        &mut self,
        f: F,
    ) -> Result<(), EvalError> {
        self.binary_op(|a, b| Ok(f(a.into_number()?, b.into_number()?).into()))
    }

    fn str_concat(&mut self) -> Result<(), EvalError> {
        let b = self.pop();
        let a = self.pop();
        let res = (a.into_str()?.to_string() + &b.into_str()?).into_rua(self);
        self.push(res);
        Ok(())
    }

    fn push(&mut self, val: RuaVal) {
        self.stack.push(val)
    }

    fn pop(&mut self) -> RuaVal {
        self.stack.pop().unwrap()
    }

    fn peek(&self, back: usize) -> RuaVal {
        self.stack[self.stack.len() - 1 - back].clone()
    }

    fn drop(&mut self, n: usize) {
        debug_assert!(self.stack.len() >= n);
        self.stack.truncate(self.stack.len() - n)
    }

    fn stack_at(&self, idx: usize) -> RuaVal {
        self.stack[idx].clone()
    }

    fn set_stack_at(&mut self, idx: usize, val: RuaVal) {
        self.stack[idx] = val
    }

    pub fn identifiers(&mut self) -> &mut Trie<TokenType> {
        &mut self.identifiers
    }

    pub fn new_string(&mut self, s: Rc<str>) -> RuaString {
        let string_id = match self.strings.entry(s.clone()) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(e) => *e.insert({
                static COUNTER: AtomicUsize = AtomicUsize::new(0);
                StringId(COUNTER.fetch_add(1, Ordering::Relaxed))
            }),
        };
        RuaString::new(s, string_id)
    }

    #[cfg(test)] // TODO add cfg for tracing
    fn trace(&self, frame: &CallFrame) {
        for el in &self.stack {
            println!("[ {el} ]");
        }
        frame.print_curr_instr();
    }

    #[cfg(test)]
    fn stack(&self) -> &Vec<RuaVal> {
        &self.stack
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StringId(usize);
