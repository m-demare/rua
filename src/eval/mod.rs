pub mod native_functions;
pub mod vals;

use std::{
    hash::BuildHasherDefault,
    rc::{Rc, Weak},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::eval::vals::{IntoRuaVal, RuaType, StackTrace};
use rua_identifiers::Trie;
use rustc_hash::FxHasher;
use weak_table::{weak_key_hash_map::Entry, WeakKeyHashMap};

use crate::{
    compiler::bytecode::Instruction,
    eval::{
        native_functions::default_global,
        vals::{string::RuaString, table::Table, EvalError, RuaVal},
    },
    lex::tokens::TokenType,
};

use self::{
    call_frame::CallFrame,
    vals::{function::Function, EvalErrorTraced, RuaResult, RuaResultUntraced},
};

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

    pub fn interpret(&mut self, function: Function) -> RuaResult {
        let first_frame_start = self.stack.len().saturating_sub(1); // Top level function isn't pushed to the stack
        #[cfg(test)]
        println!("Started tracing {function:?}, starting at stack {first_frame_start}\n\n");
        let frame = CallFrame::new(function, first_frame_start);
        let first_frame_id = frame.id();
        match self.interpret_error_boundary(frame) {
            Ok(v) => Ok(v),
            Err(mut e) => {
                for _ in self.frames.iter().rev().take_while(|frame| {
                    e.push_stack_trace(frame.func_name(), frame.curr_line());
                    frame.id() != first_frame_id
                }) {}
                self.stack.truncate(first_frame_start);
                Err(e)
            }
        }
    }

    fn interpret_error_boundary(&mut self, mut frame: CallFrame) -> RuaResult {
        macro_rules! catch {
            ($res: expr) => {
                match $res {
                    Err(e) => {
                        self.frames.push(frame);
                        return Err(EvalErrorTraced::new(e, StackTrace::new()));
                    }
                    Ok(val) => val,
                }
            };
        }
        let first_frame_id = frame.id();
        loop {
            #[cfg(test)]
            self.trace(&frame);
            let instr = frame.curr_instr();
            match instr {
                Instruction::Return => {
                    let retval = self.pop();
                    self.stack.truncate(frame.stack_start());
                    if frame.id() == first_frame_id {
                        return Ok(retval);
                    }
                    match self.frames.pop() {
                        Some(f) => {
                            self.push(retval);
                            frame = f;
                        }
                        None => return Ok(retval),
                    }
                }
                Instruction::ReturnNil => {
                    self.stack.truncate(frame.stack_start());
                    if frame.id() == first_frame_id {
                        return Ok(RuaVal::Nil);
                    }
                    match self.frames.pop() {
                        Some(f) => {
                            self.push(RuaVal::Nil);
                            frame = f;
                        }
                        None => return Ok(RuaVal::Nil),
                    }
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Constant(c) => {
                    let constant = frame.read_constant(c);
                    self.push(constant);
                }
                Instruction::Neg => {
                    catch!(self.unary_op(|v| Ok((-v.into_number()?).into())));
                }
                Instruction::Add => catch!(self.number_binary_op(|a, b| a + b)),
                Instruction::Sub => catch!(self.number_binary_op(|a, b| a - b)),
                Instruction::Mul => catch!(self.number_binary_op(|a, b| a * b)),
                Instruction::Div => catch!(self.number_binary_op(|a, b| a / b)),
                Instruction::Pow => catch!(self.number_binary_op(f64::powf)),
                Instruction::True => self.push(true.into()),
                Instruction::False => self.push(false.into()),
                Instruction::Nil => self.push(RuaVal::Nil),
                Instruction::Not => catch!(self.unary_op(|v| Ok((!v.truthy()).into()))),
                Instruction::Eq => catch!(self.binary_op(|a, b| Ok((a == b).into()))),
                Instruction::Lt => catch!(self.number_binary_op(|a, b| a < b)),
                Instruction::Gt => catch!(self.number_binary_op(|a, b| a > b)),
                Instruction::Neq => catch!(self.binary_op(|a, b| Ok((a != b).into()))),
                Instruction::Le => catch!(self.number_binary_op(|a, b| a <= b)),
                Instruction::Ge => catch!(self.number_binary_op(|a, b| a >= b)),
                Instruction::Len => catch!(self.unary_op(|v| Ok(((v.len())? as f64).into()))),
                Instruction::StrConcat => catch!(self.str_concat()),
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
                    let func = self.peek(nargs as usize);
                    match func {
                        RuaVal::Function(f) => {
                            let stack_start_pos = self.get_frame_start(nargs as usize);
                            #[cfg(test)]
                            println!("Started tracing {f:?}, starting at stack {stack_start_pos}");
                            self.frames.push(frame);
                            frame = CallFrame::new(f, stack_start_pos);
                        }
                        RuaVal::NativeFunction(f) => {
                            let args: Vec<RuaVal> = self.stack_peek_n(nargs as usize).to_vec();
                            let retval = match f.call(&args, self) {
                                Ok(v) => v,
                                Err(mut e) => {
                                    e.push_stack_trace(frame.func_name(), frame.curr_line());
                                    return Err(e);
                                }
                            };
                            self.drop(nargs as usize + 1); // drop args and function itself
                            self.push(retval);
                        }
                        v => catch!(Err::<(), EvalError>(EvalError::TypeError {
                            expected: RuaType::Function,
                            got: v.get_type()
                        })),
                    }
                }
                Instruction::SetLocal(local) => {
                    let val = self.pop();
                    self.set_stack_at(frame.stack_start() + local.pos(), val);
                }
                Instruction::GetLocal(local) => {
                    let val = self.stack_at(frame.stack_start() + local.pos());
                    self.push(val);
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
                Instruction::NewTable => {
                    self.push(RuaVal::Table(Table::new()));
                }
                Instruction::InsertKeyVal => {
                    let table = self.peek(0);
                    let val = self.peek(1);
                    let key = self.peek(2);
                    let mut table = catch!(table.into_table());
                    table.insert(key, val);
                    self.drop(3);
                    self.push(table.into());
                }
                Instruction::InsertValKey => {
                    let table = self.peek(0);
                    let key = self.peek(1);
                    let val = self.peek(2);
                    let mut table = catch!(table.into_table());
                    table.insert(key, val);
                    self.drop(3);
                    self.push(table.into());
                }
                Instruction::Index => {
                    let key = self.pop();
                    let table = self.pop();
                    let table = catch!(table.into_table());
                    self.push(table.get(&key).into());
                }
                #[cfg(debug_assertions)]
                Instruction::CheckStack(n_locals) => {
                    debug_assert_eq!(frame.stack_start() + n_locals as usize, self.stack.len());
                }
            }
        }
    }

    fn stack_peek_n(&mut self, nargs: usize) -> &[RuaVal] {
        &self.stack[self.stack.len() - nargs..self.stack.len()]
    }

    fn unary_op<F: Fn(RuaVal) -> RuaResultUntraced>(&mut self, f: F) -> Result<(), EvalError> {
        let a = self.pop();
        self.push(f(a)?);
        Ok(())
    }

    fn binary_op<F: Fn(RuaVal, RuaVal) -> RuaResultUntraced>(
        &mut self,
        f: F,
    ) -> Result<(), EvalError> {
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
        self.stack.push(val);
    }

    fn pop(&mut self) -> RuaVal {
        self.stack.pop().unwrap()
    }

    fn peek(&self, back: usize) -> RuaVal {
        self.stack[self.stack.len() - 1 - back].clone()
    }

    fn drop(&mut self, n: usize) {
        debug_assert!(self.stack.len() >= n);
        self.stack.truncate(self.stack.len() - n);
    }

    fn stack_at(&self, idx: usize) -> RuaVal {
        self.stack[idx].clone()
    }

    fn set_stack_at(&mut self, idx: usize, val: RuaVal) {
        self.stack[idx] = val;
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
            println!("[ {el:?} ]");
        }
        frame.print_curr_instr();
    }

    #[cfg(test)]
    const fn stack(&self) -> &Vec<RuaVal> {
        &self.stack
    }

    fn get_frame_start(&self, nargs: usize) -> usize {
        self.stack.len() - nargs - 1
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StringId(usize);
