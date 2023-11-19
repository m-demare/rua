pub mod native_functions;
pub mod vals;

use std::{
    cell::RefCell,
    hash::BuildHasherDefault,
    rc::{Rc, Weak},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    compiler::{bytecode::FnHandle, locals::LocalHandle, upvalues::UpvalueHandle},
    eval::vals::{closure::Closure, IntoRuaVal, RuaType},
};
use either::Either::{self, Left, Right};
use rua_trie::Trie;
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
    vals::{EvalErrorTraced, RuaResult, RuaResultUntraced, UpvalueObj},
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
    open_upvalues: Vec<UpvalueObj>,
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(STACK_SIZE),
            frames: Vec::new(),
            global: Table::new(),
            identifiers: Trie::new(),
            strings: WeakKeyHashMap::default(),
            open_upvalues: Vec::new(),
        };
        let mut global = default_global(&mut vm);
        global.insert(Into::<Rc<str>>::into("_G").into_rua(&mut vm), RuaVal::Table(global.clone()));
        vm.global = global;
        vm
    }

    pub fn interpret(&mut self, closure: Rc<Closure>) -> RuaResult {
        let first_frame_start = self.stack.len().saturating_sub(1); // Top level function isn't pushed to the stack
        #[cfg(test)]
        println!(
            "Started tracing {:?}, starting at stack {first_frame_start}\n\n",
            closure.function()
        );
        let frame = CallFrame::new(closure, first_frame_start);
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

    #[allow(clippy::too_many_lines)]
    fn interpret_error_boundary(&mut self, mut frame: CallFrame) -> RuaResult {
        let first_frame_id = frame.id();
        loop {
            #[cfg(test)]
            self.trace(&frame);
            let instr = frame.curr_instr();
            match instr {
                Instruction::Return => {
                    if let Some(val) = self.return_op(&mut frame, false, first_frame_id) {
                        return Ok(val);
                    }
                }
                Instruction::ReturnNil => {
                    if let Some(val) = self.return_op(&mut frame, true, first_frame_id) {
                        return Ok(val);
                    }
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Number(c) => {
                    let constant = frame.read_number(c);
                    self.push(constant.into());
                }
                Instruction::String(c) => {
                    let constant = frame.read_string(c);
                    self.push(constant.into());
                }
                Instruction::True => self.push(true.into()),
                Instruction::False => self.push(false.into()),
                Instruction::Nil => self.push(RuaVal::Nil),
                Instruction::Neg => {
                    trace_err(self.unary_op(|v| Ok((-v.into_number()?).into())), &frame)?;
                }
                Instruction::Not => trace_err(self.unary_op(|v| Ok((!v.truthy()).into())), &frame)?,
                Instruction::Len => trace_err(self.len_op(), &frame)?,
                Instruction::Add => trace_err(self.number_binary_op(|a, b| a + b), &frame)?,
                Instruction::Sub => trace_err(self.number_binary_op(|a, b| a - b), &frame)?,
                Instruction::Mul => trace_err(self.number_binary_op(|a, b| a * b), &frame)?,
                Instruction::Div => trace_err(self.number_binary_op(|a, b| a / b), &frame)?,
                Instruction::Mod => trace_err(self.number_binary_op(|a, b| a % b), &frame)?,
                Instruction::Pow => trace_err(self.number_binary_op(f64::powf), &frame)?,
                Instruction::Eq => trace_err(self.binary_op(|a, b| Ok((a == b).into())), &frame)?,
                Instruction::Lt => trace_err(self.number_binary_op(|a, b| a < b), &frame)?,
                Instruction::Gt => trace_err(self.number_binary_op(|a, b| a > b), &frame)?,
                Instruction::Neq => trace_err(self.binary_op(|a, b| Ok((a != b).into())), &frame)?,
                Instruction::Le => trace_err(self.number_binary_op(|a, b| a <= b), &frame)?,
                Instruction::Ge => trace_err(self.number_binary_op(|a, b| a >= b), &frame)?,
                Instruction::StrConcat => trace_err(self.str_concat(), &frame)?,
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
                Instruction::Call(nargs) => self.call(nargs, &mut frame)?,
                Instruction::SetLocal(local) => self.set_local(&frame, local),
                Instruction::GetLocal(local) => {
                    let val = self.stack_at(frame.stack_start() + local.pos());
                    self.push(val);
                }
                Instruction::JmpIfFalsePop(offset) => {
                    let val = self.pop();
                    if !val.truthy() {
                        frame.rel_jmp(offset - 1);
                    }
                }
                Instruction::JmpIfFalse(offset) => {
                    let val = self.peek(0);
                    if !val.truthy() {
                        frame.rel_jmp(offset - 1);
                    }
                }
                Instruction::JmpIfTrue(offset) => {
                    let val = self.peek(0);
                    if val.truthy() {
                        frame.rel_jmp(offset - 1);
                    }
                }
                Instruction::Jmp(offset) => frame.rel_jmp(offset - 1),
                Instruction::Loop(offset) => frame.rel_loop(offset + 1),
                Instruction::NewTable => {
                    self.push(RuaVal::Table(Table::new()));
                }
                Instruction::InsertKeyVal => {
                    let table = self.peek(0);
                    let val = self.peek(1);
                    let key = self.peek(2);
                    let mut table = trace_err(table.into_table(), &frame)?;
                    table.insert(key, val);
                    self.drop(3);
                    self.push(table.into());
                }
                Instruction::InsertValKey => {
                    let table = self.peek(0);
                    let key = self.peek(1);
                    let val = self.peek(2);
                    let mut table = trace_err(table.into_table(), &frame)?;
                    table.insert(key, val);
                    self.drop(3);
                    self.push(table.into());
                }
                Instruction::Index => {
                    let key = self.pop();
                    let table = self.pop();
                    let table = trace_err(table.into_table(), &frame)?;
                    self.push(table.get(&key).into());
                }
                Instruction::Closure(c) => {
                    self.closure(&mut frame, c);
                }
                Instruction::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop();
                }
                Instruction::GetUpvalue(up) => {
                    let val = frame.closure().get_upvalue_val(self, up);
                    self.push(val);
                }
                Instruction::SetUpvalue(up) => {
                    self.set_upvalue(&mut frame, up);
                }
                Instruction::Upvalue(_) => {
                    unreachable!("Instruction::Upvalue must be handled by Instruction::Closure")
                }
                Instruction::Multiassign(n) => {
                    self.multiassign(n, &mut frame)?;
                }
                #[cfg(debug_assertions)]
                Instruction::CheckStack(n_locals) => {
                    debug_assert_eq!(
                        frame.stack_start() + n_locals as usize,
                        self.stack.len(),
                        "Stack invariant broken: statement had non-zero effect on stack. Expected size: {}",
                        frame.stack_start() + n_locals as usize);
                }
            }
        }
    }

    fn call(&mut self, nargs: u8, frame: &mut CallFrame) -> Result<(), EvalErrorTraced> {
        let func = self.peek(nargs as usize);
        match func {
            RuaVal::Closure(closure) => {
                if closure.function().arity() < nargs {
                    self.drop((nargs - closure.function().arity()) as usize);
                }
                for _ in 0..closure.function().arity().saturating_sub(nargs) {
                    self.push(RuaVal::Nil);
                }
                let stack_start_pos = self.get_frame_start(closure.function().arity() as usize);
                #[cfg(test)]
                println!(
                    "Started tracing {:?}, starting at stack {stack_start_pos}",
                    closure.function()
                );
                let old_frame = std::mem::replace(frame, CallFrame::new(closure, stack_start_pos));
                self.frames.push(old_frame);
                Ok(())
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
                Ok(())
            }
            v => {
                let stack_trace = vec![(frame.func_name(), frame.curr_line())];
                Err(EvalErrorTraced::new(
                    EvalError::TypeError { expected: RuaType::Function, got: v.get_type() },
                    stack_trace,
                ))
            }
        }
    }

    fn return_op(
        &mut self,
        frame: &mut CallFrame,
        is_nil: bool,
        first_frame_id: usize,
    ) -> Option<RuaVal> {
        self.close_upvalues(frame.stack_start());
        let retval = if is_nil { RuaVal::Nil } else { self.pop() };
        self.stack.truncate(frame.stack_start());
        if frame.id() == first_frame_id {
            return Some(retval);
        }
        match self.frames.pop() {
            Some(f) => {
                self.push(retval);
                *frame = f;
                None
            }
            None => Some(retval),
        }
    }

    fn closure(&mut self, frame: &mut CallFrame, f: FnHandle) {
        let f = frame.read_function(f);
        let upvalue_count = f.upvalue_count();
        let mut closure = Closure::new(f);
        for _ in 0..upvalue_count {
            if let Instruction::Upvalue(up) = frame.curr_instr() {
                match up.location() {
                    Left(local) => {
                        self.capture_upvalue(&mut closure, frame.stack_start() + local.pos());
                    }
                    Right(upvalue) => {
                        closure.push_upvalue(frame.closure().get_upvalue(upvalue));
                    }
                }
            } else {
                unreachable!("Expected {upvalue_count} upvalues after this closure");
            }
        }
        self.push(RuaVal::Closure(closure.into()));
    }

    fn multiassign(&mut self, n: u8, frame: &mut CallFrame) -> Result<(), EvalErrorTraced> {
        let mut keys_in_stack = 0;
        let n = n as usize;
        for i in 0..n {
            match frame.curr_instr() {
                Instruction::SetLocal(local) => self.set_local(&*frame, local),
                Instruction::SetUpvalue(up) => self.set_upvalue(frame, up),
                Instruction::SetGlobal => {
                    let val = self.peek(0);
                    let key = self.peek(n - i + keys_in_stack);
                    self.global.insert(key, val);
                    self.pop();
                    keys_in_stack += 1;
                }
                Instruction::InsertKeyVal => {
                    let val = self.peek(0);
                    let key = self.peek(n - i + keys_in_stack);
                    let table = self.peek(n - i + keys_in_stack + 1);
                    let mut table = trace_err(table.into_table(), &*frame)?;
                    table.insert(key, val);
                    self.pop();
                    keys_in_stack += 2;
                }
                i => unreachable!("{i:?} cannot be part of Instruction::Multiassign"),
            }
        }
        self.drop(keys_in_stack);
        Ok(())
    }

    #[allow(clippy::cast_precision_loss)]
    fn len_op(&mut self) -> Result<(), EvalError> {
        self.unary_op(|v| Ok(((v.len())? as f64).into()))
    }

    fn set_upvalue(&mut self, frame: &mut CallFrame, up: UpvalueHandle) {
        let val = self.pop();
        frame.closure().set_upvalue(self, up, val);
    }

    fn set_local(&mut self, frame: &CallFrame, local: LocalHandle) {
        let val = self.pop();
        self.set_stack_at(frame.stack_start() + local.pos(), val);
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
        self.stack.pop().expect("Stack shouldn't be empty")
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

    fn capture_upvalue(&mut self, closure: &mut Closure, pos: usize) {
        let existing = self.open_upvalues.iter().find(|up| {
            let up: &Either<usize, RuaVal> = &up.borrow();
            match up {
                Left(up_pos) => *up_pos == pos,
                Right(_) => unreachable!("upvalue in open_upvalues is closed"),
            }
        });
        if let Some(up) = existing {
            closure.push_upvalue(up.clone());
        } else {
            let up = Rc::new(RefCell::new(Left(pos)));
            self.open_upvalues.push(up.clone());
            closure.push_upvalue(up);
        }
    }

    fn close_upvalues(&mut self, last: usize) {
        self.open_upvalues.retain(|up| {
            let up_pos = {
                let up: &Either<usize, RuaVal> = &up.borrow();
                match up {
                    Left(up_pos) => *up_pos,
                    Right(_) => unreachable!("upvalue in open_upvalues is closed"),
                }
            };
            let close = up_pos >= last;
            if close {
                up.replace(Right(std::mem::replace(&mut self.stack[up_pos], RuaVal::Nil)));
            }
            !close
        });
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

fn trace_err<T>(res: Result<T, EvalError>, frame: &CallFrame) -> Result<T, EvalErrorTraced> {
    res.map_err(|e| {
        let stack_trace = vec![(frame.func_name(), frame.curr_line())];
        EvalErrorTraced::new(e, stack_trace)
    })
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StringId(usize);
