pub mod native_functions;
pub mod vals;

use std::{
    cell::RefCell,
    hash::BuildHasherDefault,
    rc::{Rc, Weak},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    compiler::{locals::LocalHandle, upvalues::UpvalueHandle},
    eval::vals::{closure::Closure, IntoRuaVal, RuaType, StackTrace},
};
use either::Either::{self, Left, Right};
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
                    self.close_upvalues(frame.stack_start());
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
                    self.close_upvalues(frame.stack_start());
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
                Instruction::Mod => catch!(self.number_binary_op(|a, b| a % b)),
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
                        RuaVal::Closure(closure) => {
                            if closure.function().arity() < nargs {
                                self.drop((nargs - closure.function().arity()) as usize);
                            }
                            for _ in 0..closure.function().arity().saturating_sub(nargs) {
                                self.push(RuaVal::Nil);
                            }
                            let stack_start_pos =
                                self.get_frame_start(closure.function().arity() as usize);
                            #[cfg(test)]
                            println!(
                                "Started tracing {:?}, starting at stack {stack_start_pos}",
                                closure.function()
                            );
                            self.frames.push(frame);
                            frame = CallFrame::new(closure, stack_start_pos);
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
                    self.set_local(&frame, local);
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
                Instruction::Closure(c) => {
                    let constant = frame.read_constant(c);
                    if let RuaVal::Function(f) = constant {
                        let upvalue_count = f.upvalue_count();
                        let mut closure = Closure::new(f);
                        for _ in 0..upvalue_count {
                            if let Instruction::Upvalue(up) = frame.curr_instr() {
                                match up.location() {
                                    Left(local) => self.capture_upvalue(
                                        &mut closure,
                                        frame.stack_start() + local.pos(),
                                    ),
                                    Right(upvalue) => {
                                        closure.push_upvalue(frame.closure().get_upvalue(upvalue));
                                    }
                                }
                            } else {
                                unreachable!(
                                    "Expected {upvalue_count} upvalues after this closure"
                                );
                            }
                        }
                        self.push(RuaVal::Closure(closure.into()));
                    } else {
                        unreachable!(
                            "Shouldn't try to create closure out of a non-function object"
                        );
                    }
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
                    let mut keys_in_stack = 0;
                    let n = n as usize;
                    for i in 0..n {
                        match frame.curr_instr() {
                            Instruction::SetLocal(local) => self.set_local(&frame, local),
                            Instruction::SetUpvalue(up) => self.set_upvalue(&mut frame, up),
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
                                let mut table = catch!(table.into_table());
                                table.insert(key, val);
                                self.pop();
                                keys_in_stack += 2;
                            }
                            i => unreachable!("{i:?} cannot be part of Instruction::Multiassign"),
                        }
                    }
                    self.drop(keys_in_stack);
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

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StringId(usize);
