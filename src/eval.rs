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
    eval::{
        macros::trace_gc,
        vals::{closure::Closure, IntoRuaVal},
    },
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
mod macros;
mod tests;

const STACK_SIZE: usize = u8::MAX as usize;

pub struct Vm {
    stack: Vec<RuaVal>,
    frames: Vec<CallFrame>,
    global: Rc<Table>,
    identifiers: Trie<TokenType>,
    strings: WeakKeyHashMap<Weak<[u8]>, StringId, BuildHasherDefault<FxHasher>>,
    open_upvalues: Vec<UpvalueObj>,
    gc_data: GcData,
}

impl Vm {
    pub fn new() -> Self {
        // No need to call register_table on global, it shouldn't
        // be collected until Vm is dropped anyways
        let global = Table::new().into();

        let mut vm = Self {
            stack: Vec::with_capacity(STACK_SIZE),
            frames: Vec::new(),
            global,
            identifiers: Trie::new(),
            strings: WeakKeyHashMap::default(),
            open_upvalues: Vec::new(),
            gc_data: GcData::default(),
        };
        vm.global = default_global(&mut vm);
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
                Instruction::Nil => self.push(RuaVal::nil()),
                Instruction::Neg => {
                    trace_err(self.unary_op(|v| Ok((-v.as_number()?).into())), &frame)?;
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
                    let table = self.new_table();
                    self.push(table);
                }
                Instruction::InsertKeyVal => {
                    let table = self.peek(0);
                    let val = self.peek(1);
                    let key = self.peek(2);
                    let table = trace_err(table.into_table(), &frame)?;
                    table.insert(key, val);
                    self.drop(3);
                    // SAFETY: table was on stack, it's already registered
                    self.push(unsafe { RuaVal::from_table_unregistered(table) });
                }
                Instruction::InsertValKey => {
                    let table = self.peek(0);
                    let key = self.peek(1);
                    let val = self.peek(2);
                    let table = trace_err(table.into_table(), &frame)?;
                    table.insert(key, val);
                    self.drop(3);
                    // SAFETY: table was on stack, it's already registered
                    self.push(unsafe { RuaVal::from_table_unregistered(table) });
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
        match func.into_callable() {
            Ok(Left(closure)) => {
                if closure.function().arity() < nargs {
                    self.drop((nargs - closure.function().arity()) as usize);
                }
                for _ in 0..closure.function().arity().saturating_sub(nargs) {
                    self.push(RuaVal::nil());
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
            Ok(Right(f)) => {
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
            Err(e) => {
                let stack_trace = vec![(frame.func_name(), frame.curr_line())];
                Err(EvalErrorTraced::new(e, stack_trace))
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
        let retval = if is_nil { RuaVal::nil() } else { self.pop() };
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
        let closure = Rc::new(closure).into_rua(self);
        self.push(closure);
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
                    let table = trace_err(table.into_table(), &*frame)?;
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
        self.binary_op(|a, b| Ok(f(a.as_number()?, b.as_number()?).into()))
    }

    fn str_concat(&mut self) -> Result<(), EvalError> {
        let b = self.pop();
        let a = self.pop();
        let res = [a.into_str()?, b.into_str()?].concat().into_rua(self);
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

    pub fn new_string(&mut self, s: Rc<[u8]>) -> RuaString {
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
            let up: &Either<_, _> = &up.borrow();
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
                let up: &Either<_, _> = &up.borrow();
                match up {
                    Left(up_pos) => *up_pos,
                    Right(_) => unreachable!("upvalue in open_upvalues is closed"),
                }
            };
            let close = up_pos >= last;
            if close {
                up.replace(Right(std::mem::replace(&mut self.stack[up_pos], RuaVal::nil())));
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

    fn new_table(&mut self) -> RuaVal {
        Rc::new(Table::new()).into_rua(self)
    }

    fn register_table(&mut self, table: &Rc<Table>) {
        trace_gc!("register_table 0x{:x}", table.addr());

        if self.should_gc() {
            self.gc();
        }
        self.gc_data.tables.push(Rc::downgrade(table));
    }

    fn register_closure(&mut self, closure: &Rc<Closure>) {
        trace_gc!("register_closure 0x{:x}", closure.addr());

        if self.should_gc() {
            self.gc();
        }
        self.gc_data.closures.push(Rc::downgrade(closure));
    }

    fn should_gc(&self) -> bool {
        self.gc_data.tables.len() + self.gc_data.closures.len() >= self.gc_data.next_gc
    }

    fn gc(&mut self) {
        trace_gc!("-- GC started --");

        self.mark_roots();
        trace_gc!("-- Marked roots --");

        self.gc_data.trace_refs();
        trace_gc!("-- Traced references --");

        self.gc_data.sweep();
        trace_gc!("-- Sweeped --");
        self.global.unmark();
        trace_gc!("-- Unmarked global --");

        self.gc_data.shrink();

        self.gc_data.compute_next_gc();

        trace_gc!("-- GC ended --");
    }

    fn mark_roots(&mut self) {
        trace_gc!("Stack roots:");
        for val in &mut self.stack {
            val.mark(&mut self.gc_data);
        }

        trace_gc!("Global root:");
        if self.global.mark() {
            // SAFETY: global doesn't need to be garbage collected, since it'll be valid
            // as long as the Vm is valid, and it's dropped when the Vm is dropped
            self.gc_data.add_grey(unsafe { RuaVal::from_table_unregistered(self.global.clone()) });
        }

        trace_gc!("Frame roots:");
        for frame in &mut self.frames {
            if frame.closure().mark() {
                // SAFETY: any closure in self.frames has been created and registered
                // at some other point in time
                self.gc_data.add_grey(unsafe {
                    RuaVal::from_closure_unregistered(frame.closure().clone())
                });
            }
        }
    }
}

struct GcData {
    tables: Vec<Weak<Table>>,
    closures: Vec<Weak<Closure>>,
    grey_vals: Vec<RuaVal>,
    next_gc: usize,
}

const MIN_NEXT_GC: usize = 2000;
impl Default for GcData {
    fn default() -> Self {
        Self {
            tables: Vec::new(),
            closures: Vec::new(),
            grey_vals: Vec::new(),
            next_gc: MIN_NEXT_GC,
        }
    }
}

impl GcData {
    fn add_grey(&mut self, val: RuaVal) {
        self.grey_vals.push(val);

        #[cfg(test)]
        assert!(self.grey_vals.len() <= 100_000, "Too many grey vals for test");
        // Too avoid crashing my PC when I screw up
    }

    fn trace_refs(&mut self) {
        while let Some(grey) = self.grey_vals.pop() {
            grey.blacken(self);
        }
    }

    fn sweep(&mut self) {
        self.tables.retain(|t| match t.upgrade() {
            Some(t) => {
                let retain = t.unmark();
                if !retain {
                    t.soft_drop();
                }
                retain
            }
            None => false,
        });

        self.closures.retain(|c| match c.upgrade() {
            Some(c) => {
                let retain = c.unmark();
                if !retain {
                    c.soft_drop();
                }
                retain
            }
            None => false,
        });
    }

    fn shrink(&mut self) {
        const SHRINK_FACTOR: usize = 3;
        const EXTRA_SPACE_FACTOR: usize = 5;
        const MIN_EXTRA_SPACE: usize = 10;
        const MAX_GREY_RESERVED: usize = 100;

        let v = &mut self.tables;
        if v.len() * SHRINK_FACTOR < v.capacity() {
            trace_gc!("-- Shrinking tables --");
            v.shrink_to(v.len() + MIN_EXTRA_SPACE.max(v.len() / EXTRA_SPACE_FACTOR));
        }
        let v = &mut self.closures;
        if v.len() * SHRINK_FACTOR < v.capacity() {
            trace_gc!("-- Shrinking closures --");
            v.shrink_to(v.len() + MIN_EXTRA_SPACE.max(v.len() / EXTRA_SPACE_FACTOR));
        }

        if self.grey_vals.capacity() > MAX_GREY_RESERVED {
            self.grey_vals.shrink_to(MAX_GREY_RESERVED);
        }
    }

    fn compute_next_gc(&mut self) {
        const GC_GROWTH_FACTOR: usize = 2;

        self.next_gc =
            MIN_NEXT_GC.max((self.tables.len() + self.closures.len()) * GC_GROWTH_FACTOR);
    }
}

impl Drop for Vm {
    fn drop(&mut self) {
        trace_gc!("Dropping vm");

        self.stack.clear();
        self.global.clear();
        self.frames.clear();
        self.gc();
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
