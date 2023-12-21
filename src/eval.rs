pub mod native_functions;
pub mod vals;

use std::{
    cell::RefCell,
    hash::BuildHasherDefault,
    num::NonZeroU32,
    rc::{Rc, Weak},
    sync::atomic::{AtomicU32, AtomicUsize, Ordering},
};

use crate::{
    compiler::{
        bytecode::{BinArgs, FnHandle, JmpArgs, UnArgs},
        upvalues::UpvalueHandle,
    },
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
    compiler::bytecode::Instruction as I,
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

const MIN_STACK_SIZE: usize = u8::MAX as usize;

pub struct Vm {
    stack: Vec<RuaVal>,
    frames: Vec<CallFrame>,
    global: Rc<Table>,
    identifiers: Trie<TokenType>,
    strings: WeakKeyHashMap<Weak<[u8]>, StringId, BuildHasherDefault<FxHasher>>,
    open_upvalues: Vec<UpvalueObj>,
    gc_data: GcData,
    id: NonZeroU32,
}

impl Vm {
    pub fn new() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(1);

        // No need to call register_table on global, it shouldn't
        // be collected until Vm is dropped anyways
        let global = Table::new().into();

        let mut vm = Self {
            stack: Vec::with_capacity(MIN_STACK_SIZE),
            frames: Vec::new(),
            global,
            identifiers: Trie::new(),
            strings: WeakKeyHashMap::default(),
            open_upvalues: Vec::new(),
            gc_data: GcData::default(),
            id: NonZeroU32::new(COUNTER.fetch_add(1, Ordering::Relaxed))
                .expect("Vm id cannot be zero"),
        };
        vm.global = default_global(&mut vm);
        vm
    }

    pub fn interpret(&mut self, closure: Rc<Closure>) -> RuaResult {
        let og_len = self.stack.len();
        #[cfg(test)]
        println!("Started tracing {:?}, starting at stack {og_len}\n\n", closure.function());
        self.stack.resize(og_len + closure.function().max_used_regs() as usize, RuaVal::nil());
        let frame = CallFrame::new(closure, og_len, og_len);
        let first_frame_id = frame.id();
        match self.interpret_error_boundary(frame) {
            Ok(v) => Ok(v),
            Err(mut e) => {
                for _ in self.frames.iter().rev().take_while(|frame| {
                    e.push_stack_trace(frame.func_name(), frame.curr_line());
                    frame.id() != first_frame_id
                }) {}
                self.stack.truncate(og_len);
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
                I::Return { src } => {
                    if let Some(val) = self.return_op(&mut frame, Some(src), first_frame_id) {
                        return Ok(val);
                    }
                }
                I::ReturnNil => {
                    if let Some(val) = self.return_op(&mut frame, None, first_frame_id) {
                        return Ok(val);
                    }
                }
                I::Number { dst, src } => {
                    let constant = frame.read_number(src);
                    self.set_stack_at(&frame, dst, constant.into());
                }
                I::String { dst, src } => {
                    let constant = frame.read_string(src);
                    self.set_stack_at(&frame, dst, constant.into());
                }
                I::True { dst } => self.set_stack_at(&frame, dst, true.into()),
                I::False { dst } => self.set_stack_at(&frame, dst, false.into()),
                I::Nil { dst } => self.set_stack_at(&frame, dst, RuaVal::nil()),
                I::LFalseSkip { dst } => {
                    self.set_stack_at(&frame, dst, false.into());
                    frame.skip_instr()
                }
                I::Neg(args) => trace_err(
                    self.unary_op(&frame, args, |v| Ok((-v.as_number()?).into())),
                    &frame,
                )?,
                I::Not(args) => {
                    trace_err(self.unary_op(&frame, args, |v| Ok((!v.truthy()).into())), &frame)?
                }
                I::Len(args) => trace_err(self.len_op(&frame, args), &frame)?,
                I::Add(args) => {
                    trace_err(self.number_binary_op(&frame, args, |a, b| a + b), &frame)?
                }
                I::Sub(args) => {
                    trace_err(self.number_binary_op(&frame, args, |a, b| a - b), &frame)?
                }
                I::Mul(args) => {
                    trace_err(self.number_binary_op(&frame, args, |a, b| a * b), &frame)?
                }
                I::Div(args) => {
                    trace_err(self.number_binary_op(&frame, args, |a, b| a / b), &frame)?
                }
                I::Mod(args) => {
                    trace_err(self.number_binary_op(&frame, args, |a, b| a % b), &frame)?
                }
                I::Pow(args) => trace_err(self.number_binary_op(&frame, args, f64::powf), &frame)?,
                I::StrConcat(args) => trace_err(self.str_concat(&frame, args), &frame)?,
                I::Eq(args) => {
                    trace_err(self.skip_if(&mut frame, args, |a, b| Ok(a == b)), &frame)?
                }
                I::Neq(args) => {
                    trace_err(self.skip_if(&mut frame, args, |a, b| Ok(a != b)), &frame)?
                }
                I::Lt(args) => trace_err(
                    self.skip_if(&mut frame, args, |a, b| Ok(a.as_number()? < b.as_number()?)),
                    &frame,
                )?,
                I::Gt(args) => trace_err(
                    self.skip_if(&mut frame, args, |a, b| Ok(a.as_number()? > b.as_number()?)),
                    &frame,
                )?,
                I::Le(args) => trace_err(
                    self.skip_if(&mut frame, args, |a, b| Ok(a.as_number()? <= b.as_number()?)),
                    &frame,
                )?,
                I::Ge(args) => trace_err(
                    self.skip_if(&mut frame, args, |a, b| Ok(a.as_number()? >= b.as_number()?)),
                    &frame,
                )?,
                I::Test { src } => {
                    let val = self.stack_at(&frame, src);
                    if val.truthy() {
                        frame.skip_instr()
                    }
                }
                I::Untest { src } => {
                    let val = self.stack_at(&frame, src);
                    if !val.truthy() {
                        frame.skip_instr()
                    }
                }
                I::TestSet { dst, src } => {
                    let val = self.stack_at(&frame, src);
                    if val.truthy() {
                        frame.skip_instr()
                    }
                    self.set_stack_at(&frame, dst, val.clone());
                }
                I::UntestSet { dst, src } => {
                    let val = self.stack_at(&frame, src);
                    if !val.truthy() {
                        frame.skip_instr()
                    }
                    self.set_stack_at(&frame, dst, val.clone());
                }
                I::SetGlobal { dst, src } => {
                    let val = self.stack_at(&frame, src);
                    let key = frame.read_string(dst);
                    self.global.insert(key.into(), val.clone());
                }
                I::GetGlobal { dst, src } => {
                    let key = frame.read_string(src);
                    self.set_stack_at(&frame, dst, self.global.get(&key.into()).into());
                }
                I::Call { base, nargs } => self.call(base, nargs, &mut frame)?,
                I::Mv(UnArgs { dst, src }) => {
                    let val = self.stack_at(&frame, src);
                    self.set_stack_at(&frame, dst, val.clone());
                }
                I::Jmp(offset) => frame.rel_jmp(offset - 1),
                I::NewTable(size) => {
                    let table = self.new_table(size);
                }
                I::InsertKeyVal => {
                    let table_val = self.peek(0);
                    let val = self.peek(1);
                    let key = self.peek(2);
                    let table = trace_err(table_val.as_table(), &frame)?;
                    table.insert(key, val);
                }
                I::InsertValKey => {
                    let table_val = self.peek(0);
                    let key = self.peek(1);
                    let val = self.peek(2);
                    let table = trace_err(table_val.as_table(), &frame)?;
                    table.insert(key, val);
                }
                I::Index(BinArgs { dst, lhs, rhs }) => {
                    let table = self.stack_at(&frame, lhs);
                    let key = self.stack_at(&frame, rhs);
                    let table = trace_err(table.as_table(), &frame)?;
                    self.set_stack_at(&frame, dst, table.get(&key).into());
                }
                I::Closure { dst, src } => {
                    self.closure(&mut frame, dst, src);
                }
                I::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                }
                I::GetUpvalue(up) => {
                    let val = frame.closure().get_upvalue_val(self, up);
                }
                I::SetUpvalue(up) => {
                    self.set_upvalue(&mut frame, up);
                }
                I::Upvalue(_) => {
                    unreachable!("I::Upvalue must be handled by I::Closure")
                }
                I::Multiassign(n) => {
                    self.multiassign(n, &mut frame)?;
                }
            }
        }
    }

    fn call(&mut self, base: u8, nargs: u8, frame: &mut CallFrame) -> Result<(), EvalErrorTraced> {
        let func = self.stack_at(&frame, base).clone();
        match func.into_callable() {
            Ok(Left(closure)) => {
                if closure.function().arity() < nargs {
                    // self.drop((nargs - closure.function().arity()) as usize);
                }
                for _ in 0..closure.function().arity().saturating_sub(nargs) {
                    // self.push(RuaVal::nil());
                }
                let og_len = self.stack.len();
                let stack_start_pos = self.get_frame_start(closure.function().arity() as usize);
                self.stack.resize(
                    stack_start_pos + closure.function().max_used_regs() as usize,
                    RuaVal::nil(),
                );
                #[cfg(test)]
                println!(
                    "Started tracing {:?}, starting at stack {stack_start_pos}",
                    closure.function()
                );
                frame.set_ret_pos(base);
                let old_frame =
                    std::mem::replace(frame, CallFrame::new(closure, stack_start_pos, og_len));
                self.frames.push(old_frame);
                Ok(())
            }
            Ok(Right(f)) => {
                let args =
                    self.stack[base as usize + 1..base as usize + 1 + nargs as usize].to_vec();
                let retval = match f.call(&args, self) {
                    Ok(v) => v,
                    Err(mut e) => {
                        e.push_stack_trace(frame.func_name(), frame.curr_line());
                        return Err(e);
                    }
                };
                self.set_stack_at(&frame, base, retval);
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
        ret_src: Option<u8>,
        first_frame_id: usize,
    ) -> Option<RuaVal> {
        self.close_upvalues(frame.stack_start());
        let retval = match ret_src {
            Some(src) => self.stack_at(frame, src).clone(),
            None => RuaVal::nil(),
        };
        self.stack.truncate(frame.prev_stack_size());
        if frame.id() == first_frame_id {
            return Some(retval);
        }
        match self.frames.pop() {
            Some(f) => {
                self.set_stack_at(&f, f.ret_pos(), retval);
                *frame = f;
                None
            }
            None => Some(retval),
        }
    }

    fn closure(&mut self, frame: &mut CallFrame, dst: u8, f: FnHandle) {
        let f = frame.read_function(f);
        let upvalue_count = f.upvalue_count();
        let mut closure = Closure::new(f);
        for _ in 0..upvalue_count {
            if let I::Upvalue(up) = frame.curr_instr() {
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
        self.set_stack_at(&frame, dst, closure);
    }

    fn multiassign(&mut self, n: u8, frame: &mut CallFrame) -> Result<(), EvalErrorTraced> {
        let mut keys_in_stack = 0;
        let n = n as usize;
        // for i in 0..n {
        //     match frame.curr_instr() {
        //         I::SetLocal(local) => self.set_local(&*frame, local),
        //         I::SetUpvalue(up) => self.set_upvalue(frame, up),
        //         I::SetGlobal(s) => {
        //             let val = self.peek(0);
        //             let key = frame.read_string(s);
        //             self.global.insert(key.into(), val);
        //             self.pop();
        //         }
        //         I::InsertKeyVal => {
        //             let val = self.peek(0);
        //             let key = self.peek(n - i + keys_in_stack);
        //             let table = self.peek(n - i + keys_in_stack + 1);
        //             let table = trace_err(table.as_table(), &*frame)?;
        //             table.insert(key, val);
        //             self.pop();
        //             keys_in_stack += 2;
        //         }
        //         i => unreachable!("{i:?} cannot be part of I::Multiassign"),
        //     }
        // }
        // self.drop(keys_in_stack);
        Ok(())
    }

    #[allow(clippy::cast_precision_loss)]
    fn len_op(&mut self, frame: &CallFrame, args: UnArgs) -> Result<(), EvalError> {
        self.unary_op(frame, args, |v| Ok(((v.len())? as f64).into()))
    }

    fn set_upvalue(&mut self, frame: &mut CallFrame, up: UpvalueHandle) {
        // let val = self.pop();
        // frame.closure().set_upvalue(self, up, val);
    }

    fn unary_op<F: Fn(&RuaVal) -> RuaResultUntraced>(
        &mut self,
        frame: &CallFrame,
        args: UnArgs,
        f: F,
    ) -> Result<(), EvalError> {
        let a = self.stack_at(frame, args.src);
        self.set_stack_at(&frame, args.dst, f(a)?);
        Ok(())
    }

    fn binary_op<F: Fn(&RuaVal, &RuaVal) -> RuaResultUntraced>(
        &mut self,
        frame: &CallFrame,
        args: BinArgs,
        f: F,
    ) -> Result<(), EvalError> {
        let a = self.stack_at(frame, args.lhs);
        let b = self.stack_at(frame, args.rhs);
        self.set_stack_at(&frame, args.dst, f(a, b)?);
        Ok(())
    }

    fn number_binary_op<T: Into<RuaVal>, F: Fn(f64, f64) -> T>(
        &mut self,
        frame: &CallFrame,
        args: BinArgs,
        f: F,
    ) -> Result<(), EvalError> {
        self.binary_op(frame, args, |a, b| Ok(f(a.as_number()?, b.as_number()?).into()))
    }

    fn str_concat(&mut self, frame: &CallFrame, args: BinArgs) -> Result<(), EvalError> {
        let a = self.stack_at(frame, args.lhs);
        let b = self.stack_at(frame, args.rhs);
        let res = [a.as_str()?, b.as_str()?].concat().into_rua(self);
        self.set_stack_at(&frame, args.dst, res);
        Ok(())
    }

    fn skip_if<F: FnOnce(&RuaVal, &RuaVal) -> Result<bool, EvalError>>(
        &self,
        frame: &mut CallFrame,
        args: JmpArgs,
        pred: F,
    ) -> Result<(), EvalError> {
        let (a, b) = (self.stack_at(frame, args.lhs), self.stack_at(frame, args.rhs));
        if pred(a, b)? {
            frame.skip_instr()
        }
        Ok(())
        // TODO optimize JMPs to avoid another instr dispatch cycle
    }

    // fn pop(&mut self) -> RuaVal {
    //     self.stack.pop().expect("Stack shouldn't be empty")
    // }

    fn peek(&self, back: usize) -> RuaVal {
        self.stack[self.stack.len() - 1 - back].clone()
    }

    fn stack_at(&self, frame: &CallFrame, idx: u8) -> &RuaVal {
        &self.stack[frame.stack_start() + idx as usize]
    }

    fn stack_at_abs(&self, idx: usize) -> &RuaVal {
        &self.stack[idx]
    }

    fn set_stack_at(&mut self, frame: &CallFrame, idx: u8, val: RuaVal) {
        self.stack[frame.stack_start() + idx as usize] = val;
    }

    fn set_stack_at_abs(&mut self, idx: usize, val: RuaVal) {
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
        for (i, el) in self.stack.iter().enumerate() {
            println!("[ {el:?} ] {}", if i == frame.stack_start() { "<-" } else { "" });
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

    fn new_table(&mut self, size: u16) -> RuaVal {
        Rc::new(Table::with_capacity(size.into())).into_rua(self)
    }

    fn register_table(&mut self, table: &Rc<Table>) {
        trace_gc!("register_table 0x{:x}", table.addr());

        if self.should_gc() {
            self.gc();
        }
        push_cleaning_weaks(&mut self.gc_data.tables, Rc::downgrade(table));
    }

    fn register_closure(&mut self, closure: &Rc<Closure>) {
        trace_gc!("register_closure 0x{:x}", closure.addr());

        if self.should_gc() {
            self.gc();
        }
        push_cleaning_weaks(&mut self.gc_data.closures, Rc::downgrade(closure));
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

    const fn id(&self) -> NonZeroU32 {
        self.id
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

fn push_cleaning_weaks<T>(vec: &mut Vec<Weak<T>>, val: Weak<T>) {
    if vec.len() == vec.capacity() {
        vec.retain(|el| el.upgrade().is_some());
        vec.reserve(vec.len()); // Reserve at least n more slots
    }
    vec.push(val);
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StringId(usize);
