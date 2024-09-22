pub mod native_functions;
pub mod vals;

use std::{
    cell::RefCell,
    io,
    num::NonZeroU32,
    ops::BitAnd,
    rc::{Rc, Weak},
    sync::atomic::{AtomicU32, Ordering},
};

use crate::{
    compiler::{
        bytecode::{
            BinArgs, FnHandle, NVArgs, NumberHandle, StringHandle, UnArgs, VNArgs, VVJmpArgs,
        },
        upvalues::UpvalueLocation,
    },
    eval::{
        macros::trace_gc,
        vals::{closure::Closure, IntoRuaVal},
    },
};
use rand::{rngs::StdRng, SeedableRng};
use rua_trie::Trie;

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
    vals::{
        function::NativeFunction, Callable, EvalErrorTraced, RuaResult, RuaResultTraced, Upvalue,
        UpvalueObj,
    },
    weak_interner::WeakInterner,
};

mod call_frame;
mod macros;
mod tests;
mod weak_interner;

#[cfg(not(test))]
const RECURSION_LIMIT: usize = 500_000;
#[cfg(test)]
const RECURSION_LIMIT: usize = 10;

pub struct Vm {
    stack: Vec<RuaVal>,
    frames: Vec<CallFrame>,
    global: Rc<Table>,
    identifiers: Trie<TokenType>,
    strings: WeakInterner<[u8]>,
    open_upvalues: Vec<UpvalueObj>,
    gc_data: GcData,
    id: NonZeroU32,
    stdout: Box<dyn io::Write>,
    rng: StdRng,
}

const REMAINDER: fn(f64, f64) -> f64 = f64::rem_euclid;

impl Vm {
    #[allow(clippy::missing_panics_doc)]
    #[must_use]
    pub fn with_args(args: &[Rc<[u8]>], stdout: Box<dyn io::Write>) -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(1);

        // No need to call register_table on global, it shouldn't
        // be collected until Vm is dropped anyways
        let global = Table::new().into();

        let mut vm = Self {
            stack: Vec::with_capacity(u8::MAX as usize),
            frames: Vec::new(),
            global,
            identifiers: Trie::new(),
            strings: WeakInterner::default(),
            open_upvalues: Vec::new(),
            gc_data: GcData::default(),
            id: NonZeroU32::new(COUNTER.fetch_add(1, Ordering::Relaxed))
                .expect("Vm id cannot be zero"),
            stdout,
            rng: StdRng::from_entropy(),
        };
        vm.global = default_global(&mut vm);

        let args: Table = args.iter().map(|s| vm.new_string(s.clone())).collect();
        let args = args.into_rua(&mut vm);
        let arg_str = vm.new_string((*b"arg").into()).into();
        vm.global.insert(arg_str, args).expect("arg is a valid table key");
        vm
    }

    #[must_use]
    pub fn new(stdout: Box<dyn io::Write>) -> Self {
        Self::with_args(&[], stdout)
    }

    /// # Errors
    ///
    /// Returns any errors encountered during evaluation
    #[allow(clippy::missing_panics_doc)]
    pub fn interpret(&mut self, closure: Rc<Closure>) -> RuaResultTraced {
        let og_len = self.stack.len();
        #[cfg(test)]
        println!("Started tracing {:?}, starting at stack {og_len}\n\n", closure.function());
        self.stack.resize(og_len + closure.function().max_used_regs() as usize, RuaVal::nil());
        let frame = CallFrame::new(closure, og_len, og_len, u8::MAX, usize::MAX);
        let first_frame_id = frame.id();
        match self.interpret_error_boundary(frame) {
            Ok(v) => Ok(v),
            Err(mut e) => {
                let mut iter = self.frames.iter().rev();
                let mut ip = if let Some(frame) = iter.next() {
                    // Patch last stack trace,
                    // necessary because each frame contains the IP for the previous frame
                    let last_trace =
                        e.stack_trace().last_mut().expect("must be at least 1 stack_trace");
                    last_trace.0 = frame.func_name();
                    last_trace.1 = frame.line_at(last_trace.1);
                    if frame.id() == first_frame_id {
                        self.stack.truncate(og_len);
                        return Err(e);
                    }
                    frame.ret_ip()
                } else {
                    e.stack_trace().pop();
                    self.stack.truncate(og_len);
                    return Err(e);
                };

                for _ in iter.take_while(|frame| {
                    e.push_stack_trace(frame.func_name(), frame.line_at(ip));
                    ip = frame.ret_ip();
                    frame.id() != first_frame_id
                }) {}
                self.stack.truncate(og_len);
                Err(e)
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn interpret_error_boundary(&mut self, mut frame: CallFrame) -> RuaResultTraced {
        let first_frame_id = frame.id();
        let mut ip = 0;

        macro_rules! table_get {
            ($dst: expr, $table: expr, $key: expr) => {{
                let table = self.stack_at(&frame, $table);
                let table = trace(table.as_table(), &frame, ip)?;
                self.set_stack_at(&frame, $dst, table.get($key).into());
            }};
        }

        loop {
            #[cfg(test)]
            self.trace(&frame, ip);
            let instr = frame.instr_at(ip);
            ip += 1;
            match instr {
                I::Return { src } => {
                    ip = frame.ret_ip();
                    if let Some(val) = self.return_op(&mut frame, Some(src), first_frame_id) {
                        return Ok(val);
                    }
                }
                I::ReturnNil => {
                    ip = frame.ret_ip();
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
                    ip += 1;
                }
                I::Neg(args) => trace(
                    self.unary_op(&frame, args, |v| Ok((-v.as_number()?).into())),
                    &frame,
                    ip,
                )?,
                I::Not(args) => {
                    trace(self.unary_op(&frame, args, |v| Ok((!v.truthy()).into())), &frame, ip)?;
                }
                I::Len(args) => trace(self.len_op(&frame, args), &frame, ip)?,
                I::AddVV(args) => trace(self.num_bin_op(&frame, args, |a, b| a + b), &frame, ip)?,
                I::SubVV(args) => trace(self.num_bin_op(&frame, args, |a, b| a - b), &frame, ip)?,
                I::MulVV(args) => trace(self.num_bin_op(&frame, args, |a, b| a * b), &frame, ip)?,
                I::DivVV(args) => trace(self.num_bin_op(&frame, args, |a, b| a / b), &frame, ip)?,
                I::ModVV(args) => trace(self.num_bin_op(&frame, args, REMAINDER), &frame, ip)?,
                I::PowVV(args) => trace(self.num_bin_op(&frame, args, f64::powf), &frame, ip)?,
                I::AddVN(args) => trace(self.num_vn_op(&frame, args, |a, b| a + b), &frame, ip)?,
                I::SubVN(args) => trace(self.num_vn_op(&frame, args, |a, b| a - b), &frame, ip)?,
                I::MulVN(args) => trace(self.num_vn_op(&frame, args, |a, b| a * b), &frame, ip)?,
                I::DivVN(args) => trace(self.num_vn_op(&frame, args, |a, b| a / b), &frame, ip)?,
                I::ModVN(args) => trace(self.num_vn_op(&frame, args, REMAINDER), &frame, ip)?,
                I::PowVN(args) => trace(self.num_vn_op(&frame, args, f64::powf), &frame, ip)?,
                I::SubNV(args) => trace(self.num_nv_op(&frame, args, |a, b| a - b), &frame, ip)?,
                I::DivNV(args) => trace(self.num_nv_op(&frame, args, |a, b| a / b), &frame, ip)?,
                I::ModNV(args) => trace(self.num_nv_op(&frame, args, REMAINDER), &frame, ip)?,
                I::PowNV(args) => trace(self.num_nv_op(&frame, args, f64::powf), &frame, ip)?,
                I::StrConcat(args) => trace(self.str_concat(&frame, args), &frame, ip)?,
                I::EqVV(args) => {
                    ip = trace(self.skip_if_vv(&frame, args, |a, b| Ok(a == b), ip), &frame, ip)?;
                }
                I::NeqVV(args) => {
                    ip = trace(self.skip_if_vv(&frame, args, |a, b| Ok(a != b), ip), &frame, ip)?;
                }
                I::LtVV(args) => {
                    ip = trace(
                        self.skip_if_vv(
                            &frame,
                            args,
                            |a, b| Ok(a.as_number_strict()? < b.as_number_strict()?),
                            ip,
                        ),
                        &frame,
                        ip,
                    )?;
                }
                I::LeVV(args) => {
                    ip = trace(
                        self.skip_if_vv(
                            &frame,
                            args,
                            |a, b| Ok(a.as_number_strict()? <= b.as_number_strict()?),
                            ip,
                        ),
                        &frame,
                        ip,
                    )?;
                }
                I::EqVN { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_vn(&frame, lhs, rhs, |a, b| Ok(a == &b.into()), ip),
                        &frame,
                        ip,
                    )?;
                }
                I::NeqVN { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_vn(&frame, lhs, rhs, |a, b| Ok(a != &b.into()), ip),
                        &frame,
                        ip,
                    )?;
                }
                I::LtVN { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_vn(&frame, lhs, rhs, |a, b| Ok(a.as_number_strict()? < b), ip),
                        &frame,
                        ip,
                    )?;
                }
                I::LeVN { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_vn(
                            &frame,
                            lhs,
                            rhs,
                            |a, b| Ok(a.as_number_strict()? <= b),
                            ip,
                        ),
                        &frame,
                        ip,
                    )?;
                }
                I::EqNV { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_nv(&frame, lhs, rhs, |a, b| Ok(&RuaVal::from(a) == b), ip),
                        &frame,
                        ip,
                    )?;
                }
                I::NeqNV { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_nv(&frame, lhs, rhs, |a, b| Ok(&RuaVal::from(a) != b), ip),
                        &frame,
                        ip,
                    )?;
                }
                I::LtNV { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_nv(&frame, lhs, rhs, |a, b| Ok(a < b.as_number_strict()?), ip),
                        &frame,
                        ip,
                    )?;
                }
                I::LeNV { lhs, rhs } => {
                    ip = trace(
                        self.skip_if_nv(
                            &frame,
                            lhs,
                            rhs,
                            |a, b| Ok(a <= b.as_number_strict()?),
                            ip,
                        ),
                        &frame,
                        ip,
                    )?;
                }
                I::Test { src } => {
                    let val = self.stack_at(&frame, src);
                    if val.truthy() {
                        ip += 1;
                    }
                }
                I::Untest { src } => {
                    let val = self.stack_at(&frame, src);
                    if !val.truthy() {
                        ip += 1;
                    }
                }
                I::TestSet { dst, src } => {
                    let val = self.stack_at(&frame, src);
                    if val.truthy() {
                        ip += 1;
                    }
                    self.set_stack_at(&frame, dst, val.clone());
                }
                I::UntestSet { dst, src } => {
                    let val = self.stack_at(&frame, src);
                    if !val.truthy() {
                        ip += 1;
                    }
                    self.set_stack_at(&frame, dst, val.clone());
                }
                I::SetGlobal { dst, src } => {
                    let val = self.stack_at(&frame, src);
                    let key = frame.read_string(dst);
                    trace(self.global.insert(key.into(), val.clone()), &frame, ip)?;
                }
                I::GetGlobal { dst, src } => {
                    let key = frame.read_string(src);
                    self.set_stack_at(&frame, dst, self.global.get(&key.into()).into());
                }
                I::Call { base, nargs } => ip = self.call(base, nargs, &mut frame, ip)?,
                I::TailCall { base, nargs } => {
                    let (new_ip, val) =
                        self.tail_call(base, nargs, &mut frame, ip, first_frame_id)?;
                    ip = new_ip;
                    if let Some(val) = val {
                        return Ok(val);
                    }
                }
                I::Mv(UnArgs { dst, src }) => {
                    let val = self.stack_at(&frame, src);
                    self.set_stack_at(&frame, dst, val.clone());
                }
                I::Jmp(offset) => {
                    ip = compute_jmp(ip, offset);
                }
                I::ForPrep { from, offset } => {
                    ip = trace(self.for_prep(&frame, from, offset, ip), &frame, ip)?;
                }
                I::ForLoop { from, offset } => {
                    ip = self.for_loop(&frame, from, offset, ip);
                }
                I::NewTable { dst, capacity } => {
                    let table = self.new_table(capacity);
                    self.set_stack_at(&frame, dst, table);
                }
                I::InsertV { table, key, val } => {
                    self.table_insert(&frame, table, val, self.stack_at(&frame, key).clone(), ip)?;
                }
                I::InsertS { table, key, val } => {
                    self.table_insert(
                        &frame,
                        table,
                        val,
                        frame.read_string(StringHandle::from_unchecked(key)).into(),
                        ip,
                    )?;
                }
                I::InsertN { table, key, val } => {
                    self.table_insert(
                        &frame,
                        table,
                        val,
                        frame.read_number(NumberHandle::from_unchecked(key)).into(),
                        ip,
                    )?;
                }
                I::IndexV(BinArgs { dst, lhs, rhs }) => {
                    table_get!(dst, lhs, self.stack_at(&frame, rhs));
                }
                I::IndexS(BinArgs { dst, lhs, rhs }) => {
                    table_get!(
                        dst,
                        lhs,
                        &frame.read_string(StringHandle::from_unchecked(rhs)).into()
                    );
                }
                I::IndexN(BinArgs { dst, lhs, rhs }) => {
                    table_get!(
                        dst,
                        lhs,
                        &frame.read_number(NumberHandle::from_unchecked(rhs)).into()
                    );
                }
                I::Closure { dst, src } => {
                    ip = self.closure(&frame, dst, src, ip);
                }
                I::CloseUpvalues { from, to } => {
                    self.close_upvalues(from.into(), to.into());
                }
                I::GetUpvalue { dst, src } => {
                    let val = frame.closure().get_upvalue_val(self, src);
                    self.set_stack_at(&frame, dst, val);
                }
                I::SetUpvalue { dst, src } => {
                    let val = self.stack_at(&frame, src);
                    frame.closure().set_upvalue(self, dst, val.clone());
                }
                I::Upvalue(_) => {
                    unreachable!("I::Upvalue must be handled by I::Closure")
                }
            }
        }
    }

    fn table_insert(
        &self,
        frame: &CallFrame,
        table: u8,
        val: u8,
        key: RuaVal,
        ip: usize,
    ) -> Result<(), EvalErrorTraced> {
        let table = self.stack_at(frame, table);
        let table = trace(table.as_table(), frame, ip)?;
        let val = self.stack_at(frame, val);
        trace(table.insert(key, val.clone()), frame, ip)?;
        Ok(())
    }

    fn continue_loop(from: f64, to: f64, step: f64) -> bool {
        (step >= 0.0 && from <= to) || (step < 0.0 && from >= to)
    }

    fn for_loop(&mut self, frame: &CallFrame, from: u8, offset: u16, ip: usize) -> usize {
        const ERR_MSG: &str = "Loop vals must be numbers (checked at for_prep)";

        let step_val = self.stack_at(frame, from + 2);
        let to_val = self.stack_at(frame, from + 1);
        let from_val = self.stack_at(frame, from);

        let step_val = step_val.as_number_strict().expect(ERR_MSG);
        let to_val = to_val.as_number_strict().expect(ERR_MSG);
        let from_val = from_val.as_number_strict().expect(ERR_MSG);

        let from_val = from_val + step_val;
        if Self::continue_loop(from_val, to_val, step_val) {
            self.set_stack_at(frame, from + 3, from_val.into());
            self.set_stack_at(frame, from, from_val.into());
            ip - offset as usize
        } else {
            ip
        }
    }

    fn for_prep(
        &mut self,
        frame: &CallFrame,
        from: u8,
        offset: u16,
        ip: usize,
    ) -> Result<usize, EvalError> {
        let step_val = self.stack_at(frame, from + 2);
        let to_val = self.stack_at(frame, from + 1);
        let from_val = self.stack_at(frame, from);

        let step_val = step_val.as_number_strict()?;
        let to_val = to_val.as_number_strict()?;
        let from_val = from_val.as_number_strict()?;

        if Self::continue_loop(from_val, to_val, step_val) {
            self.set_stack_at(frame, from + 3, from_val.into());
            Ok(ip)
        } else {
            Ok(ip + offset as usize)
        }
    }

    fn call(
        &mut self,
        base: u8,
        nargs: u8,
        frame: &mut CallFrame,
        curr_ip: usize,
    ) -> Result<usize, EvalErrorTraced> {
        let func = self.stack_at(frame, base).clone();
        match func.into_callable() {
            Ok(Callable::Closure(closure)) => {
                trace(
                    self.call_closure::<false>(frame, closure, base, nargs, curr_ip),
                    frame,
                    curr_ip,
                )?;
                Ok(0)
            }
            Ok(Callable::Native(f)) => {
                self.call_native(frame, &f, base, nargs, curr_ip)?;
                Ok(curr_ip)
            }
            Err(e) => Err(trace_err(e, frame, curr_ip)),
        }
    }

    fn tail_call(
        &mut self,
        base: u8,
        nargs: u8,
        frame: &mut CallFrame,
        curr_ip: usize,
        first_frame_id: usize,
    ) -> Result<(usize, Option<RuaVal>), EvalErrorTraced> {
        let func = self.stack_at(frame, base).clone();
        match func.into_callable() {
            Ok(Callable::Closure(closure)) => {
                // Tail call cannot overflow stack
                let _ = self.call_closure::<true>(frame, closure, base, nargs, curr_ip);
                Ok((0, None))
            }
            Ok(Callable::Native(f)) => {
                self.call_native(frame, &f, base, nargs, curr_ip)?;
                Ok((curr_ip, self.return_op(frame, Some(base), first_frame_id)))
            }
            Err(e) => {
                let stack_trace =
                    vec![(frame.func_name(), frame.line_at(curr_ip)), ("".into(), frame.ret_ip())];
                Err(EvalErrorTraced::new(e, stack_trace))
            }
        }
    }

    #[inline]
    fn call_closure<const TAIL: bool>(
        &mut self,
        frame: &mut CallFrame,
        closure: Rc<Closure>,
        base: u8,
        nargs: u8,
        curr_ip: usize,
    ) -> Result<(), EvalError> {
        let og_len = self.stack.len();

        let stack_start_pos = if TAIL {
            self.close_upvalues(frame.stack_start(), self.stack.len());
            // Copy args and function to current stack base
            for i in 0..=nargs {
                let val = std::mem::take(&mut self.stack[frame.resolve_reg(base + i)]);
                self.set_stack_at(frame, i, val);
            }
            frame.stack_start()
        } else {
            frame.resolve_reg(base)
        };
        self.stack.resize(
            og_len.max(stack_start_pos + closure.function().max_used_regs() as usize),
            RuaVal::nil(),
        );
        for i in 0..closure.function().arity().saturating_sub(nargs) {
            self.set_stack_at_abs(stack_start_pos + nargs as usize + i as usize, RuaVal::nil());
        }

        #[cfg(test)]
        println!("Started tracing {:?}, starting at stack {stack_start_pos}", closure.function());

        let new_frame = if TAIL {
            CallFrame::new(
                closure,
                stack_start_pos,
                frame.prev_stack_size(),
                frame.retval_dst(),
                frame.ret_ip(),
            )
        } else {
            CallFrame::new(closure, stack_start_pos, og_len, base, curr_ip)
        };
        let old_frame = std::mem::replace(frame, new_frame);
        if !TAIL {
            self.frames.push(old_frame);
            if self.frames.len() >= RECURSION_LIMIT {
                return Err(EvalError::StackOverflow);
            }
        }
        Ok(())
    }

    #[inline]
    fn call_native(
        &mut self,
        frame: &CallFrame,
        native_fn: &Rc<NativeFunction>,
        base: u8,
        nargs: u8,
        curr_ip: usize,
    ) -> Result<(), EvalErrorTraced> {
        let arg_start = frame.resolve_reg(base) + 1;
        let retval = match native_fn.call(self, arg_start, nargs) {
            Ok(v) => v,
            Err(mut e) => {
                e.push_stack_trace(frame.func_name(), frame.line_at(curr_ip));
                e.push_stack_trace("".into(), frame.ret_ip());
                return Err(e);
            }
        };
        self.set_stack_at(frame, base, retval);
        Ok(())
    }

    fn return_op(
        &mut self,
        frame: &mut CallFrame,
        ret_src: Option<u8>,
        first_frame_id: usize,
    ) -> Option<RuaVal> {
        self.close_upvalues(frame.stack_start(), self.stack.len());
        let retval = match ret_src {
            Some(src) => std::mem::take(&mut self.stack[frame.stack_start() + src as usize]),
            None => RuaVal::nil(),
        };
        let retval_dst = frame.retval_dst();
        self.stack.truncate(frame.prev_stack_size());
        if frame.id() == first_frame_id {
            return Some(retval);
        }
        match self.frames.pop() {
            Some(f) => {
                self.set_stack_at(&f, retval_dst, retval);
                *frame = f;
                None
            }
            None => Some(retval),
        }
    }

    fn closure(&mut self, frame: &CallFrame, dst: u8, f: FnHandle, mut ip: usize) -> usize {
        let f = frame.read_function(f);
        let upvalue_count = f.upvalue_count();
        let mut closure = Closure::new(f);
        for _ in 0..upvalue_count {
            if let I::Upvalue(up) = frame.instr_at(ip) {
                match up.location() {
                    UpvalueLocation::ParentStack(local) => {
                        self.capture_upvalue(&mut closure, frame.stack_start() + local.pos());
                    }
                    UpvalueLocation::ParentUpval(upvalue) => {
                        closure.push_upvalue(frame.closure().get_upvalue(upvalue));
                    }
                }
                ip += 1;
            } else {
                unreachable!("Expected {upvalue_count} upvalues after this closure");
            }
        }
        let closure = Rc::new(closure).into_rua(self);
        self.set_stack_at(frame, dst, closure);
        ip
    }

    #[allow(clippy::cast_precision_loss)]
    fn len_op(&mut self, frame: &CallFrame, args: UnArgs) -> Result<(), EvalError> {
        self.unary_op(frame, args, |v| Ok(((v.len())? as f64).into()))
    }

    #[inline]
    fn unary_op<F: Fn(&RuaVal) -> RuaResult>(
        &mut self,
        frame: &CallFrame,
        args: UnArgs,
        f: F,
    ) -> Result<(), EvalError> {
        let a = self.stack_at(frame, args.src);
        self.set_stack_at(frame, args.dst, f(a)?);
        Ok(())
    }

    #[inline]
    fn binary_op<F: Fn(&RuaVal, &RuaVal) -> RuaResult>(
        &mut self,
        frame: &CallFrame,
        args: BinArgs,
        f: F,
    ) -> Result<(), EvalError> {
        let (a, b) = (self.stack_at(frame, args.lhs), self.stack_at(frame, args.rhs));
        self.set_stack_at(frame, args.dst, f(a, b)?);
        Ok(())
    }

    #[inline]
    fn num_bin_op<T: Into<RuaVal>, F: Fn(f64, f64) -> T>(
        &mut self,
        frame: &CallFrame,
        args: BinArgs,
        f: F,
    ) -> Result<(), EvalError> {
        self.binary_op(frame, args, |a, b| Ok(f(a.as_number()?, b.as_number()?).into()))
    }

    #[inline]
    fn num_vn_op<T: Into<RuaVal>, F: Fn(f64, f64) -> T>(
        &mut self,
        frame: &CallFrame,
        args: VNArgs,
        f: F,
    ) -> Result<(), EvalError> {
        let v = self.stack_at(frame, args.lhs).as_number()?;
        let n = frame.read_number(NumberHandle::from_unchecked(args.rhs));
        self.set_stack_at(frame, args.dst, f(v, n).into());
        Ok(())
    }

    #[inline]
    fn num_nv_op<T: Into<RuaVal>, F: Fn(f64, f64) -> T>(
        &mut self,
        frame: &CallFrame,
        args: NVArgs,
        f: F,
    ) -> Result<(), EvalError> {
        let n = frame.read_number(NumberHandle::from_unchecked(args.lhs));
        let v = self.stack_at(frame, args.rhs).as_number()?;
        self.set_stack_at(frame, args.dst, f(n, v).into());
        Ok(())
    }

    fn str_concat(&mut self, frame: &CallFrame, args: BinArgs) -> Result<(), EvalError> {
        let (a, b) = (self.stack_at(frame, args.lhs), self.stack_at(frame, args.rhs));
        let res = [a.as_str()?, b.as_str()?].concat().into_rua(self);
        self.set_stack_at(frame, args.dst, res);
        Ok(())
    }

    #[inline]
    fn skip_if_vv<F: FnOnce(&RuaVal, &RuaVal) -> Result<bool, EvalError>>(
        &self,
        frame: &CallFrame,
        args: VVJmpArgs,
        pred: F,
        ip: usize,
    ) -> Result<usize, EvalError> {
        let (a, b) = (self.stack_at(frame, args.lhs), self.stack_at(frame, args.rhs));
        Ok(Self::skip_if_imm(frame, pred(a, b)?, ip))
    }

    #[inline]
    fn skip_if_vn<F: FnOnce(&RuaVal, f64) -> Result<bool, EvalError>>(
        &self,
        frame: &CallFrame,
        lhs: u8,
        rhs: NumberHandle,
        pred: F,
        ip: usize,
    ) -> Result<usize, EvalError> {
        let (a, b) = (self.stack_at(frame, lhs), frame.read_number(rhs));
        Ok(Self::skip_if_imm(frame, pred(a, b)?, ip))
    }

    #[inline]
    fn skip_if_nv<F: FnOnce(f64, &RuaVal) -> Result<bool, EvalError>>(
        &self,
        frame: &CallFrame,
        lhs: NumberHandle,
        rhs: u8,
        pred: F,
        ip: usize,
    ) -> Result<usize, EvalError> {
        let (a, b) = (frame.read_number(lhs), self.stack_at(frame, rhs));
        Ok(Self::skip_if_imm(frame, pred(a, b)?, ip))
    }

    #[inline]
    fn skip_if_imm(frame: &CallFrame, imm: bool, ip: usize) -> usize {
        if imm {
            // Skip next instruction (Jmp)
            ip + 1
        } else {
            // Take Jmp
            let instr = frame.instr_at(ip);
            match instr {
                I::Jmp(offset) => compute_jmp(ip, offset) + 1,
                _ => unreachable!("Instruction after condition must be a Jmp"),
            }
        }
    }

    #[inline]
    fn stack_at(&self, frame: &CallFrame, idx: u8) -> &RuaVal {
        self.stack_at_abs(frame.resolve_reg(idx))
    }

    #[inline]
    fn stack_at_abs(&self, idx: usize) -> &RuaVal {
        &self.stack[idx]
    }

    #[inline]
    fn set_stack_at(&mut self, frame: &CallFrame, idx: u8, val: RuaVal) {
        self.set_stack_at_abs(frame.resolve_reg(idx), val);
    }

    #[inline]
    fn set_stack_at_abs(&mut self, idx: usize, val: RuaVal) {
        self.stack[idx] = val;
    }

    pub(crate) fn identifiers(&mut self) -> &mut Trie<TokenType> {
        &mut self.identifiers
    }

    pub fn new_string(&mut self, s: Rc<[u8]>) -> RuaString {
        let (s, hash) = self.strings.insert_or_get(s);
        RuaString::new(s, hash)
    }

    fn capture_upvalue(&mut self, closure: &mut Closure, pos: usize) {
        let existing = self.open_upvalues.iter().find(|up| match &*up.borrow() {
            Upvalue::Open(up_pos) => *up_pos == pos,
            Upvalue::Closed(_) => unreachable!("upvalue in open_upvalues is closed"),
        });
        if let Some(up) = existing {
            closure.push_upvalue(up.clone());
        } else {
            let up = Rc::new(RefCell::new(Upvalue::Open(pos)));
            self.open_upvalues.push(up.clone());
            closure.push_upvalue(up);
        }
    }

    fn close_upvalues(&mut self, from: usize, to: usize) {
        self.open_upvalues.retain(|up| {
            let up_pos = {
                match &*up.borrow() {
                    Upvalue::Open(up_pos) => *up_pos,
                    Upvalue::Closed(_) => unreachable!("upvalue in open_upvalues is closed"),
                }
            };
            let close = up_pos >= from && to >= up_pos;
            if close {
                up.replace(Upvalue::Closed(self.stack[up_pos].clone()));
            }
            !close
        });
    }

    #[cfg(test)] // TODO add cfg for tracing
    fn trace(&self, frame: &CallFrame, ip: usize) {
        for (i, el) in self.stack.iter().enumerate() {
            println!("[ {el:?} ] {}", if i == frame.stack_start() { "<-" } else { "" });
        }
        frame.print_instr_at(ip);
    }

    #[cfg(test)]
    const fn stack(&self) -> &Vec<RuaVal> {
        &self.stack
    }

    fn new_table(&mut self, capacity: u16) -> RuaVal {
        let map_capacity_log = capacity.bitand(0x000F);
        let map_capacity = if map_capacity_log == 0 { 0usize } else { 1 << map_capacity_log };
        let arr_capacity = capacity.bitand(0xFFF0) >> 4;
        Rc::new(Table::with_capacities(map_capacity, arr_capacity.into())).into_rua(self)
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
        let id = self.id();
        if self.global.mark() {
            // SAFETY: global doesn't need to be garbage collected, since it'll be valid
            // as long as the Vm is valid, and it's dropped when the Vm is dropped
            self.gc_data.add_grey(RuaVal::from_table_unregistered(self.global.clone(), id));
        }

        trace_gc!("Frame roots:");
        for frame in &mut self.frames {
            if frame.closure().mark() {
                // SAFETY: any closure in self.frames has been created and registered
                // at some other point in time
                self.gc_data
                    .add_grey(RuaVal::from_closure_unregistered(frame.closure().clone(), id));
            }
        }
    }

    const fn id(&self) -> NonZeroU32 {
        self.id
    }
}

#[allow(clippy::cast_sign_loss)]
#[allow(clippy::cast_possible_wrap)]
#[inline]
const fn compute_jmp(ip: usize, offset: i16) -> usize {
    (ip as isize + offset as isize) as usize
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

#[inline]
fn trace<T>(res: Result<T, EvalError>, frame: &CallFrame, ip: usize) -> Result<T, EvalErrorTraced> {
    res.map_err(|e| trace_err(e, frame, ip))
}

fn trace_err(e: EvalError, frame: &CallFrame, ip: usize) -> EvalErrorTraced {
    let stack_trace = vec![(frame.func_name(), frame.line_at(ip)), ("".into(), frame.ret_ip())];
    EvalErrorTraced::new(e, stack_trace)
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
        Self::new(Box::new(io::stdout()))
    }
}
