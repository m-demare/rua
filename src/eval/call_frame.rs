use struct_invariant::invariant;

use crate::compiler::bytecode::{FnHandle, Instruction, NumberHandle, StringHandle};

use std::{
    fmt::Debug,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use super::vals::{closure::Closure, function::Function, string::RuaString};

pub struct CallFrame {
    closure: Rc<Closure>,
    ip: usize,
    start: usize,
    id: usize,
    ret_pos: u8,
    prev_stack_size: usize,
}

#[invariant(self.closure.function().chunk().code().len() >= self.ip, "Invalid IP")]
impl CallFrame {
    pub fn new(closure: Rc<Closure>, start: usize, prev_stack_size: usize) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self {
            closure,
            ip: 0,
            start,
            id: COUNTER.fetch_add(1, Ordering::Relaxed),
            ret_pos: 0,
            prev_stack_size,
        }
    }

    #[cfg(test)]
    pub fn print_curr_instr(&self) {
        let instr = &self.closure.function().chunk().code()[self.ip];
        println!("{} {:?}", self.ip, instr);
    }

    #[inline]
    pub fn curr_instr(&mut self) -> Instruction {
        let instr = self.closure.function().chunk().code()[self.ip];
        self.ip += 1;
        instr
    }

    #[inline]
    pub fn skip_instr(&mut self) {
        self.ip += 1;
    }

    #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
    pub fn rel_jmp(&mut self, offset: i16) {
        self.ip = (self.ip as isize + offset as isize) as usize;
    }

    pub fn read_number(&self, c: NumberHandle) -> f64 {
        self.closure.function().chunk().read_number(c)
    }

    pub fn read_string(&self, c: StringHandle) -> RuaString {
        self.closure.function().chunk().read_string(c)
    }

    pub fn read_function(&self, c: FnHandle) -> Function {
        self.closure.function().chunk().read_function(c)
    }

    #[inline]
    pub const fn stack_start(&self) -> usize {
        self.start
    }

    pub const fn id(&self) -> usize {
        self.id
    }

    pub fn func_name(&self) -> Rc<str> {
        self.closure.function().pretty_name()
    }

    pub fn curr_line(&self) -> usize {
        self.closure.function().chunk().line_at(self.ip)
    }

    pub fn closure(&self) -> &Rc<Closure> {
        &self.closure
    }

    pub fn set_ret_pos(&mut self, ret_pos: u8) {
        self.ret_pos = ret_pos
    }

    pub fn ret_pos(&self) -> u8 {
        self.ret_pos
    }

    pub fn prev_stack_size(&self) -> usize {
        self.prev_stack_size
    }

    #[inline]
    pub fn resolve_reg(&self, reg: u8) -> usize {
        self.stack_start() + reg as usize
    }
}

impl Debug for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Callframe with ip {}, starting at stack position {}, at function {}",
            self.ip,
            self.start,
            self.closure.function().pretty_name()
        )
    }
}
