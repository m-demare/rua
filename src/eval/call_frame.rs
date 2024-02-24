use crate::compiler::bytecode::{FnHandle, Instruction, NumberHandle, StringHandle};

use std::{
    fmt::Debug,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use super::vals::{closure::Closure, function::Function, string::RuaString};

pub struct CallFrame {
    closure: Rc<Closure>,
    start: usize,
    id: usize,
    retval_dst: u8,
    prev_stack_size: usize,
    ret_ip: usize,
}

impl CallFrame {
    pub fn new(
        closure: Rc<Closure>,
        start: usize,
        prev_stack_size: usize,
        retval_dst: u8,
        ret_ip: usize,
    ) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self {
            closure,
            start,
            id: COUNTER.fetch_add(1, Ordering::Relaxed),
            retval_dst,
            prev_stack_size,
            ret_ip,
        }
    }

    #[cfg(test)]
    pub fn print_instr_at(&self, ip: usize) {
        let chunk = self.closure.function().chunk();
        let instr = chunk.code()[ip];
        println!("{ip} {}", chunk.format_instr(instr));
    }

    #[inline]
    pub fn instr_at(&self, ip: usize) -> Instruction {
        self.closure.function().chunk().code()[ip]
    }

    #[inline]
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

    pub fn line_at(&self, ip: usize) -> usize {
        self.closure.function().chunk().line_at(ip)
    }

    pub const fn closure(&self) -> &Rc<Closure> {
        &self.closure
    }

    pub const fn retval_dst(&self) -> u8 {
        self.retval_dst
    }

    pub const fn prev_stack_size(&self) -> usize {
        self.prev_stack_size
    }

    #[inline]
    pub const fn resolve_reg(&self, reg: u8) -> usize {
        self.stack_start() + reg as usize
    }

    pub const fn ret_ip(&self) -> usize {
        self.ret_ip
    }
}

impl Debug for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Callframe starting at stack position {}, for function {}",
            self.start,
            self.closure.function().pretty_name()
        )
    }
}
