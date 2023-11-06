use crate::compiler::bytecode::{Constant, Instruction};

use std::{
    fmt::Debug,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use super::vals::{closure::Closure, RuaVal};

pub struct CallFrame {
    closure: Rc<Closure>,
    ip: usize,
    start: usize,
    id: usize,
}

impl CallFrame {
    pub fn new(closure: Rc<Closure>, start: usize) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self { closure, ip: 0, start, id: COUNTER.fetch_add(1, Ordering::Relaxed) }
    }

    #[cfg(test)]
    pub fn print_curr_instr(&self) {
        let instr = &self.closure.function().chunk().code()[self.ip];
        println!("{} {:?}", self.ip, instr);
    }

    pub fn curr_instr(&mut self) -> Instruction {
        let instr = self.closure.function().chunk().code()[self.ip].clone();
        self.ip += 1;
        instr
    }

    pub fn rel_jmp(&mut self, offset: isize) {
        self.ip = (self.ip as isize + offset) as usize;
    }

    pub fn read_constant(&self, c: Constant) -> RuaVal {
        self.closure.function().chunk().read_constant(c)
    }

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

    pub fn closure(&mut self) -> &Rc<Closure> {
        &self.closure
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
