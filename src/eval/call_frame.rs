use crate::compiler::bytecode::{Constant, Instruction};

use super::vals::{function::Function, RuaVal};

#[derive(Debug)]
pub struct CallFrame {
    function: Function,
    ip: usize,
    start: usize,
}

impl CallFrame {
    pub fn new(function: Function, start: usize) -> Self {
        Self { function, ip: 0, start }
    }

    #[cfg(test)]
    pub fn print_curr_instr(&self) {
        let instr = &self.function.chunk().code()[self.ip];
        println!("{} {:?}", self.ip, instr);
    }

    pub fn curr_instr(&mut self) -> Instruction {
        let instr = self.function.chunk().code()[self.ip].clone();
        self.ip += 1;
        instr
    }

    pub fn rel_jmp(&mut self, offset: isize) {
        self.ip = (self.ip as isize + offset) as usize
    }

    pub fn read_constant(&self, c: Constant) -> RuaVal {
        self.function.chunk().read_constant(c)
    }

    pub fn stack_start(&self) -> usize {
        self.start
    }
}
