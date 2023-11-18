use std::{fmt::Debug, num::TryFromIntError};
use thiserror::Error;

use crate::{
    eval::vals::{function::Function, string::RuaString, RuaVal},
    lex::tokens::{Token, TokenType},
};

use super::{
    locals::LocalHandle,
    upvalues::{Upvalue, UpvalueHandle, Upvalues},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Return,
    ReturnNil,
    Constant(Constant),
    Closure(Constant),
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    True,
    False,
    Nil,
    Not,
    Eq,
    Lt,
    Gt,
    Neq,
    Le,
    Ge,
    Len,
    StrConcat,
    GetGlobal,
    SetGlobal,
    Call(u8),
    Pop,
    GetLocal(LocalHandle),
    SetLocal(LocalHandle),
    JmpIfFalse(i32),
    JmpIfTrue(i32),
    Jmp(i32),
    JmpIfFalsePop(i32),

    #[cfg(debug_assertions)]
    CheckStack(u8),
    NewTable,
    InsertKeyVal,
    InsertValKey,
    Index,
    GetUpvalue(UpvalueHandle),
    SetUpvalue(UpvalueHandle),
    Upvalue(Upvalue),
    CloseUpvalue,
    Multiassign(u8),
}

#[derive(PartialEq, Eq)]
pub struct Chunk {
    code: Vec<Instruction>,
    constants: Vec<RuaVal>,
    lines: Vec<(usize, usize)>, // in run-length encoding
}

#[cfg(not(test))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Constant(u32);

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Constant(pub(crate) u32);

impl Chunk {
    #[cfg(test)]
    pub fn new(code: Vec<Instruction>, constants: Vec<RuaVal>, lines: Vec<(usize, usize)>) -> Self {
        debug_assert!(lines.iter().map(|l| l.1).sum::<usize>() >= code.len());
        Self { code, constants, lines }
    }

    pub fn read_constant(&self, c: Constant) -> RuaVal {
        self.constants.get(c.0 as usize).expect("Invalid constant").clone()
    }

    pub const fn code(&self) -> &Vec<Instruction> {
        &self.code
    }

    pub fn add_closure(&mut self, func: Function, upvalues: Upvalues, line: usize) {
        let c = Constant(self.constants.len().try_into().expect("Too many constants in one chunk")); // TODO handle gracefully
        self.constants.push(RuaVal::Function(func));
        self.add_instruction(Instruction::Closure(c), line);
        for up in upvalues {
            self.add_instruction(Instruction::Upvalue(up), line);
        }
    }

    pub fn add_constant(&mut self, val: RuaVal, line: usize) -> usize {
        let c = Constant(self.constants.len().try_into().expect("Too many constants in one chunk")); // TODO handle gracefully
        self.constants.push(val); // TODO maybe reuse constants?
        self.add_instruction(Instruction::Constant(c), line)
    }

    pub fn add_instruction(&mut self, instr: Instruction, line: usize) -> usize {
        self.code.push(instr);
        let last_line = self.lines.last_mut().expect("lines is always non-empty");
        if last_line.0 == line {
            last_line.1 += 1;
        } else {
            self.lines.push((line, 1));
        }
        self.code.len() - 1
    }

    pub fn pop_instruction(&mut self) -> Option<Instruction> {
        debug_assert!(!self.code.is_empty() && !self.lines.is_empty());
        let last_line = self.lines.last_mut().expect("lines is always non-empty");
        if last_line.1 > 1 {
            last_line.1 -= 1;
        } else if self.lines.len() > 1 {
            self.lines.pop();
        }
        debug_assert!(!self.lines.is_empty());
        self.code.pop()
    }

    pub fn patch_jmp(&mut self, jmp: usize, line: usize) -> Result<(), ParseError> {
        use Instruction as I;
        debug_assert!(matches!(
            self.code[jmp],
            I::JmpIfTrue(_) | I::JmpIfFalsePop(_) | I::JmpIfFalse(_) | I::Jmp(_)
        ));

        let offset = Self::offset(self.code.len(), jmp).or(Err(ParseError::JmpTooFar(line)))?;

        match self.code.get(jmp) {
            Some(I::JmpIfFalsePop(_)) => self.code[jmp] = I::JmpIfFalsePop(offset),
            Some(I::JmpIfFalse(_)) => self.code[jmp] = I::JmpIfFalse(offset),
            Some(I::JmpIfTrue(_)) => self.code[jmp] = I::JmpIfTrue(offset),
            Some(I::Jmp(_)) => self.code[jmp] = I::Jmp(offset),
            _ => unreachable!("Tried to patch a non Jmp instruction"),
        }

        Ok(())
    }

    pub fn offset(from: usize, to: usize) -> Result<i32, TryFromIntError> {
        let offset = from as isize - to as isize;
        offset.try_into()
    }

    pub fn line_at(&self, mut ip: usize) -> usize {
        self.lines
            .iter()
            .find(|(_, n)| {
                ip = ip.saturating_sub(*n);
                ip == 0
            })
            .map_or(0, |(line, _)| *line)
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self { code: Vec::new(), constants: Vec::new(), lines: vec![(0, 0)] }
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lines = self.lines.iter();
        let mut line = lines.next();
        let mut count_in_line = 0;
        if 0 == line.ok_or(std::fmt::Error)?.1 {
            line = lines.next();
        }
        writeln!(f, "\nopnr  line descr")?;
        for (i, instr) in self.code.iter().enumerate() {
            let line_str = if count_in_line == 0 {
                let linenr = line.ok_or(std::fmt::Error)?.0;
                format!("{linenr: >5}")
            } else {
                "    |".to_string()
            };
            writeln!(f, "{i:4} {line_str} {instr:?}")?;
            count_in_line += 1;
            if count_in_line >= line.ok_or(std::fmt::Error)?.1 {
                line = lines.next();
                count_in_line = 0;
            }
        }
        writeln!(f, "Constants:")?;
        for (i, c) in self.constants.iter().enumerate() {
            writeln!(f, "{i} {c}")?;
        }
        writeln!(f, "Lines:")?;
        writeln!(f, "{:?}", self.lines)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("Unexpected token. Got {0:?}, expected {}{1:?}", if .1.len() > 1 { "one of " } else { "" })]
    UnexpectedToken(Box<Token>, Box<[TokenType]>),
    #[error("Unexpected token. Got {:?}, expected {1}", .0.ttype)]
    UnexpectedTokenWithErrorMsg(Box<Token>, Box<str>),
    #[error("Function expression cannot have a name (got {0:?} at line {1})")]
    NamedFunctionExpr(RuaString, usize),
    #[error("Function statement must have a name (line {0})")]
    UnnamedFunctionSt(usize),
    #[error("Expected statement, got expression (line {0})")]
    UnexpectedExpression(usize),
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error(
        "Only Identifier, FieldAccess or Index exprssions are allowed as assignment LHS (line {0})"
    )]
    InvalidAssignLHS(usize),
    #[error("Cannot have more than 256 local variables in a given scope")]
    TooManyLocals,
    #[error("Cannot pass more than 256 arguments to a function")]
    TooManyArgs,
    #[error("Attempted to jmp too far (line {0})")]
    JmpTooFar(usize),
    #[error("Too many items in lhs of assignment (line {0})")]
    TooManyAssignLhs(usize),
}
