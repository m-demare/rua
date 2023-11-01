use std::fmt::Debug;
use thiserror::Error;

use crate::{
    eval::vals::{string::RuaString, RuaVal},
    lex::tokens::{Token, TokenType},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Return,
    ReturnNil,
    Constant(Constant),
    Neg,
    Add,
    Sub,
    Mul,
    Div,
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
    StrLen,
    StrConcat,
    GetGlobal,
    SetGlobal,
    Call(u16),
    Pop,
    GetLocal(u8),
    SetLocal(u8),
    JmpIfFalse(i32),
    JmpIfTrue(i32),
    Jmp(i32),
    JmpIfFalsePop(i32),
}

#[derive(PartialEq)]
pub struct Program {
    pub(crate) code: Vec<Instruction>,
    pub(super) constants: Vec<RuaVal>,
    pub(super) lines: Vec<(usize, usize)>, // in run-length encoding
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Constant(pub(super) u32);

impl Program {
    pub fn new(code: Vec<Instruction>, constants: Vec<RuaVal>, lines: Vec<(usize, usize)>) -> Self {
        debug_assert!(lines.iter().map(|l| l.1).sum::<usize>() >= code.len());
        Self { code, constants, lines }
    }

    pub fn read_constant(&self, c: Constant) -> RuaVal {
        self.constants.get(c.0 as usize).expect("Invalid constant").clone()
    }
}

impl Debug for Program {
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
        Ok(())
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("Unexpected token. Got {0:?}, expected {}{1:?}", if .1.len() > 1 { "one of " } else { "" })]
    UnexpectedToken(Box<Token>, Box<[TokenType]>),
    #[error("Unexpected token. Got {:?}, expected {1}", .0.ttype)]
    UnexpectedTokenWithErrorMsg(Box<Token>, Box<str>),
    // #[error("Cannot close block of type {0:?} with token {:?}", .1.ttype)]
    // UnexpectedClose(Box<BlockType>, Box<Token>),
    #[error("Function expression cannot have a name (got {0:?})")]
    NamedFunctionExpr(RuaString),
    #[error("Function statement must have a name")]
    UnnamedFunctionSt,
    #[error("Expected statement, got expression")]
    UnexpectedExpression,
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("Only Identifier, FieldAccess or Index exprssions are allowed as assignment LHS")]
    InvalidAssignLHS,
    #[error("Cannot have more than 256 local variables in a given scope")]
    TooManyLocals,
    #[error("Attempted to jmp too far")]
    JmpTooFar,
}
