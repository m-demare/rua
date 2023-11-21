use std::{fmt::Debug, num::TryFromIntError};
use thiserror::Error;

use crate::{
    eval::vals::{function::Function, string::RuaString},
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

    Number(NumberHandle),
    String(StringHandle),
    True,
    False,
    Nil,

    Neg,
    Not,
    Len,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Lt,
    Gt,
    Neq,
    Le,
    Ge,
    StrConcat,

    GetGlobal,
    SetGlobal,

    Call(u8),
    Pop,

    GetLocal(LocalHandle),
    SetLocal(LocalHandle),

    JmpIfFalse(u16),
    JmpIfTrue(u16),
    Jmp(u16),
    Loop(u16),
    JmpIfFalsePop(u16),

    NewTable,
    InsertKeyVal,
    InsertValKey,
    Index,

    Closure(FnHandle),
    Upvalue(Upvalue),
    CloseUpvalue,
    GetUpvalue(UpvalueHandle),
    SetUpvalue(UpvalueHandle),

    Multiassign(u8),

    #[cfg(debug_assertions)]
    CheckStack(u8),
}

#[cfg(target_arch = "x86_64")]
static_assertions::assert_eq_size!(Instruction, [u8; 4]);

#[derive(PartialEq)]
pub struct Chunk {
    code: Vec<Instruction>,
    numbers: Vec<f64>,
    strings: Vec<RuaString>,
    functions: Vec<Function>,
    lines: Vec<(usize, usize)>, // in run-length encoding
}

#[cfg(not(test))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumberHandle(u16);

#[cfg(not(test))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringHandle(u16);

#[cfg(not(test))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FnHandle(u16);

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumberHandle(pub(crate) u16);

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringHandle(pub(crate) u16);

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FnHandle(pub(crate) u16);

macro_rules! add_constant {
    ($val: expr, $vec: expr, $handle: expr, $find_previous: expr) => {{
        #[allow(clippy::float_cmp)]
        let prev = if $find_previous { $vec.iter().position(|c| *c == $val) } else { None };
        if let Some(c) = prev {
            $handle(c.try_into().expect("Constants shouldn't exceed limit"))
        } else {
            let c = $vec.len().try_into().map_err(|_| ParseError::TooManyConstants)?;
            $vec.push($val);
            $handle(c)
        }
    }};
    ($val: expr, $vec: expr, $handle: expr) => {{
        add_constant!($val, $vec, $handle, true)
    }};
}

impl Chunk {
    #[cfg(all(test, debug_assertions))]
    pub fn new(
        code: Vec<Instruction>,
        numbers: Vec<f64>,
        strings: Vec<RuaString>,
        functions: Vec<Function>,
        lines: Vec<(usize, usize)>,
    ) -> Self {
        debug_assert!(lines.iter().map(|l| l.1).sum::<usize>() == code.len());
        Self { code, numbers, strings, functions, lines }
    }

    pub fn read_number(&self, c: NumberHandle) -> f64 {
        *self.numbers.get(c.0 as usize).expect("Invalid constant")
    }

    pub fn read_string(&self, c: StringHandle) -> RuaString {
        self.strings.get(c.0 as usize).expect("Invalid constant").clone()
    }

    pub fn read_function(&self, c: FnHandle) -> Function {
        self.functions.get(c.0 as usize).expect("Invalid constant").clone()
    }

    pub const fn code(&self) -> &Vec<Instruction> {
        &self.code
    }

    pub fn add_closure(
        &mut self,
        func: Function,
        upvalues: Upvalues,
        line: usize,
    ) -> Result<(), ParseError> {
        let c = add_constant!(func, self.functions, FnHandle, false);
        self.add_instruction(Instruction::Closure(c), line);
        for up in upvalues {
            self.add_instruction(Instruction::Upvalue(up), line);
        }
        Ok(())
    }

    pub fn add_number(&mut self, val: f64, line: usize) -> Result<(), ParseError> {
        let c = add_constant!(val, self.numbers, NumberHandle);
        self.add_instruction(Instruction::Number(c), line);
        Ok(())
    }

    pub fn add_string(&mut self, val: RuaString, line: usize) -> Result<(), ParseError> {
        let c = add_constant!(val, self.strings, StringHandle);
        self.add_instruction(Instruction::String(c), line);
        Ok(())
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

    pub fn offset(from: usize, to: usize) -> Result<u16, TryFromIntError> {
        debug_assert!(from >= to);
        let offset = from - to;
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
        Self {
            code: Vec::new(),
            numbers: Vec::new(),
            strings: Vec::new(),
            functions: Vec::new(),
            lines: vec![(0, 0)],
        }
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
        writeln!(f, "Numbers:")?;
        for (i, c) in self.numbers.iter().enumerate() {
            writeln!(f, "{i} {c}")?;
        }
        writeln!(f, "Strings:")?;
        for (i, c) in self.strings.iter().enumerate() {
            writeln!(f, "{i} {c}")?;
        }
        writeln!(f, "Functions:")?;
        for (i, c) in self.functions.iter().enumerate() {
            writeln!(f, "{i} {}", c.pretty_name())?;
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
    #[error("Cannot have more than 2**16 constants in one chunk")]
    TooManyConstants,
    #[error("Attempted to jmp too far (line {0})")]
    JmpTooFar(usize),
    #[error("Too many items in lhs of assignment (line {0})")]
    TooManyAssignLhs(usize),
}
