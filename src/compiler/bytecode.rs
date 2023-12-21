use std::{fmt::Debug, num::TryFromIntError};
use struct_invariant::invariant;
use thiserror::Error;

use crate::{
    eval::vals::{function::Function, string::RuaString},
    lex::tokens::{Token, TokenType},
};

use super::upvalues::{Upvalue, UpvalueHandle, Upvalues};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Return { src: u8 },
    ReturnNil,

    Number { dst: u8, src: NumberHandle },
    String { dst: u8, src: StringHandle },
    True { dst: u8 },
    False { dst: u8 },
    Nil { dst: u8 },
    LFalseSkip { dst: u8 },

    Neg(UnArgs),
    Not(UnArgs),
    Len(UnArgs),

    Add(BinArgs),
    Sub(BinArgs),
    Mul(BinArgs),
    Div(BinArgs),
    Mod(BinArgs),
    Pow(BinArgs),
    StrConcat(BinArgs),

    Eq(JmpArgs),
    Neq(JmpArgs),
    Lt(JmpArgs),
    Gt(JmpArgs),
    Le(JmpArgs),
    Ge(JmpArgs),
    Test { src: u8 },
    Untest { src: u8 },
    TestSet { dst: u8, src: u8 },
    UntestSet { dst: u8, src: u8 },

    GetGlobal { dst: u8, src: StringHandle },
    SetGlobal { dst: StringHandle, src: u8 },

    Call { base: u8, nargs: u8 },

    Mv(UnArgs),

    Jmp(i16), // TODO i24?

    NewTable(u16),
    InsertKeyVal,
    InsertValKey,
    Index(BinArgs),

    Closure { dst: u8, src: FnHandle },
    Upvalue(Upvalue),
    CloseUpvalue,
    GetUpvalue(UpvalueHandle),
    SetUpvalue(UpvalueHandle),

    Multiassign(u8),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct BinArgs {
    pub dst: u8,
    pub lhs: u8,
    pub rhs: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct JmpArgs {
    pub lhs: u8,
    pub rhs: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct UnArgs {
    pub dst: u8,
    pub src: u8,
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
            let c = $vec.len().try_into().or(Err(ParseError::TooManyConstants))?;
            $vec.push($val);
            $handle(c)
        }
    }};
    ($val: expr, $vec: expr, $handle: expr) => {{
        add_constant!($val, $vec, $handle, true)
    }};
}

#[invariant(self.lines.iter().map(|l| l.1).sum::<usize>() == self.code.len(), "Every instruction should have a corresponding line")]
#[invariant(!self.lines.is_empty(), "lines cannot be empty")]
impl Chunk {
    pub fn new(
        code: Vec<Instruction>,
        numbers: Vec<f64>,
        strings: Vec<RuaString>,
        functions: Vec<Function>,
        lines: Vec<(usize, usize)>,
    ) -> Self {
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
        dst: u8,
        func: Function,
        upvalues: Upvalues,
        line: usize,
    ) -> Result<(), ParseError> {
        let src = add_constant!(func, self.functions, FnHandle, false);
        self.add_instruction(Instruction::Closure { dst, src }, line);
        for up in upvalues {
            self.add_instruction(Instruction::Upvalue(up), line);
        }
        Ok(())
    }

    pub fn add_number(&mut self, dst: u8, val: f64, line: usize) -> Result<usize, ParseError> {
        let c = add_constant!(val, self.numbers, NumberHandle);
        Ok(self.add_instruction(Instruction::Number { dst, src: c }, line))
    }

    pub fn add_string(
        &mut self,
        dst: u8,
        val: RuaString,
        line: usize,
    ) -> Result<usize, ParseError> {
        let c = add_constant!(val, self.strings, StringHandle);
        Ok(self.add_instruction(Instruction::String { dst, src: c }, line))
    }

    pub fn new_string_constant(&mut self, val: RuaString) -> Result<StringHandle, ParseError> {
        Ok(add_constant!(val, self.strings, StringHandle))
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

    pub(super) fn replace_instruction(&mut self, instr: Instruction, idx: usize) {
        self.code[idx] = instr;
    }

    pub fn pop_instruction(&mut self) -> Option<Instruction> {
        if let Some(instr) = self.code.pop() {
            let last_line = self.lines.last_mut().expect("lines is always non-empty");
            if last_line.1 > 1 {
                last_line.1 -= 1;
            } else if self.lines.len() > 1 {
                self.lines.pop();
            } else {
                self.lines[0] = (0, 0)
            }
            Some(instr)
        } else {
            None
        }
    }

    pub fn patch_jmp(&mut self, jmp: usize, to: usize, line: usize) -> Result<(), ParseError> {
        use Instruction as I;

        let offset = Self::offset(to, jmp).or(Err(ParseError::JmpTooFar(line)))?;

        match self.code.get(jmp) {
            Some(I::Jmp(_)) => self.code[jmp] = I::Jmp(offset),
            i => unreachable!("Tried to patch a non Jmp instruction {i:?}"),
        }

        Ok(())
    }

    pub fn negate_cond(&mut self, instr_idx: usize) {
        use Instruction as I;
        let new_instr = match self.code.get(instr_idx) {
            Some(I::Eq(JmpArgs { lhs, rhs })) => I::Neq(JmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::Neq(JmpArgs { lhs, rhs })) => I::Eq(JmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::Lt(JmpArgs { lhs, rhs })) => I::Ge(JmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::Gt(JmpArgs { lhs, rhs })) => I::Le(JmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::Le(JmpArgs { lhs, rhs })) => I::Gt(JmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::Ge(JmpArgs { lhs, rhs })) => I::Lt(JmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::TestSet { dst, src }) => I::UntestSet { dst: *dst, src: *src },
            Some(I::UntestSet { dst, src }) => I::TestSet { dst: *dst, src: *src },
            i => unreachable!("Tried to negate a non conditional instruction {i:?}"),
        };
        self.code[instr_idx] = new_instr;
    }

    pub fn chg_dst_of(&mut self, instr_idx: usize, dst: u8) {
        self.code[instr_idx].chg_dst(dst)
    }

    pub fn offset(from: usize, to: usize) -> Result<i16, TryFromIntError> {
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

    pub fn nconstants(&self) -> usize {
        self.numbers.len() + self.strings.len()
    }

    pub fn nfunctions(&self) -> usize {
        self.functions.len()
    }

    #[cfg(debug_assertions)]
    pub fn validate_srcs_and_dsts(&self) {
        if let Some(invalid) = self.code.iter().position(|i| !i.has_valid_regs()) {
            panic!("Found instruction with invalid regs (idx {invalid}). Code: {:?}", self.code)
        }
    }
}

impl Instruction {
    fn chg_dst(&mut self, new_dst: u8) {
        match self {
            Instruction::Number { dst, .. } => *dst = new_dst,
            Instruction::String { dst, .. } => *dst = new_dst,
            Instruction::True { dst, .. } => *dst = new_dst,
            Instruction::False { dst, .. } => *dst = new_dst,
            Instruction::Nil { dst, .. } => *dst = new_dst,
            Instruction::LFalseSkip { dst, .. } => *dst = new_dst,
            Instruction::Neg(i) => i.dst = new_dst,
            Instruction::Not(i) => i.dst = new_dst,
            Instruction::Len(i) => i.dst = new_dst,
            Instruction::Add(i) => i.dst = new_dst,
            Instruction::Sub(i) => i.dst = new_dst,
            Instruction::Mul(i) => i.dst = new_dst,
            Instruction::Div(i) => i.dst = new_dst,
            Instruction::Mod(i) => i.dst = new_dst,
            Instruction::Pow(i) => i.dst = new_dst,
            Instruction::StrConcat(i) => i.dst = new_dst,
            Instruction::TestSet { dst, src } => {
                if new_dst == *src {
                    *self = Instruction::Test { src: *src }
                } else {
                    *dst = new_dst
                }
            }
            Instruction::UntestSet { dst, src } => {
                if new_dst == *src {
                    *self = Instruction::Untest { src: *src }
                } else {
                    *dst = new_dst
                }
            }
            Instruction::GetGlobal { dst, .. } => *dst = new_dst,
            Instruction::Mv(i) => i.dst = new_dst,
            Instruction::NewTable(_) => todo!(),
            Instruction::Index(i) => i.dst = new_dst,
            Instruction::Closure { dst, .. } => *dst = new_dst,
            Instruction::GetUpvalue(_) => todo!(),
            i => unreachable!("Cannot change dst of {i:?}"),
        }
    }

    #[cfg(debug_assertions)]
    pub fn has_valid_regs(&self) -> bool {
        fn validate(reg: u8) -> bool {
            reg < 255
        }
        fn validate_bin(args: BinArgs) -> bool {
            validate(args.dst) && validate(args.lhs) && validate(args.rhs)
        }
        fn validate_un(args: UnArgs) -> bool {
            validate(args.dst) && validate(args.src)
        }
        fn validate_jmp(args: JmpArgs) -> bool {
            validate(args.lhs) && validate(args.rhs)
        }
        match self {
            Instruction::Number { dst, .. } => validate(*dst),
            Instruction::String { dst, .. } => validate(*dst),
            Instruction::True { dst, .. } => validate(*dst),
            Instruction::False { dst, .. } => validate(*dst),
            Instruction::Nil { dst, .. } => validate(*dst),
            Instruction::LFalseSkip { dst, .. } => validate(*dst),
            Instruction::Neg(i) => validate_un(*i),
            Instruction::Not(i) => validate_un(*i),
            Instruction::Len(i) => validate_un(*i),
            Instruction::Add(i) => validate_bin(*i),
            Instruction::Sub(i) => validate_bin(*i),
            Instruction::Mul(i) => validate_bin(*i),
            Instruction::Div(i) => validate_bin(*i),
            Instruction::Mod(i) => validate_bin(*i),
            Instruction::Pow(i) => validate_bin(*i),
            Instruction::StrConcat(i) => validate_bin(*i),
            Instruction::TestSet { dst, src } => validate(*dst) && validate(*src),
            Instruction::UntestSet { dst, src } => validate(*dst) && validate(*src),
            Instruction::GetGlobal { dst, .. } => validate(*dst),
            Instruction::SetGlobal { src, .. } => validate(*src),
            Instruction::Mv(i) => validate_un(*i),
            Instruction::NewTable(_) => todo!(),
            Instruction::Index(i) => validate_bin(*i),
            Instruction::Closure { dst, .. } => validate(*dst),
            Instruction::GetUpvalue(_) => todo!(),
            Instruction::Return { src } => validate(*src),
            Instruction::ReturnNil => true,
            Instruction::Eq(i) => validate_jmp(*i),
            Instruction::Neq(i) => validate_jmp(*i),
            Instruction::Lt(i) => validate_jmp(*i),
            Instruction::Gt(i) => validate_jmp(*i),
            Instruction::Le(i) => validate_jmp(*i),
            Instruction::Ge(i) => validate_jmp(*i),
            Instruction::Test { src } => validate(*src),
            Instruction::Untest { src } => validate(*src),
            Instruction::Call { base, .. } => validate(*base),
            Instruction::Jmp(_) => true,
            Instruction::InsertKeyVal => todo!(),
            Instruction::InsertValKey => todo!(),
            Instruction::Upvalue(_) => todo!(),
            Instruction::CloseUpvalue => todo!(),
            Instruction::SetUpvalue(_) => todo!(),
            Instruction::Multiassign(_) => todo!(),
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new(Vec::new(), Vec::new(), Vec::new(), Vec::new(), vec![(0, 0)])
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
        writeln!(f, "\nopnr   line  opcode")?;
        for (i, instr) in self.code.iter().enumerate() {
            let line_str = if count_in_line == 0 {
                let linenr = line.ok_or(std::fmt::Error)?.0;
                format!("{linenr: >6}")
            } else {
                "     |".to_string()
            };
            let clarification = match instr {
                Instruction::Number { src, .. } => format!("; {}", self.numbers[src.0 as usize]),
                Instruction::Closure { src, .. } => {
                    format!("; {}", self.functions[src.0 as usize].pretty_name())
                }
                Instruction::String { src: s, .. }
                | Instruction::GetGlobal { src: s, .. }
                | Instruction::SetGlobal { dst: s, .. } => {
                    format!("; \"{}\"", self.strings[s.0 as usize])
                }
                _ => String::new(),
            };
            writeln!(f, "{i:4} {line_str} {instr:?} {clarification}")?;
            count_in_line += 1;
            if count_in_line >= line.ok_or(std::fmt::Error)?.1 {
                line = lines.next();
                count_in_line = 0;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("Unexpected token. Got {0:?}, expected {}{1:?}", if .1.len() > 1 { "one of " } else { "" })]
    UnexpectedToken(Box<Token>, Box<[TokenType]>),
    #[error("Unexpected token. Got {:?}, expected {1} (line {2})", .0.ttype)]
    UnexpectedTokenWithErrorMsg(Box<Token>, Box<str>, usize),
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
