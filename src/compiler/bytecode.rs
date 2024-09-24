use std::fmt::Debug;
use struct_invariant::invariant;
use thiserror::Error;

use crate::{
    eval::vals::{function::Function, string::RuaString},
    lex::tokens::{Token, TokenType},
};

use super::upvalues::{UpvalueDesc, UpvalueHandle, Upvalues};

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

    AddVV(BinArgs),
    SubVV(BinArgs),
    MulVV(BinArgs),
    DivVV(BinArgs),
    ModVV(BinArgs),
    PowVV(BinArgs),
    StrConcat(BinArgs),

    AddVN(VNArgs),
    SubVN(VNArgs),
    MulVN(VNArgs),
    DivVN(VNArgs),
    ModVN(VNArgs),
    PowVN(VNArgs),

    SubNV(NVArgs),
    DivNV(NVArgs),
    ModNV(NVArgs),
    PowNV(NVArgs),

    EqVV(VVJmpArgs),
    NeqVV(VVJmpArgs),
    LtVV(VVJmpArgs),
    LeVV(VVJmpArgs),

    EqVN { lhs: u8, rhs: NumberHandle },
    NeqVN { lhs: u8, rhs: NumberHandle },
    LtVN { lhs: u8, rhs: NumberHandle },
    LeVN { lhs: u8, rhs: NumberHandle },

    EqNV { lhs: NumberHandle, rhs: u8 },
    NeqNV { lhs: NumberHandle, rhs: u8 },
    LtNV { lhs: NumberHandle, rhs: u8 },
    LeNV { lhs: NumberHandle, rhs: u8 },

    Test { src: u8 },
    Untest { src: u8 },
    TestSet { dst: u8, src: u8 },
    UntestSet { dst: u8, src: u8 },

    GetGlobal { dst: u8, src: StringHandle },
    SetGlobal { dst: StringHandle, src: u8 },

    Call { base: u8, nargs: u8 },
    TailCall { base: u8, nargs: u8 },

    Mv(UnArgs),

    Jmp(i16), // TODO i24?
    ForPrep { from: u8, offset: u16 },
    ForLoop { from: u8, offset: u16 },

    NewTable { dst: u8, capacity: u16 },
    InsertV { table: u8, key: u8, val: u8 },
    InsertS { table: u8, key: u8, val: u8 },
    InsertN { table: u8, key: u8, val: u8 },
    IndexV(BinArgs),
    IndexS(BinArgs),
    IndexN(BinArgs),

    Closure { dst: u8, src: FnHandle },
    Upvalue(UpvalueDesc),
    CloseUpvalues { from: u8, to: u8 },
    GetUpvalue { dst: u8, src: UpvalueHandle },
    SetUpvalue { dst: UpvalueHandle, src: u8 },
}

#[cfg(target_arch = "x86_64")]
static_assertions::assert_eq_size!(Instruction, [u8; 4]);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct BinArgs {
    pub(crate) dst: u8,
    pub(crate) lhs: u8,
    pub(crate) rhs: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct NVArgs {
    pub(crate) dst: u8,
    pub(crate) lhs: u8,
    pub(crate) rhs: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct VNArgs {
    pub(crate) dst: u8,
    pub(crate) lhs: u8,
    pub(crate) rhs: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct VVJmpArgs {
    pub(crate) lhs: u8,
    pub(crate) rhs: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct UnArgs {
    pub(crate) dst: u8,
    pub(crate) src: u8,
}

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

    #[must_use]
    #[inline]
    pub(crate) fn read_number(&self, c: NumberHandle) -> f64 {
        *self.numbers.get(c.0 as usize).expect("Invalid constant")
    }

    #[must_use]
    pub(crate) fn read_string(&self, c: StringHandle) -> RuaString {
        self.strings.get(c.0 as usize).expect("Invalid constant").clone()
    }

    #[must_use]
    pub(crate) fn read_function(&self, c: FnHandle) -> Function {
        self.functions.get(c.0 as usize).expect("Invalid constant").clone()
    }

    #[must_use]
    #[inline]
    pub(crate) const fn code(&self) -> &Vec<Instruction> {
        &self.code
    }

    pub(super) fn add_closure(
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

    pub(super) fn add_number(
        &mut self,
        dst: u8,
        val: f64,
        line: usize,
    ) -> Result<usize, ParseError> {
        let c = add_constant!(val, self.numbers, NumberHandle);
        Ok(self.add_instruction(Instruction::Number { dst, src: c }, line))
    }

    pub(super) fn new_number_constant(&mut self, val: f64) -> Result<NumberHandle, ParseError> {
        Ok(add_constant!(val, self.numbers, NumberHandle))
    }

    pub(super) fn add_string(
        &mut self,
        dst: u8,
        val: RuaString,
        line: usize,
    ) -> Result<usize, ParseError> {
        let c = add_constant!(val, self.strings, StringHandle);
        Ok(self.add_instruction(Instruction::String { dst, src: c }, line))
    }

    pub(super) fn new_string_constant(
        &mut self,
        val: RuaString,
    ) -> Result<StringHandle, ParseError> {
        Ok(add_constant!(val, self.strings, StringHandle))
    }

    pub(super) fn add_instruction(&mut self, instr: Instruction, line: usize) -> usize {
        self.code.push(instr);
        let last_line = self.lines.last_mut().expect("lines is always non-empty");
        if last_line.0 == line {
            last_line.1 += 1;
        } else {
            self.lines.push((line, 1));
        }
        self.code.len() - 1
    }

    pub(super) fn replace_instruction(&mut self, idx: usize, instr: Instruction) {
        self.code[idx] = instr;
    }

    pub(super) fn patch_jmp(
        &mut self,
        jmp: usize,
        to: usize,
        line: usize,
    ) -> Result<(), ParseError> {
        debug_assert!(matches!(self.code.get(jmp), Some(Instruction::Jmp(_))));

        let offset = Self::offset(to, jmp, line)?;
        self.code[jmp] = Instruction::Jmp(offset);
        Ok(())
    }

    pub(super) fn negate_cond(&mut self, instr_idx: usize) {
        use Instruction as I;
        let new_instr = match self.code.get(instr_idx) {
            Some(I::EqVV(VVJmpArgs { lhs, rhs })) => I::NeqVV(VVJmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::NeqVV(VVJmpArgs { lhs, rhs })) => I::EqVV(VVJmpArgs { lhs: *lhs, rhs: *rhs }),
            Some(I::LtVV(VVJmpArgs { lhs, rhs })) => I::LeVV(VVJmpArgs { lhs: *rhs, rhs: *lhs }),
            Some(I::LeVV(VVJmpArgs { lhs, rhs })) => I::LtVV(VVJmpArgs { lhs: *rhs, rhs: *lhs }),

            Some(I::EqVN { lhs, rhs }) => I::NeqVN { lhs: *lhs, rhs: *rhs },
            Some(I::NeqVN { lhs, rhs }) => I::EqVN { lhs: *lhs, rhs: *rhs },
            Some(I::LtVN { lhs, rhs }) => I::LeNV { lhs: *rhs, rhs: *lhs },
            Some(I::LeVN { lhs, rhs }) => I::LtNV { lhs: *rhs, rhs: *lhs },

            Some(I::EqNV { lhs, rhs }) => I::NeqNV { lhs: *lhs, rhs: *rhs },
            Some(I::NeqNV { lhs, rhs }) => I::EqNV { lhs: *lhs, rhs: *rhs },
            Some(I::LtNV { lhs, rhs }) => I::LeVN { lhs: *rhs, rhs: *lhs },
            Some(I::LeNV { lhs, rhs }) => I::LtVN { lhs: *rhs, rhs: *lhs },

            Some(I::TestSet { dst, src }) => I::UntestSet { dst: *dst, src: *src },
            Some(I::UntestSet { dst, src }) => I::TestSet { dst: *dst, src: *src },
            i => unreachable!("Tried to negate a non conditional instruction {i:?}"),
        };
        self.code[instr_idx] = new_instr;
    }

    pub(super) fn chg_dst_of(&mut self, instr_idx: usize, dst: u8) {
        self.code[instr_idx].chg_dst(dst)
    }

    pub(super) fn offset(from: usize, to: usize, line: usize) -> Result<i16, ParseError> {
        let offset = isize::try_from(from).or(Err(ParseError::TooManyInstructions))?
            - isize::try_from(to).or(Err(ParseError::TooManyInstructions))?
            - 1;
        offset.try_into().or(Err(ParseError::JmpTooFar(line)))
    }

    #[must_use]
    pub(crate) fn line_at(&self, mut ip: usize) -> usize {
        self.lines
            .iter()
            .find(|(_, n)| {
                ip = ip.saturating_sub(*n);
                ip == 0
            })
            .map_or(0, |(line, _)| *line)
    }

    #[must_use]
    pub(crate) fn nconstants(&self) -> usize {
        self.numbers.len() + self.strings.len()
    }

    #[must_use]
    pub(crate) fn nfunctions(&self) -> usize {
        self.functions.len()
    }

    #[cfg(debug_assertions)]
    pub(super) fn validate_srcs_and_dsts(&self) {
        if let Some(invalid) = self.code.iter().position(|i| !i.has_valid_regs()) {
            panic!("Found instruction with invalid regs (idx {invalid}). Code: {:?}", self.code)
        }
    }
}

impl Instruction {
    fn chg_dst(&mut self, new_dst: u8) {
        match self {
            Self::Number { dst, .. }
            | Self::String { dst, .. }
            | Self::True { dst, .. }
            | Self::False { dst, .. }
            | Self::Nil { dst, .. }
            | Self::LFalseSkip { dst, .. }
            | Self::GetGlobal { dst, .. }
            | Self::NewTable { dst, .. }
            | Self::Closure { dst, .. }
            | Self::GetUpvalue { dst, .. } => *dst = new_dst,
            Self::Neg(i) | Self::Not(i) | Self::Len(i) | Self::Mv(i) => i.dst = new_dst,
            Self::AddVV(i)
            | Self::SubVV(i)
            | Self::MulVV(i)
            | Self::DivVV(i)
            | Self::ModVV(i)
            | Self::PowVV(i)
            | Self::StrConcat(i)
            | Self::IndexV(i)
            | Self::IndexS(i)
            | Self::IndexN(i) => i.dst = new_dst,
            Self::AddVN(i)
            | Self::SubVN(i)
            | Self::MulVN(i)
            | Self::DivVN(i)
            | Self::ModVN(i)
            | Self::PowVN(i) => i.dst = new_dst,
            Self::SubNV(i) | Self::DivNV(i) | Self::ModNV(i) | Self::PowNV(i) => i.dst = new_dst,
            Self::TestSet { dst, src } => {
                if new_dst == *src {
                    *self = Self::Test { src: *src }
                } else {
                    *dst = new_dst;
                }
            }
            Self::UntestSet { dst, src } => {
                if new_dst == *src {
                    *self = Self::Untest { src: *src }
                } else {
                    *dst = new_dst;
                }
            }
            i => unreachable!("Cannot change dst of {i:?}"),
        }
    }

    #[cfg(debug_assertions)]
    const fn has_valid_regs(self) -> bool {
        const fn validate(reg: u8) -> bool {
            reg < 255
        }
        const fn validate_bin(args: BinArgs) -> bool {
            validate(args.dst) && validate(args.lhs) && validate(args.rhs)
        }
        const fn validate_un(args: UnArgs) -> bool {
            validate(args.dst) && validate(args.src)
        }
        const fn validate_jmp_vv(args: VVJmpArgs) -> bool {
            validate(args.lhs) && validate(args.rhs)
        }
        const fn validate_vn(args: VNArgs) -> bool {
            validate(args.dst) && validate(args.lhs)
        }
        const fn validate_nv(args: NVArgs) -> bool {
            validate(args.dst) && validate(args.rhs)
        }
        match self {
            Self::Number { dst, .. }
            | Self::String { dst, .. }
            | Self::True { dst, .. }
            | Self::False { dst, .. }
            | Self::Nil { dst, .. }
            | Self::LFalseSkip { dst, .. }
            | Self::GetGlobal { dst, .. }
            | Self::NewTable { dst, .. }
            | Self::Closure { dst, .. }
            | Self::GetUpvalue { dst, .. } => validate(dst),
            Self::Neg(i) | Self::Not(i) | Self::Len(i) | Self::Mv(i) => validate_un(i),
            Self::AddVV(i)
            | Self::SubVV(i)
            | Self::MulVV(i)
            | Self::DivVV(i)
            | Self::ModVV(i)
            | Self::PowVV(i)
            | Self::StrConcat(i)
            | Self::IndexV(i)
            | Self::IndexS(i)
            | Self::IndexN(i) => validate_bin(i),
            Self::TestSet { dst, src } | Self::UntestSet { dst, src } => {
                validate(dst) && validate(src)
            }
            Self::EqVV(i) | Self::NeqVV(i) | Self::LtVV(i) | Self::LeVV(i) => validate_jmp_vv(i),
            Self::EqVN { lhs, .. }
            | Self::NeqVN { lhs, .. }
            | Self::LtVN { lhs, .. }
            | Self::LeVN { lhs, .. } => validate(lhs),
            Self::EqNV { rhs, .. }
            | Self::NeqNV { rhs, .. }
            | Self::LtNV { rhs, .. }
            | Self::LeNV { rhs, .. } => validate(rhs),
            Self::Return { src }
            | Self::Test { src }
            | Self::Untest { src }
            | Self::SetGlobal { src, .. }
            | Self::SetUpvalue { src, .. } => validate(src),
            Self::Call { base, .. } | Self::TailCall { base, .. } => validate(base),
            Self::InsertV { table, key, val } => validate(table) && validate(key) && validate(val),
            Self::InsertS { table, val, .. } | Self::InsertN { table, val, .. } => {
                validate(table) && validate(val)
            }
            Self::ReturnNil | Self::Jmp(_) | Self::Upvalue(_) => true,
            Self::CloseUpvalues { from, to } => validate(from) && validate(to),
            Self::ForPrep { from, .. } | Self::ForLoop { from, .. } => validate(from),
            Self::AddVN(i)
            | Self::SubVN(i)
            | Self::MulVN(i)
            | Self::DivVN(i)
            | Self::ModVN(i)
            | Self::PowVN(i) => validate_vn(i),
            Self::SubNV(i) | Self::DivNV(i) | Self::ModNV(i) | Self::PowNV(i) => validate_nv(i),
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new(Vec::new(), Vec::new(), Vec::new(), Vec::new(), vec![(0, 0)])
    }
}

impl Chunk {
    pub(crate) fn format_instr(&self, instr: Instruction) -> String {
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
            Instruction::AddVN(VNArgs { rhs: n, .. })
            | Instruction::SubVN(VNArgs { rhs: n, .. })
            | Instruction::MulVN(VNArgs { rhs: n, .. })
            | Instruction::DivVN(VNArgs { rhs: n, .. })
            | Instruction::ModVN(VNArgs { rhs: n, .. })
            | Instruction::PowVN(VNArgs { rhs: n, .. })
            | Instruction::SubNV(NVArgs { lhs: n, .. })
            | Instruction::DivNV(NVArgs { lhs: n, .. })
            | Instruction::ModNV(NVArgs { lhs: n, .. })
            | Instruction::PowNV(NVArgs { lhs: n, .. }) => {
                format!("; {}", self.numbers[n as usize])
            }
            Instruction::InsertN { key, .. } | Instruction::IndexN(BinArgs { rhs: key, .. }) => {
                format!("; {}", self.numbers[key as usize])
            }
            Instruction::InsertS { key, .. } | Instruction::IndexS(BinArgs { rhs: key, .. }) => {
                format!("; \"{}\"", self.strings[key as usize])
            }
            _ => String::new(),
        };
        format!("{instr:?} {clarification}")
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
        for (i, instr) in self.code.iter().copied().enumerate() {
            let same_line = "     |";
            let line_str = if count_in_line == 0 {
                let linenr = line.ok_or(std::fmt::Error)?.0;
                if linenr == 0 && i > 0 {
                    same_line.to_string()
                } else {
                    format!("{linenr: >6}")
                }
            } else {
                same_line.to_string()
            };

            writeln!(f, "{i:4} {line_str}  {}", self.format_instr(instr))?;
            count_in_line += 1;
            if count_in_line >= line.ok_or(std::fmt::Error)?.1 {
                line = lines.next();
                count_in_line = 0;
            }
        }
        for func in &self.functions {
            write!(f, "\n\n{func:?}")?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("Unexpected token. Got {:?}, expected {}{:?}", .0.got, if .0.expected.len() > 1 { "one of " } else { "" }, .0.expected)]
    UnexpectedToken(UnexpectedToken),
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
    #[error("Too many instructions")]
    TooManyInstructions,
    #[error("Table to large near {0}")]
    TableTooLarge(usize),
}

#[derive(Debug, PartialEq)]
pub struct UnexpectedToken {
    pub(super) got: Box<Token>,
    pub(super) expected: Box<[TokenType]>,
}

impl TryInto<u8> for NumberHandle {
    type Error = ();

    #[inline]
    fn try_into(self) -> Result<u8, Self::Error> {
        self.0.try_into().map_err(|_| ())
    }
}

impl NumberHandle {
    #[inline]
    pub(crate) fn from_unchecked(value: u8) -> Self {
        Self(value.into())
    }
}

impl TryInto<u8> for StringHandle {
    type Error = ();

    #[inline]
    fn try_into(self) -> Result<u8, Self::Error> {
        self.0.try_into().map_err(|_| ())
    }
}

impl StringHandle {
    #[inline]
    pub(crate) fn from_unchecked(value: u8) -> Self {
        Self(value.into())
    }
}
