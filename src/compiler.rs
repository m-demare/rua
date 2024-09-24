use std::ops::BitAnd;

use crate::{
    eval::{
        vals::{closure::Closure, function::Function, string::RuaString, table::try_into_f64},
        Vm,
    },
    lex::{
        tokens::{Token, TokenType as TT},
        Tokenizer,
    },
};

use self::{
    bytecode::{
        Chunk, Instruction as I, Instruction, ParseError, StringHandle, UnArgs, UnexpectedToken,
    },
    expr::{ExprDesc, ExprKind},
    fold::{BinFolder, CommutativeFolder, ComparisonFolder, NonCommutativeFolder, StringFolder},
    locals::{LocalHandle, Locals},
    upvalues::{UpvalueHandle, UpvalueLocation, Upvalues},
    utils::{
        consume, debug_peek_token, match_token, peek_token_is, precedence_of_binary, Precedence,
    },
};

pub mod bytecode;
mod expr;
mod fold;
pub mod locals;
mod tests;
pub mod upvalues;
mod utils;

pub struct Compiler<'vm, T: Iterator<Item = u8> + Clone> {
    tokens: Tokenizer<'vm, T>,
    peeked_token: Option<Token>,
    context: CompilerCtxt,
    context_stack: Vec<CompilerCtxt>,
    last_tok_line: usize,
}

#[allow(unused_parens)]
impl<'vm, T: Iterator<Item = u8> + Clone> Compiler<'vm, T> {
    fn new(mut tokens: Tokenizer<'vm, T>) -> Self {
        let name = tokens.vm().new_string((*b"<main>").into());
        let peeked_token = tokens.next();
        Self {
            tokens,
            context: CompilerCtxt::main(name),
            context_stack: Vec::new(),
            peeked_token,
            last_tok_line: 0,
        }
    }

    fn compile_fn(&mut self) -> Result<(Function, Upvalues), ParseError> {
        self.scoped_block()?;
        self.instruction(I::ReturnNil, 0);
        if let Some(ctxt) = self.context_stack.pop() {
            let fn_ctxt = std::mem::replace(&mut self.context, ctxt);
            #[cfg(debug_assertions)]
            fn_ctxt.chunk.validate_srcs_and_dsts();

            let arity = fn_ctxt.arity;
            let name = fn_ctxt.name;
            let func = Function::new(
                fn_ctxt.chunk,
                arity,
                name,
                fn_ctxt.max_used_regs,
                fn_ctxt.upvalues.len(),
            );
            Ok((func, fn_ctxt.upvalues))
        } else {
            unreachable!("compile_fn must have a parent context")
        }
    }

    /// # Errors
    ///
    /// Will return any parsing error encountered
    pub fn compile(mut self) -> Result<Function, ParseError> {
        self.scoped_block()?;
        self.instruction(I::ReturnNil, 0);

        debug_assert!(self.context_stack.is_empty());
        #[cfg(debug_assertions)]
        self.context.chunk.validate_srcs_and_dsts();

        let arity = self.context.arity;
        let name = self.context.name;
        let func = Function::new(self.context.chunk, arity, name, self.context.max_used_regs, 0);
        Ok(func)
    }

    fn scoped_block(&mut self) -> Result<(), ParseError> {
        self.begin_scope();

        self.block()?;

        self.end_scope();
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.context.locals.begin_scope();
    }

    fn end_scope(&mut self) {
        let mut any_captured = false;
        let to = self.context.locals.len();
        self.context.locals.end_scope(|local| {
            any_captured |= local.is_captured();
        });
        let from = self.context.locals.len();
        if any_captured {
            self.context.chunk.add_instruction(I::CloseUpvalues { from, to }, 0);
        }
    }

    fn block(&mut self) -> Result<(), ParseError> {
        while let Some(token) = self.peek_token() {
            debug_assert_eq!(
                self.context.rh, 0,
                "Should have no registers reserved between statements. Next token was {token:?}"
            );
            match token {
                Token { ttype: TT::RETURN, line, .. } => {
                    let line = *line;
                    self.return_st(line)?;
                }
                Token { ttype: TT::LOCAL, line, .. } => {
                    let line = *line;
                    self.local_st(line)?;
                }
                Token { ttype: TT::IDENTIFIER(..) | TT::LPAREN, line, .. } => {
                    let line = *line;
                    self.assign_or_call_st(line)?;
                }
                Token { ttype: TT::FUNCTION, line, .. } => {
                    let line = *line;
                    self.next_token();
                    self.function_st(line, false)?;
                }
                Token { ttype: TT::END | TT::ELSE | TT::ELSEIF | TT::UNTIL, .. } => break,
                Token { ttype: TT::IF, line, .. } => {
                    let line = *line;
                    self.if_st(line)?;
                }
                Token { ttype: TT::WHILE, line, .. } => {
                    let line = *line;
                    self.while_st(line)?;
                }
                Token { ttype: TT::DO, .. } => self.do_block(true)?,
                Token { ttype: TT::FOR, line, .. } => {
                    let line = *line;
                    self.for_st(line)?;
                }
                Token { ttype: TT::REPEAT, line } => {
                    let line = *line;
                    self.repeat_until_st(line)?;
                }
                Token { ttype: TT::SEMICOLON, .. } => {
                    self.next_token();
                }
                t => {
                    return Err(ParseError::UnexpectedToken(UnexpectedToken {
                        got: Box::new(t.clone()),
                        expected: Box::new([
                            TT::LOCAL,
                            TT::RETURN,
                            TT::SEMICOLON,
                            TT::IDENTIFIER_DUMMY,
                            TT::IF,
                            TT::WHILE,
                            TT::END,
                            TT::ELSE,
                            TT::ELSEIF,
                            TT::REPEAT,
                            TT::UNTIL,
                        ]),
                    }))
                }
            }
        }
        debug_assert_eq!(self.context.rh, 0, "Should have no registers reserved at end of block");
        Ok(())
    }

    fn expression(&mut self, precedence: Precedence) -> Result<ExprDesc, ParseError> {
        let prefix_desc = self.prefix_exp()?;
        self.infix_exp(prefix_desc, precedence)
    }

    fn group_expression(&mut self) -> Result<ExprDesc, ParseError> {
        let desc = self.expression(Precedence::Lowest)?;
        consume!(self; (TT::RPAREN));
        Ok(desc)
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.context.chunk
    }

    const fn current_chunk(&self) -> &Chunk {
        &self.context.chunk
    }

    fn emit_number(&mut self, dst: u8, val: f64, line: usize) -> Result<usize, ParseError> {
        self.current_chunk_mut().add_number(dst, val, line)
    }

    fn emit_string(&mut self, dst: u8, val: RuaString, line: usize) -> Result<usize, ParseError> {
        self.current_chunk_mut().add_string(dst, val, line)
    }

    fn declare_local(&mut self, s: RuaString) -> Result<LocalHandle, ParseError> {
        let local = self.context.locals.declare(s)?;
        self.context.max_used_regs = self.context.max_used_regs.max(
            (u16::from(self.context.rh) + u16::from(self.context.locals.len()))
                .try_into()
                .map_err(|_| ParseError::TooManyLocals)?,
        );
        Ok(local)
    }

    const MAX_TMPS: u8 = 250;
    fn tmp(&mut self) -> Result<u8, ParseError> {
        let dst = (u16::from(self.context.rh) + u16::from(self.context.locals.len()))
            .try_into()
            .map_err(|_| ParseError::TooManyLocals)?;
        if dst >= Self::MAX_TMPS {
            return Err(ParseError::TooManyLocals); // TODO new error
        }
        self.context.rh += 1;
        self.context.max_used_regs = self.context.max_used_regs.max(dst + 1);
        Ok(dst)
    }

    fn is_tmp(&self, r: u8) -> bool {
        r >= self.context.locals.len()
    }

    fn free_reg(&mut self, r: u8) {
        if self.is_tmp(r) {
            self.context.rh -= 1;
            debug_assert_eq!(
                r,
                self.context.locals.len() + self.context.rh,
                "Can only free last reserved reg (tried to free {r}). Next token was {:?}, generated code was {:?}",
                self.peek_token(),
                self.current_chunk()
            );
        }
    }

    fn free2reg(&mut self, mut r1: u8, mut r2: u8) {
        if r1 < r2 {
            std::mem::swap(&mut r1, &mut r2);
        }
        self.free_reg(r1);
        self.free_reg(r2);
    }

    fn free_n_reg(&mut self, n: u8) {
        self.context.rh -= n;
    }

    fn new_string_constant(&mut self, val: RuaString) -> Result<StringHandle, ParseError> {
        self.current_chunk_mut().new_string_constant(val)
    }

    fn emit_closure(
        &mut self,
        dst: u8,
        val: Function,
        upvalues: Upvalues,
        line: usize,
    ) -> Result<(), ParseError> {
        self.current_chunk_mut().add_closure(dst, val, upvalues, line)
    }

    fn unary(&mut self, op: &TT, line: usize) -> Result<ExprDesc, ParseError> {
        let inner_desc = self.expression(Precedence::Prefix)?;
        match op {
            TT::NOT => self.not(inner_desc, line),
            TT::LEN => self.len(inner_desc, line),
            TT::MINUS => self.neg(inner_desc, line),
            t => unreachable!("Invalid unary {t:?}"),
        }
    }

    fn neg(&mut self, mut inner_desc: ExprDesc, line: usize) -> Result<ExprDesc, ParseError> {
        if let Some(n) = inner_desc.as_number() {
            Ok(ExprDesc::new(ExprKind::Number(-n)))
        } else {
            let dst = u8::MAX;
            let src = inner_desc.to_any_reg(self)?;
            self.free_reg(src);
            let instr_idx = Some(self.instruction(I::Neg(UnArgs { dst, src }), line));
            Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
        }
    }

    fn not(&mut self, mut inner_desc: ExprDesc, line: usize) -> Result<ExprDesc, ParseError> {
        if let Some(b) = inner_desc.truthyness() {
            Ok(ExprDesc::new(if b { ExprKind::False } else { ExprKind::True }))
        } else {
            let dst = u8::MAX;
            let src = inner_desc.to_any_reg(self)?;
            self.free_reg(src);
            let instr_idx = Some(self.instruction(I::Not(UnArgs { dst, src }), line));
            Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
        }
    }

    #[allow(clippy::cast_precision_loss)]
    fn len(&mut self, mut inner_desc: ExprDesc, line: usize) -> Result<ExprDesc, ParseError> {
        if let Some(s) = inner_desc.as_str() {
            Ok(ExprDesc::new(ExprKind::Number(s.len() as f64)))
        } else {
            let dst = u8::MAX;
            let src = inner_desc.to_any_reg(self)?;
            self.free_reg(src);
            let instr_idx = Some(self.instruction(I::Len(UnArgs { dst, src }), line));
            Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
        }
    }

    fn prefix_exp(&mut self) -> Result<ExprDesc, ParseError> {
        match self.next_token() {
            Some(Token { ttype: t @ (TT::NOT | TT::LEN | TT::MINUS), line, .. }) => {
                self.unary(&t, line)
            }
            Some(Token { ttype: TT::IDENTIFIER(id), .. }) => Ok(self.named_variable(id)),
            Some(Token { ttype: TT::NUMBER(n), .. }) => Ok(ExprDesc::new(ExprKind::Number(n))),
            Some(Token { ttype: TT::STRING(s), .. }) => Ok(ExprDesc::new(ExprKind::String(s))),
            Some(Token { ttype: TT::TRUE, .. }) => Ok(ExprDesc::new(ExprKind::True)),
            Some(Token { ttype: TT::FALSE, .. }) => Ok(ExprDesc::new(ExprKind::False)),
            Some(Token { ttype: TT::NIL, .. }) => Ok(ExprDesc::new(ExprKind::Nil)),
            Some(Token { ttype: TT::LPAREN, .. }) => self.group_expression(),
            Some(Token { ttype: TT::LBRACE, line, .. }) => self.table_literal(line),
            Some(Token { ttype: TT::FUNCTION, .. }) => self.function_expr(),
            Some(t) => {
                let line = t.line;
                Err(ParseError::UnexpectedTokenWithErrorMsg(
                    Box::new(t),
                    "an expression".into(),
                    line,
                ))
            }
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn named_variable(&mut self, id: RuaString) -> ExprDesc {
        if let Some(local) = self.context.resolve_local(&id) {
            ExprDesc::new(ExprKind::Local { reg: local.into() })
        } else if CompilerCtxt::is_upvalue(&id, &mut self.context_stack) {
            ExprDesc::new(ExprKind::Upvalue(id))
        } else {
            ExprDesc::new(ExprKind::Global(id))
        }
    }

    fn instruction(&mut self, instr: Instruction, line: usize) -> usize {
        self.current_chunk_mut().add_instruction(instr, line)
    }

    fn binary<Instr: BinFolder>(
        &mut self,
        mut lhs_desc: ExprDesc,
        i: Instr,
        line: usize,
        precedence: Precedence,
    ) -> Result<ExprDesc, ParseError> {
        if !i.is_foldable(&lhs_desc) {
            lhs_desc.to_any_reg(self)?;
        }
        let rhs_desc = self.expression(precedence)?;

        i.get_res(self, lhs_desc, rhs_desc, line)
    }

    fn cond_jmp(&mut self, i: Instruction, line: usize) -> usize {
        self.instruction(i, line);
        self.instruction(I::Jmp(0), line)
    }

    fn comparison<Instr: BinFolder>(
        &mut self,
        mut lhs_desc: ExprDesc,
        i: Instr,
        line: usize,
        precedence: Precedence,
        swap: bool,
    ) -> Result<ExprDesc, ParseError> {
        if !i.is_foldable(&lhs_desc) {
            lhs_desc.to_any_reg(self)?;
        }

        let mut rhs_desc = self.expression(precedence)?;
        if swap {
            std::mem::swap(&mut lhs_desc, &mut rhs_desc);
        }

        i.get_res(self, lhs_desc, rhs_desc, line)
    }

    #[allow(clippy::too_many_lines, clippy::float_cmp)]
    fn infix_exp(
        &mut self,
        mut lhs_desc: ExprDesc,
        precedence: Precedence,
    ) -> Result<ExprDesc, ParseError> {
        macro_rules! validate_precedence {
            ($t: expr) => {{
                let new_precedence = precedence_of_binary(&$t);
                if precedence >= new_precedence {
                    break Ok(lhs_desc);
                }
                self.next_token();
                new_precedence
            }};
        }
        macro_rules! func {
            ($variant: ident) => {{
                |lhs, rhs| I::$variant { lhs, rhs }
            }};
        }

        loop {
            match self.peek_token() {
                Some(Token { ttype: t @ TT::PLUS, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(
                        lhs_desc,
                        CommutativeFolder::new(|a, b| a + b, I::AddVV, I::AddVN),
                        line,
                        new_precedence,
                    )?;
                }
                Some(Token { ttype: t @ TT::MINUS, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(
                        lhs_desc,
                        NonCommutativeFolder::new(|a, b| a - b, I::SubVV, I::SubVN, I::SubNV),
                        line,
                        new_precedence,
                    )?;
                }
                Some(Token { ttype: t @ TT::TIMES, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(
                        lhs_desc,
                        CommutativeFolder::new(|a, b| a * b, I::MulVV, I::MulVN),
                        line,
                        new_precedence,
                    )?;
                }
                Some(Token { ttype: t @ TT::DIV, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(
                        lhs_desc,
                        NonCommutativeFolder::new(|a, b| a / b, I::DivVV, I::DivVN, I::DivNV),
                        line,
                        new_precedence,
                    )?;
                }
                Some(Token { ttype: t @ TT::MOD, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(
                        lhs_desc,
                        NonCommutativeFolder::new(f64::rem_euclid, I::ModVV, I::ModVN, I::ModNV),
                        line,
                        new_precedence,
                    )?;
                }
                Some(Token { ttype: t @ TT::EXP, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(
                        lhs_desc,
                        NonCommutativeFolder::new(f64::powf, I::PowVV, I::PowVN, I::PowNV),
                        line,
                        new_precedence,
                    )?;
                }
                Some(Token { ttype: t @ TT::DOTDOT, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(
                        lhs_desc,
                        StringFolder::new(|a, b| [a, b].concat().into(), I::StrConcat),
                        line,
                        new_precedence,
                    )?;
                }
                Some(Token { ttype: t @ TT::EQ, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(
                        lhs_desc,
                        ComparisonFolder::new(|a, b| a == b, I::EqVV, func!(EqVN), func!(EqNV)),
                        line,
                        new_precedence,
                        false,
                    )?;
                }
                Some(Token { ttype: t @ TT::NEQ, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(
                        lhs_desc,
                        ComparisonFolder::new(|a, b| a != b, I::NeqVV, func!(NeqVN), func!(NeqNV)),
                        line,
                        new_precedence,
                        false,
                    )?;
                }
                Some(Token { ttype: t @ TT::LT, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(
                        lhs_desc,
                        ComparisonFolder::new(|a, b| a < b, I::LtVV, func!(LtVN), func!(LtNV)),
                        line,
                        new_precedence,
                        false,
                    )?;
                }
                Some(Token { ttype: t @ TT::GT, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(
                        lhs_desc,
                        ComparisonFolder::new(|a, b| a < b, I::LtVV, func!(LtVN), func!(LtNV)),
                        line,
                        new_precedence,
                        true,
                    )?;
                }
                Some(Token { ttype: t @ TT::LE, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(
                        lhs_desc,
                        ComparisonFolder::new(|a, b| a <= b, I::LeVV, func!(LeVN), func!(LeNV)),
                        line,
                        new_precedence,
                        false,
                    )?;
                }
                Some(Token { ttype: t @ TT::GE, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(
                        lhs_desc,
                        ComparisonFolder::new(|a, b| a >= b, I::LeVV, func!(LeVN), func!(LeNV)),
                        line,
                        new_precedence,
                        true,
                    )?;
                }
                Some(Token { ttype: t @ TT::LBRACK, line, .. }) => {
                    let (line, _) = (*line, validate_precedence!(t));
                    let table = lhs_desc.to_any_reg(self)?;
                    let rhs_desc = self.expression(Precedence::Lowest)?;
                    consume!(self; (TT::RBRACK));

                    lhs_desc = self.index(table, rhs_desc, line)?;
                }
                Some(Token { ttype: t @ TT::AND, line, .. }) => {
                    let (line, _new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.and(lhs_desc, line)?;
                }
                Some(Token { ttype: t @ TT::OR, line, .. }) => {
                    let (line, _new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.or(lhs_desc, line)?;
                }
                Some(Token { ttype: TT::LPAREN | TT::STRING(_) | TT::LBRACE, line, .. }) => {
                    if precedence < Precedence::Call {
                        lhs_desc = self.call_expr(lhs_desc, *line)?;
                    } else {
                        break Ok(lhs_desc);
                    }
                }
                Some(Token { ttype: t @ TT::DOT, line, .. }) => {
                    let (line, _) = (*line, validate_precedence!(t));
                    let id = self.identifier()?;
                    let table = lhs_desc.to_any_reg(self)?;
                    lhs_desc = self.index_string(table, id, line)?;
                }
                _ => break Ok(lhs_desc),
            }
        }
    }

    fn index(
        &mut self,
        table: u8,
        mut key_desc: ExprDesc,
        line: usize,
    ) -> Result<ExprDesc, ParseError> {
        match key_desc.kind {
            ExprKind::String(key) => self.index_string(table, key, line),
            ExprKind::Number(key) => self.index_number(table, key, line),
            _ => {
                let key = key_desc.to_any_reg(self)?;
                Ok(ExprDesc::new(ExprKind::IndexV { table, key, line }))
            }
        }
    }

    fn index_string(
        &mut self,
        table: u8,
        key: RuaString,
        line: usize,
    ) -> Result<ExprDesc, ParseError> {
        let handle = self.current_chunk_mut().new_string_constant(key.clone())?.try_into();
        if let Ok(handle) = handle {
            Ok(ExprDesc::new(ExprKind::IndexS { table, key: handle, line }))
        } else {
            // Constant handle is too big,
            // discharge constant to reg and use VV operation
            let mut key_desc = ExprDesc::new(ExprKind::String(key));
            key_desc.to_any_reg(self)?;
            self.index(table, key_desc, line)
        }
    }

    fn index_number(&mut self, table: u8, key: f64, line: usize) -> Result<ExprDesc, ParseError> {
        let handle = self.current_chunk_mut().new_number_constant(key)?.try_into();
        if let Ok(handle) = handle {
            Ok(ExprDesc::new(ExprKind::IndexN { table, key: handle, line }))
        } else {
            // Constant handle is too big,
            // discharge constant to reg and use VV operation
            let mut key_desc = ExprDesc::new(ExprKind::Number(key));
            key_desc.to_any_reg(self)?;
            self.index(table, key_desc, line)
        }
    }

    fn return_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::RETURN);
        self.next_token();

        if peek_token_is!(self, TT::END | TT::ELSE | TT::ELSEIF | TT::UNTIL | TT::SEMICOLON)
            || self.peek_token().is_none()
        {
            self.instruction(I::ReturnNil, line);
            return Ok(());
        }

        let mut desc = self.expression(Precedence::Lowest)?;
        match_token!(self, TT::SEMICOLON);
        match self.peek_token() {
            Some(Token { ttype: TT::END | TT::ELSE | TT::ELSEIF | TT::UNTIL, .. }) | None => {}
            Some(t) => {
                return Err(ParseError::UnexpectedToken(UnexpectedToken {
                    got: t.clone().into(),
                    expected: [TT::END, TT::ELSE, TT::ELSEIF, TT::UNTIL].into(),
                }))
            }
        }
        let src = desc.to_any_reg(self)?;

        match self.current_chunk().code().last() {
            Some(I::Call { base, nargs })
                if matches!(desc, ExprDesc { kind: ExprKind::Tmp { .. }, .. }) =>
            {
                let (base, nargs) = (*base, *nargs);
                let pos = self.current_chunk().code().len() - 1;
                self.current_chunk_mut().replace_instruction(pos, I::TailCall { base, nargs });
            }
            _ => {
                self.instruction(I::Return { src }, line);
            }
        }

        self.free_reg(src);
        Ok(())
    }

    fn local_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::LOCAL);
        self.next_token();

        let mut locals = match self.next_token() {
            Some(Token { ttype: TT::FUNCTION, line, .. }) => return self.function_st(line, true),
            Some(Token { ttype: TT::IDENTIFIER(id), .. }) => vec![id],
            Some(t) => {
                return Err(ParseError::UnexpectedToken(UnexpectedToken {
                    got: t.into(),
                    expected: [TT::IDENTIFIER_DUMMY, TT::FUNCTION].into(),
                }))
            }
            None => return Err(ParseError::UnexpectedEOF),
        };

        while match_token!(self, TT::COMMA) {
            locals.push(self.identifier()?);
        }
        let mut locals = locals.into_iter();
        if match_token!(self, TT::ASSIGN) {
            while {
                let mut val = self.expression(Precedence::Lowest)?;
                val.free_tmp_regs(self);
                if let Some(local) = locals.next() {
                    val.free_reg(self);

                    let handle = self.declare_local(local)?;

                    val.to_reg(self, handle.into())?;
                } else {
                    val.to_any_reg(self)?;
                    val.free_reg(self);
                }
                debug_assert_eq!(
                    self.context.rh, 0,
                    "Everything should be freed after local declaration"
                );
                match_token!(self, TT::COMMA)
            } {}
        }
        for local in locals {
            let handle = self.declare_local(local)?;
            self.instruction(Instruction::Nil { dst: handle.into() }, line);
        }

        Ok(())
    }

    fn match_lhs_rhs(&mut self, dsts: &[ExprDesc], srcs: &mut Vec<ExprDesc>) {
        while srcs.len() > dsts.len() {
            let src = srcs.pop().expect("Not empty (more srcs than dsts)");
            src.free_tmp_regs(self);
            src.free_reg(self);
        }
        while dsts.len() > srcs.len() {
            srcs.push(ExprDesc::new(ExprKind::Nil));
        }
    }

    fn identifier(&mut self) -> Result<RuaString, ParseError> {
        match self.next_token() {
            Some(Token { ttype: TT::IDENTIFIER(id), .. }) => Ok(id),
            Some(t) => Err(ParseError::UnexpectedToken(UnexpectedToken {
                got: t.into(),
                expected: [TT::IDENTIFIER_DUMMY].into(),
            })),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    const fn peek_token(&self) -> Option<&Token> {
        self.peeked_token.as_ref()
    }

    fn next_token(&mut self) -> Option<Token> {
        self.last_tok_line = self.peeked_token.as_ref().map_or(0, |t| t.line);
        std::mem::replace(&mut self.peeked_token, self.tokens.next())
    }

    fn assign_or_call_st(&mut self, line: usize) -> Result<(), ParseError> {
        let dst = self.expression(Precedence::Lowest)?;
        match self.peek_token() {
            Some(Token { ttype: TT::ASSIGN, line, .. }) => {
                let line = *line;
                self.next_token();
                let val = self.expression(Precedence::Lowest)?;
                self.assign(&dst, val, line)?;
                dst.free_tmp_regs(self);
            }
            Some(Token { ttype: TT::COMMA, line, .. }) => {
                let line = *line;
                return self.multiassign(dst, line);
            }
            _ => match self.current_chunk().code().last() {
                Some(I::Call { .. }) => {
                    dst.free_reg(self);
                }
                _ => return Err(ParseError::UnexpectedExpression(line)),
            },
        }

        Ok(())
    }

    fn assign(&mut self, dst: &ExprDesc, mut src: ExprDesc, line: usize) -> Result<(), ParseError> {
        match dst.kind {
            ExprKind::Local { reg, .. } => {
                src.free_tmp_regs(self);
                src.free_reg(self);
                src.to_reg(self, reg)?;
            }
            ExprKind::Global(ref id) => {
                let src = src.to_any_reg(self)?;
                let dst = self.new_string_constant(id.clone())?;
                self.instruction(I::SetGlobal { dst, src }, line);
                self.free_reg(src);
            }
            ExprKind::Upvalue(ref id) => {
                let src = src.to_any_reg(self)?;
                let dst = self
                    .context
                    .resolve_upvalue(id, &mut self.context_stack)?
                    .expect("Got invalid upvalue");
                self.instruction(I::SetUpvalue { dst, src }, line);
                self.free_reg(src);
            }
            ExprKind::IndexV { table, key, line } => {
                let val = src.to_any_reg(self)?;
                self.instruction(I::InsertV { table, key, val }, line);
                self.free_reg(val);
            }
            ExprKind::IndexS { table, key, line } => {
                let val = src.to_any_reg(self)?;
                self.instruction(I::InsertS { table, key, val }, line);
                self.free_reg(val);
            }
            ExprKind::IndexN { table, key, line } => {
                let val = src.to_any_reg(self)?;
                self.instruction(I::InsertN { table, key, val }, line);
                self.free_reg(val);
            }
            _ => return Err(ParseError::InvalidAssignLHS(line)),
        };
        Ok(())
    }

    fn multiassign(&mut self, first_dst: ExprDesc, line: usize) -> Result<(), ParseError> {
        let mut dsts = vec![first_dst];
        let mut srcs = Vec::new();
        while match_token!(self, TT::COMMA) {
            dsts.push(self.expression(Precedence::Lowest)?);
        }
        consume!(self; (TT::ASSIGN));
        while {
            let mut src = self.expression(Precedence::Lowest)?;
            src.free_reg(self);
            src.to_next_reg(self)?;
            srcs.push(src);
            match_token!(self, TT::COMMA)
        } {}

        self.match_lhs_rhs(&dsts, &mut srcs);

        for (dst, src) in dsts.iter().zip(srcs.into_iter()).rev() {
            self.assign(dst, src, line)?;
        }

        dsts.iter().rev().for_each(|d| d.free_tmp_regs(self));

        Ok(())
    }

    fn call_expr(&mut self, mut lhs_desc: ExprDesc, line: usize) -> Result<ExprDesc, ParseError> {
        debug_peek_token!(self, TT::LPAREN | TT::STRING(_) | TT::LBRACE);
        lhs_desc.free_reg(self);
        let base = lhs_desc.to_next_reg(self)?;
        let rh = self.context.rh;
        let cant_args = match self.next_token() {
            Some(Token { ttype: TT::LPAREN, .. }) => self.arg_list()?,
            Some(Token { ttype: TT::STRING(s), line, .. }) => {
                let arg = self.tmp()?;
                self.emit_string(arg, s, line)?;
                1
            }
            Some(Token { ttype: TT::LBRACE, line, .. }) => {
                self.table_literal(line)?;
                1
            }
            _ => unreachable!("Invalid token at call_expr"),
        };

        self.free_n_reg(cant_args);
        debug_assert_eq!(
            self.context.rh, rh,
            "Should have left space only for retval (expected {rh})",
        );
        self.instruction(I::Call { base, nargs: cant_args }, line);
        Ok(ExprDesc::new(ExprKind::Tmp { reg: base, instr_idx: None }))
    }

    fn arg_list(&mut self) -> Result<u8, ParseError> {
        let mut cant_args = 0;
        if match_token!(self, TT::RPAREN) {
            return Ok(cant_args);
        }
        while {
            if cant_args == u8::MAX {
                return Err(ParseError::TooManyArgs);
            }
            cant_args += 1;
            let mut item = self.expression(Precedence::Lowest)?;
            item.free_reg(self);
            item.to_next_reg(self)?;
            match_token!(self, TT::COMMA)
        } {}
        consume!(self; (TT::RPAREN));
        Ok(cant_args)
    }

    fn if_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::IF);
        self.next_token();
        let mut cond = self.expression(Precedence::Lowest)?;
        let mut jmp_idx = cond.skip_if_false(self, line, false)?;
        consume!(self; (TT::THEN));
        self.scoped_block()?;

        let mut else_jmp = None;
        while match_token!(self, TT::ELSEIF) {
            let new_else_jmp = Some(self.jmp(0, line));
            if let Some(jmp_idx) = jmp_idx {
                self.patch_jmp_here(jmp_idx, line)?;
            }
            let mut cond = self.expression(Precedence::Lowest)?;
            jmp_idx = cond.skip_if_false(self, line, false)?;
            consume!(self; (TT::THEN));
            self.scoped_block()?;

            self.patch_list_to_here(else_jmp)?;
            if else_jmp.is_none() {
                else_jmp = new_else_jmp;
            }
        }

        match self.next_token() {
            Some(Token { ttype: TT::END, .. }) => self.patch_list_to_here(jmp_idx)?,
            Some(Token { ttype: TT::ELSE, line: line_else, .. }) => {
                let new_else_jmp = self.jmp(0, line);
                self.patch_list_to_here(jmp_idx)?;
                self.scoped_block()?;

                self.patch_jmp_here(else_jmp.unwrap_or(new_else_jmp), line_else)?;
                consume!(self; (TT::END));
            }
            Some(t) => {
                return Err(ParseError::UnexpectedToken(UnexpectedToken {
                    got: t.into(),
                    expected: [TT::END, TT::ELSE, TT::ELSEIF].into(),
                }))
            }
            None => return Err(ParseError::UnexpectedEOF),
        }

        Ok(())
    }

    fn while_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::WHILE);
        self.next_token();
        let loop_start = self.pc();
        let mut cond = self.expression(Precedence::Lowest)?;
        let exit_jmp = cond.skip_if_false(self, line, false)?;
        self.do_block(true)?;

        self.jmp_to(loop_start, line)?;
        if let Some(exit_jmp) = exit_jmp {
            self.patch_jmp_here(exit_jmp, line)?;
        }

        Ok(())
    }

    fn for_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::FOR);
        self.next_token();
        let empty_str = self.tokens.vm().new_string([].into());

        let id = self.identifier()?;

        let from = self.declare_local(empty_str.clone())?;
        let to = self.declare_local(empty_str.clone())?;
        let step = self.declare_local(empty_str)?;

        consume!(self; (TT::ASSIGN));
        let mut from_desc = self.expression(Precedence::Lowest)?;
        consume!(self; (TT::COMMA));
        let mut to_desc = self.expression(Precedence::Lowest)?;
        let mut step_desc = if match_token!(self, (TT::COMMA)) {
            self.expression(Precedence::Lowest)?
        } else {
            ExprDesc::new(ExprKind::Number(1.0))
        };
        step_desc.free_tmp_regs(self);
        step_desc.free_reg(self);
        step_desc.to_reg(self, step.into())?;
        to_desc.free_tmp_regs(self);
        to_desc.free_reg(self);
        to_desc.to_reg(self, to.into())?;
        from_desc.free_tmp_regs(self);
        from_desc.free_reg(self);
        from_desc.to_reg(self, from.into())?;

        let forprep = self.instruction(I::ForPrep { from: from.into(), offset: u16::MAX }, line);

        self.begin_scope();

        let _it_var = self.declare_local(id)?;

        self.do_block(false)?;

        self.end_scope();

        let for_loop = self.pc();
        self.instruction(
            I::ForLoop {
                from: from.into(),
                offset: (for_loop - forprep).try_into().or(Err(ParseError::JmpTooFar(line)))?,
            },
            line,
        );

        self.current_chunk_mut().replace_instruction(
            forprep,
            I::ForPrep {
                from: from.into(),
                offset: (for_loop - forprep).try_into().or(Err(ParseError::JmpTooFar(line)))?,
            },
        );

        self.context.locals.drop(3);
        Ok(())
    }

    fn repeat_until_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::REPEAT);
        self.next_token();
        let loop_start = self.pc();
        self.scoped_block()?;
        consume!(self; (TT::UNTIL));

        let mut cond = self.expression(Precedence::Lowest)?;
        let exit_jmp = cond.skip_if_false(self, line, false)?;

        if let Some(exit_jmp) = exit_jmp {
            self.patch_jmp(exit_jmp, loop_start, line)?;
        }

        Ok(())
    }

    fn do_block(&mut self, scoped: bool) -> Result<(), ParseError> {
        consume!(self; (TT::DO));
        if scoped {
            self.scoped_block()?;
        } else {
            self.block()?;
        }
        consume!(self; (TT::END));
        Ok(())
    }

    fn jmp(&mut self, offset: i16, line: usize) -> usize {
        self.instruction(I::Jmp(offset), line)
    }

    fn jmp_to(&mut self, to: usize, line: usize) -> Result<usize, ParseError> {
        let pc = self.pc();
        let offset = Chunk::offset(to, pc, line)?;
        Ok(self.instruction(I::Jmp(offset), line))
    }

    fn patch_jmp_here(&mut self, jmp: usize, line: usize) -> Result<(), ParseError> {
        let to = self.pc();
        self.patch_jmp(jmp, to, line)
    }

    fn patch_jmp(&mut self, jmp: usize, to: usize, line: usize) -> Result<(), ParseError> {
        self.current_chunk_mut().patch_jmp(jmp, to, line)
    }

    fn negate_cond(&mut self, instr_idx: usize) {
        self.current_chunk_mut().negate_cond(instr_idx);
    }

    fn get_jmp_dst(&self, jmp_idx: usize) -> Option<usize> {
        let offset: isize = match self.current_chunk().code().get(jmp_idx) {
            Some(Instruction::Jmp(offset)) => (*offset).into(),
            i => unreachable!("non Jmp instruction in jmp list {i:?}"),
        };
        if offset == 0 {
            None
        } else {
            #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
            Some((jmp_idx as isize + offset + 1) as usize)
        }
    }

    fn concat_jmp_lists(
        &mut self,
        l1: &mut Option<usize>,
        l2: Option<usize>,
    ) -> Result<(), ParseError> {
        match (&l1, l2) {
            (_, None) => {}
            (None, Some(_)) => {
                *l1 = l2;
            }
            (Some(l1), Some(l2)) => {
                let mut list = *l1;
                while let Some(dst) = self.get_jmp_dst(list) {
                    list = dst;
                }
                self.patch_jmp(list, l2, 0)?;
            }
        };
        Ok(())
    }

    fn patch_list_to_here(&mut self, list: Option<usize>) -> Result<(), ParseError> {
        let pc = self.pc();
        self.patch_list_aux(list, pc, u8::MAX, Some(pc))
    }

    fn pc(&self) -> usize {
        self.current_chunk().code().len()
    }

    fn patch_list_aux(
        &mut self,
        mut list: Option<usize>,
        vtarget: usize,
        reg: u8,
        dtarget: Option<usize>,
    ) -> Result<(), ParseError> {
        while let Some(l) = list {
            let next = self.get_jmp_dst(l);
            match self.current_chunk().code().get(l - 1) {
                Some(I::TestSet { .. } | I::UntestSet { .. }) => {
                    self.current_chunk_mut().chg_dst_of(l - 1, reg);
                    self.patch_jmp(l, vtarget, self.peek_token().map_or(0, |t| t.line))?;
                }
                _ => {
                    self.patch_jmp(l, dtarget.unwrap_or_else(||
                        panic!("dtarget should've been computed, jmp at {l} needed val. Code: {:?}", self.current_chunk().code())), self.peek_token().map_or(0, |t| t.line))?;
                }
            }
            list = next;
        }
        Ok(())
    }

    fn list_needs_val(&self, mut list: Option<usize>) -> bool {
        while let Some(jmp) = list {
            match self.current_chunk().code().get(jmp - 1) {
                Some(I::TestSet { .. } | I::UntestSet { .. }) => {}
                _ => return true,
            }
            list = self.get_jmp_dst(jmp);
        }
        false
    }

    fn and(&mut self, mut lhs_desc: ExprDesc, line: usize) -> Result<ExprDesc, ParseError> {
        lhs_desc.skip_if_false(self, line, true)?;

        let mut rhs_desc = self.expression(Precedence::And)?;

        debug_assert!(lhs_desc.t_jmp.is_none()); // Should be closed by skip_if_false
        self.concat_jmp_lists(&mut rhs_desc.f_jmp, lhs_desc.f_jmp)?;
        Ok(rhs_desc)
    }

    fn or(&mut self, mut lhs_desc: ExprDesc, line: usize) -> Result<ExprDesc, ParseError> {
        lhs_desc.skip_if_true(self, line)?;

        let mut rhs_desc = self.expression(Precedence::Or)?;

        debug_assert!(lhs_desc.f_jmp.is_none()); // Should be closed by skip_if_true
        self.concat_jmp_lists(&mut rhs_desc.t_jmp, lhs_desc.t_jmp)?;
        Ok(rhs_desc)
    }

    fn function_st(&mut self, line: usize, local: bool) -> Result<(), ParseError> {
        let id = self.identifier().map_or(Err(ParseError::UnnamedFunctionSt(line)), Ok)?;
        let (function, upvalues) = self.function(id.clone())?;

        if local {
            let loc = self.declare_local(id)?;
            self.emit_closure(loc.into(), function, upvalues, line)?;
        } else {
            let id = self.new_string_constant(id)?;
            let src = self.tmp()?;
            self.emit_closure(src, function, upvalues, line)?;
            self.free_reg(src);
            self.instruction(I::SetGlobal { dst: id, src }, line);
        }
        Ok(())
    }

    fn function_expr(&mut self) -> Result<ExprDesc, ParseError> {
        if let Some(Token { ttype: TT::IDENTIFIER(id), line, .. }) = self.peek_token() {
            return Err(ParseError::NamedFunctionExpr(id.clone(), *line));
        }
        let name = self.tokens.vm().new_string([].into());
        let (func, upvals) = self.function(name)?;
        Ok(ExprDesc::new(ExprKind::Function((func, upvals).into())))
    }

    fn function(&mut self, id: RuaString) -> Result<(Function, Upvalues), ParseError> {
        consume!(self; (TT::LPAREN));
        let mut locals = Locals::new();
        let _func = locals.declare(id.clone())?;
        let arity = self.parameter_list(&mut locals)?;

        let old_ctxt =
            std::mem::replace(&mut self.context, CompilerCtxt::for_function(locals, arity, id));
        self.context_stack.push(old_ctxt);
        let retval = self.compile_fn();
        consume!(self; (TT::END));
        retval
    }

    fn parameter_list(&mut self, locals: &mut Locals) -> Result<u8, ParseError> {
        let mut arity = 0;
        if match_token!(self, TT::RPAREN) {
            return Ok(arity);
        }
        while {
            arity += 1;
            locals.declare(self.identifier()?)?;
            match_token!(self, TT::COMMA)
        } {}
        consume!(self; (TT::RPAREN));
        Ok(arity)
    }

    fn table_literal(&mut self, line: usize) -> Result<ExprDesc, ParseError> {
        let mut arr_count = 0usize;
        let mut map_count = 0usize;
        let table = self.tmp()?;
        let table_instr = self.instruction(I::NewTable { dst: table, capacity: 0 }, line);
        loop {
            let desc = match self.peek_token() {
                Some(Token { ttype: TT::LBRACK, .. }) => {
                    self.next_token();
                    let lhs_desc = self.expression(Precedence::Lowest)?;
                    consume!(self; (TT::RBRACK));
                    match self.peek_token() {
                        Some(Token { ttype: TT::ASSIGN, .. }) => lhs_desc,
                        Some(t) => {
                            return Err(ParseError::UnexpectedToken(UnexpectedToken {
                                got: t.clone().into(),
                                expected: [TT::ASSIGN].into(),
                            }))
                        }
                        None => return Err(ParseError::UnexpectedEOF),
                    }
                }
                Some(Token { ttype: TT::RBRACE, .. }) => {
                    break;
                }
                Some(Token { line, .. }) => {
                    let line = *line;
                    let mut desc = self.expression(Precedence::Lowest)?;
                    if peek_token_is!(self, TT::ASSIGN) {
                        desc.kind = match desc.kind {
                            ExprKind::Local { reg, .. } => {
                                let name = self
                                    .context
                                    .locals
                                    .name_of_reg(reg)
                                    .expect("Got non existing local");
                                ExprKind::String(name)
                            }
                            ExprKind::Global(s) | ExprKind::Upvalue(s) | ExprKind::String(s) => {
                                ExprKind::String(s)
                            }
                            _ => return Err(ParseError::InvalidAssignLHS(line)),
                        };
                    }
                    desc
                }
                None => return Err(ParseError::UnexpectedEOF),
            };

            match self.peek_token() {
                Some(Token { ttype: TT::ASSIGN, line, .. }) => {
                    let line = *line;
                    self.next_token();
                    let mut key_desc = desc;
                    map_count += 1;
                    if !matches!(key_desc.kind, ExprKind::Number(_) | ExprKind::String(_)) {
                        key_desc.to_any_reg(self)?;
                    }
                    let rhs_desc = self.expression(Precedence::Lowest)?;
                    let index_desc = self.index(table, key_desc, line)?;
                    rhs_desc.free_tmp_regs(self);
                    self.assign(&index_desc, rhs_desc, line)?;
                    index_desc.free_reg(self);
                    index_desc.free_key_regs(self);
                }
                Some(Token { line, .. }) => {
                    let line = *line;
                    let rhs_desc = desc;
                    arr_count += 1;

                    #[allow(clippy::cast_precision_loss)]
                    let index_desc = self.index_number(
                        table,
                        try_into_f64(arr_count).ok_or(ParseError::TableTooLarge(line))?,
                        line,
                    )?;
                    rhs_desc.free_tmp_regs(self);
                    self.assign(&index_desc, rhs_desc, line)?;
                    index_desc.free_reg(self);
                    index_desc.free_key_regs(self);
                }
                None => {}
            }

            if !match_token!(self, TT::COMMA, TT::SEMICOLON) {
                break;
            }
        }
        consume!(self; (TT::RBRACE));
        if arr_count > 0 {
            arr_count += 1;
        }

        // Capacity: 12 bits for array_capacity, 4 bits for log2(map_capacity)
        let arr_capacity = u16::min(0x0FFF, arr_count.try_into().unwrap_or(u16::MAX));
        let map_capacity: u16 = (if map_count == 1 { 2 } else { map_count })
            .checked_ilog2()
            .unwrap_or(0)
            .try_into()
            .expect("Log cannot be bigger than 32");
        let capacity = (arr_capacity << 4) + map_capacity.bitand(0x000F);
        self.current_chunk_mut()
            .replace_instruction(table_instr, I::NewTable { dst: table, capacity });

        Ok(ExprDesc::new(ExprKind::Tmp { reg: table, instr_idx: None }))
    }
}

struct CompilerCtxt {
    locals: Locals,
    chunk: Chunk,
    upvalues: Upvalues,
    arity: u8,
    name: RuaString,
    max_used_regs: u8,
    rh: u8,
}

impl CompilerCtxt {
    fn main(name: RuaString) -> Self {
        Self {
            locals: Locals::default(),
            chunk: Chunk::default(),
            upvalues: Upvalues::default(),
            arity: 0,
            name,
            max_used_regs: 0,
            rh: 0,
        }
    }

    fn for_function(locals: Locals, arity: u8, name: RuaString) -> Self {
        let max_used_regs = locals.len();
        Self {
            locals,
            chunk: Chunk::default(),
            upvalues: Upvalues::default(),
            arity,
            name,
            max_used_regs,
            rh: 0,
        }
    }

    fn resolve_local(&self, id: &RuaString) -> Option<LocalHandle> {
        self.locals.resolve(id)
    }

    fn is_upvalue(id: &RuaString, context_stack: &mut [Self]) -> bool {
        if let Some((parent, tail)) = context_stack.split_last_mut() {
            parent.resolve_local(id).is_some() || Self::is_upvalue(id, tail)
        } else {
            false
        }
    }

    fn resolve_upvalue(
        &mut self,
        id: &RuaString,
        context_stack: &mut [Self],
    ) -> Result<Option<UpvalueHandle>, ParseError> {
        if let Some((parent, tail)) = context_stack.split_last_mut() {
            if let Some(local) = parent.resolve_local(id) {
                parent.capture(local);
                Some(self.add_upvalue(UpvalueLocation::ParentStack(local))).transpose()
            } else {
                parent
                    .resolve_upvalue(id, tail)?
                    .map(|upvalue| self.add_upvalue(UpvalueLocation::ParentUpval(upvalue)))
                    .transpose()
            }
        } else {
            Ok(None)
        }
    }

    fn add_upvalue(&mut self, upvalue: UpvalueLocation) -> Result<UpvalueHandle, ParseError> {
        self.upvalues.find_or_add(upvalue)
    }

    fn capture(&mut self, local: LocalHandle) {
        self.locals.capture(local);
    }
}

/// # Errors
///
/// Will return any parsing error encountered
pub fn compile<C: Iterator<Item = u8> + Clone>(
    chars: C,
    vm: &mut Vm,
) -> Result<Closure, ParseError> {
    let tokens = Tokenizer::new(chars, vm);
    let compiler = Compiler::new(tokens);
    compiler.compile().map(Closure::new)
}
