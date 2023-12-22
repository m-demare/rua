use std::fmt::Debug;

use either::Either::{self, Left, Right};

use crate::{
    compiler::bytecode::BinArgs,
    eval::{
        vals::{closure::Closure, function::Function, string::RuaString},
        Vm,
    },
    lex::{
        tokens::{Token, TokenType as TT},
        Tokenizer,
    },
};

use self::{
    bytecode::{
        Chunk, FnHandle, Instruction as I, Instruction, JmpArgs, ParseError, StringHandle, UnArgs,
    },
    locals::{LocalHandle, Locals},
    upvalues::{UpvalueHandle, Upvalues},
    utils::{consume, debug_peek_token, match_token, peek_token_is},
};

pub mod bytecode;
pub mod locals;
mod tests;
pub mod upvalues;
mod utils;

pub struct Compiler<'vm, T: Iterator<Item = u8> + Clone> {
    tokens: Tokenizer<'vm, T>,
    peeked_token: Option<Token>,
    context: CompilerCtxt,
    context_stack: Vec<CompilerCtxt>,
}

#[allow(unused_parens)]
impl<'vm, T: Iterator<Item = u8> + Clone> Compiler<'vm, T> {
    fn new(mut tokens: Tokenizer<'vm, T>) -> Self {
        let name = tokens.vm().new_string((*b"<main>").into());
        let peeked_token = tokens.next();
        Self { tokens, context: CompilerCtxt::main(name), context_stack: Vec::new(), peeked_token }
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
        self.context.locals.end_scope(|local| {
            if local.is_captured() {
                self.context.chunk.add_instruction(I::CloseUpvalue, 0);
            } else {
                // self.context.chunk.add_instruction(I::Pop, 0);
            }
        });
    }

    fn block(&mut self) -> Result<(), ParseError> {
        while let Some(token) = self.peek_token() {
            debug_assert_eq!(self.context.rh, 0);
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
                Token { ttype: TT::END | TT::ELSE | TT::ELSEIF, .. } => break,
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
                    // self.for_st(line)?;
                }
                Token { ttype: TT::SEMICOLON, .. } => {
                    self.next_token();
                }
                t => {
                    return Err(ParseError::UnexpectedToken(
                        Box::new(t.clone()),
                        Box::new([
                            TT::LOCAL,
                            TT::RETURN,
                            TT::SEMICOLON,
                            TT::IDENTIFIER_DUMMY,
                            TT::IF,
                            TT::WHILE,
                            TT::END,
                            TT::ELSE,
                            TT::ELSEIF,
                        ]),
                    ))
                }
            }
        }
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

    fn current_chunk(&self) -> &Chunk {
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
            (self.context.rh as u16 + self.context.locals.len() as u16)
                .try_into()
                .map_err(|_| ParseError::TooManyLocals)?,
        );
        Ok(local)
    }

    const MAX_TMPS: u8 = 250;
    fn tmp(&mut self) -> Result<u8, ParseError> {
        let dst = (self.context.rh as u16 + self.context.locals.len() as u16)
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
            debug_assert_eq!(r, self.context.locals.len() + self.context.rh);
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

    fn unary(&mut self, op: TT, line: usize) -> Result<ExprDesc, ParseError> {
        let mut inner_desc = self.expression(Precedence::Prefix)?;
        let src = inner_desc.to_any_reg(self)?;
        self.free_reg(src);
        let dst = u8::MAX;
        let instr_idx = match op {
            TT::NOT => self.instruction(I::Not(UnArgs { dst, src }), line),
            TT::LEN => self.instruction(I::Len(UnArgs { dst, src }), line),
            TT::MINUS => self.instruction(I::Neg(UnArgs { dst, src }), line),
            _ => unreachable!("Invalid unary"),
        };
        let instr_idx = Some(instr_idx);
        Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
    }

    fn prefix_exp(&mut self) -> Result<ExprDesc, ParseError> {
        match self.next_token() {
            Some(Token { ttype: t @ (TT::NOT | TT::LEN | TT::MINUS), line, .. }) => {
                self.unary(t, line)
            }
            Some(Token { ttype: TT::IDENTIFIER(id), line, .. }) => self.named_variable(id, line),
            Some(Token { ttype: TT::NUMBER(n), .. }) => Ok(ExprDesc::new(ExprKind::Number(n))),
            Some(Token { ttype: TT::STRING(s), .. }) => Ok(ExprDesc::new(ExprKind::String(s))),
            Some(Token { ttype: TT::TRUE, .. }) => Ok(ExprDesc::new(ExprKind::True)),
            Some(Token { ttype: TT::FALSE, .. }) => Ok(ExprDesc::new(ExprKind::False)),
            Some(Token { ttype: TT::NIL, .. }) => Ok(ExprDesc::new(ExprKind::Nil)),
            Some(Token { ttype: TT::LPAREN, .. }) => self.group_expression(),
            Some(Token { ttype: TT::LBRACE, line, .. }) => self.table_literal(line),
            // Some(Token { ttype: TT::FUNCTION, line, .. }) => return self.function_expr(line),
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

    fn named_variable(&mut self, id: RuaString, line: usize) -> Result<ExprDesc, ParseError> {
        if let Some(local) = self.context.resolve_local(&id) {
            Ok(ExprDesc::new(ExprKind::Local { reg: local.into(), instr_idx: None }))
        } else if let Some(upvalue) = self.context.resolve_upvalue(&id, &mut self.context_stack)? {
            // let dst = self.tmp()?;
            // self.instruction(I::GetUpvalue(upvalue), line);
            // Ok(ExprDesc::new(ExprKind::Local(dst)))
            todo!()
        } else {
            Ok(ExprDesc::new(ExprKind::Global(id)))
        }
    }

    fn instruction(&mut self, instr: Instruction, line: usize) -> usize {
        self.current_chunk_mut().add_instruction(instr, line)
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        self.current_chunk_mut().pop_instruction()
    }

    fn binary<F: FnOnce(BinArgs) -> Instruction>(
        &mut self,
        mut lhs_desc: ExprDesc,
        i: F,
        line: usize,
        precedence: Precedence,
    ) -> Result<ExprDesc, ParseError> {
        let lhs = lhs_desc.to_any_reg(self)?;
        let mut rhs_desc = self.expression(precedence)?;
        let rhs = rhs_desc.to_any_reg(self)?;
        self.free2reg(lhs, rhs);

        let dst = u8::MAX;
        let instr_idx = Some(self.instruction(i(BinArgs { dst, lhs, rhs }), line));

        Ok(ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx }))
    }

    fn cond_jmp(&mut self, i: Instruction, line: usize) -> usize {
        self.instruction(i, line);
        self.instruction(I::Jmp(0), line)
    }

    fn comparison<F: FnOnce(JmpArgs) -> Instruction>(
        &mut self,
        mut lhs_desc: ExprDesc,
        i: F,
        line: usize,
        precedence: Precedence,
    ) -> Result<ExprDesc, ParseError> {
        let lhs = lhs_desc.to_any_reg(self)?;

        let mut rhs_desc = self.expression(precedence)?;
        let rhs = rhs_desc.to_any_reg(self)?;
        self.free2reg(lhs, rhs);

        let instr_idx = self.cond_jmp(i(JmpArgs { lhs, rhs }), line);
        Ok(ExprDesc::new(ExprKind::Jmp { instr_idx }))
    }

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

        loop {
            match self.peek_token() {
                Some(Token { ttype: t @ TT::PLUS, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::Add, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::MINUS, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::Sub, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::TIMES, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::Mul, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::DIV, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::Div, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::MOD, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::Mod, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::EXP, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::Pow, line, new_precedence)?;
                }

                Some(Token { ttype: t @ TT::EQ, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(lhs_desc, I::Eq, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::NEQ, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(lhs_desc, I::Neq, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::LT, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(lhs_desc, I::Lt, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::GT, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(lhs_desc, I::Gt, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::LE, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(lhs_desc, I::Le, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::GE, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.comparison(lhs_desc, I::Ge, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::DOTDOT, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::StrConcat, line, new_precedence)?;
                }
                Some(Token { ttype: t @ TT::LBRACK, line, .. }) => {
                    let (line, new_precedence) = (*line, validate_precedence!(t));
                    lhs_desc = self.binary(lhs_desc, I::Index, line, new_precedence)?;
                    consume!(self; (TT::RBRACK));
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
                Some(Token { ttype: TT::DOT, line, .. }) => {
                    let line = *line;
                    if precedence < Precedence::FieldAccess {
                        self.next_token();
                        let id = self.identifier()?;
                        let lhs = lhs_desc.to_any_reg(self)?;
                        let rhs = self.tmp()?;
                        self.emit_string(rhs, id, line)?;
                        self.free2reg(lhs, rhs);
                        let dst = self.tmp()?;
                        let instr_idx =
                            Some(self.instruction(I::Index(BinArgs { dst, lhs, rhs }), line));
                        lhs_desc = ExprDesc::new(ExprKind::Tmp { reg: dst, instr_idx });
                    } else {
                        break Ok(lhs_desc);
                    }
                }
                _ => break Ok(lhs_desc),
            }
        }
    }

    fn return_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::RETURN);
        self.next_token();

        if peek_token_is!(self, TT::END | TT::ELSE | TT::ELSEIF | TT::SEMICOLON)
            || self.peek_token().is_none()
        {
            self.instruction(I::ReturnNil, line);
            return Ok(());
        }

        let mut desc = self.expression(Precedence::Lowest)?;
        match self.peek_token() {
            Some(Token { ttype: TT::END | TT::ELSE | TT::ELSEIF, .. }) | None => {}
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.clone().into(),
                    [TT::END, TT::ELSE, TT::ELSEIF].into(),
                ))
            }
        }
        let src = desc.to_any_reg(self)?;
        self.instruction(I::Return { src }, line);
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
                return Err(ParseError::UnexpectedToken(
                    t.into(),
                    [TT::IDENTIFIER_DUMMY, TT::FUNCTION].into(),
                ))
            }
            None => return Err(ParseError::UnexpectedEOF),
        };

        while match_token!(self, TT::COMMA) {
            locals.push(self.identifier()?);
        }
        let mut locals = locals.into_iter();
        let mut handles = Vec::new();
        if match_token!(self, TT::ASSIGN) {
            while {
                let dst = if let Some(local) = locals.next() {
                    let handle = self.declare_local(local)?;
                    handles.push(handle);
                    Some(handle.into())
                } else {
                    None
                };
                let mut val = self.expression(Precedence::Lowest)?;
                if let Some(dst) = dst {
                    val.free_reg(self);
                    val.to_reg(self, dst)?;
                }
                debug_assert_eq!(self.context.rh, 0);
                match_token!(self, TT::COMMA)
            } {}
        }
        for local in locals {
            let handle = self.declare_local(local)?;
            handles.push(handle);
            self.instruction(Instruction::Nil { dst: handle.into() }, line);
        }

        for handle in handles {
            self.context.locals.make_usable(handle);
        }

        Ok(())
    }

    // fn match_lhs_rhs(&mut self, lhsnr: usize, mut rhsnr: usize, line: usize) {
    //     while lhsnr < rhsnr {
    //         self.instruction(I::Pop, line);
    //         rhsnr -= 1;
    //     }
    //     while lhsnr > rhsnr {
    //         self.instruction(I::Nil, line);
    //         rhsnr += 1;
    //     }
    // }

    fn identifier(&mut self) -> Result<RuaString, ParseError> {
        match self.next_token() {
            Some(Token { ttype: TT::IDENTIFIER(id), .. }) => Ok(id),
            Some(t) => Err(ParseError::UnexpectedToken(t.into(), [TT::IDENTIFIER_DUMMY].into())),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    const fn peek_token(&self) -> Option<&Token> {
        self.peeked_token.as_ref()
    }

    fn next_token(&mut self) -> Option<Token> {
        std::mem::replace(&mut self.peeked_token, self.tokens.next())
    }

    fn assign_or_call_st(&mut self, line: usize) -> Result<(), ParseError> {
        let dst = self.expression(Precedence::Lowest)?;
        match self.peek_token() {
            Some(Token { ttype: TT::ASSIGN, line, .. }) => {
                let line = *line;
                let instr = self.current_chunk().code().last();
                if matches!(instr, Some(&I::Index(..))) {
                    // return self.multiassign(line);
                }
                self.next_token();
                let mut val = self.expression(Precedence::Lowest)?;
                match dst.kind {
                    ExprKind::Local { reg, .. } => {
                        val.to_reg(self, reg)?;
                        val.free_reg(self);
                    }
                    ExprKind::Global(dst) => {
                        let src = val.to_any_reg(self)?;
                        let dst = self.new_string_constant(dst)?;
                        self.instruction(I::SetGlobal { dst, src }, line);
                        self.free_reg(src);
                    }
                    _ => return Err(ParseError::InvalidAssignLHS(line)),
                };
            }
            Some(Token { ttype: TT::COMMA, line, .. }) => {
                let line = *line;
                // return self.multiassign(line);
            }
            _ => {
                match self.current_chunk().code().last() {
                    // TODO not all expressions output instructions now
                    Some(I::Call { .. }) => {
                        dst.free_reg(self);
                    }
                    _ => return Err(ParseError::UnexpectedExpression(line)),
                }
            }
        }

        Ok(())
    }

    // fn multiassign(&mut self, line: usize) -> Result<(), ParseError> {
    //     // The first lhs was already parsed by assign_or_call_st
    //     let mut lhsnr = 1;
    //     let mut rhsnr = 0;
    //     let mut ops = Vec::new();
    //     ops.push(self.convert_to_assign(line)?);
    //     while match_token!(self, TT::COMMA) {
    //         self.expression(Precedence::Lowest)?;
    //         ops.push(self.convert_to_assign(line)?);
    //         lhsnr += 1;
    //     }
    //     consume!(self; (TT::ASSIGN));
    //     while {
    //         self.expression(Precedence::Lowest)?;
    //         rhsnr += 1;
    //         match_token!(self, TT::COMMA)
    //     } {}

    //     self.match_lhs_rhs(lhsnr, rhsnr, line);

    //     self.instruction(
    //         I::Multiassign(lhsnr.try_into().or(Err(ParseError::TooManyAssignLhs(line)))?),
    //         line,
    //     );
    //     for op in ops.iter().rev() {
    //         self.instruction(*op, line);
    //     }

    //     Ok(())
    // }

    fn call_expr(&mut self, mut lhs_desc: ExprDesc, line: usize) -> Result<ExprDesc, ParseError> {
        debug_peek_token!(self, TT::LPAREN | TT::STRING(_) | TT::LBRACE);
        let rh = self.context.rh;
        let base = self.tmp()?;
        lhs_desc.to_reg(self, base)?;
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
        debug_assert_eq!(self.context.rh, rh + 1);
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
            if let ExprKind::Tmp { reg, .. } = item.kind {
                if reg != u8::MAX {
                    self.free_reg(reg)
                }
            }
            let dst = self.tmp()?;
            item.to_reg(self, dst)?;
            match_token!(self, TT::COMMA)
        } {}
        consume!(self; (TT::RPAREN));
        Ok(cant_args)
    }

    fn if_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::IF);
        self.next_token();
        let mut cond = self.expression(Precedence::Lowest)?;
        let jmp_idx = cond.skip_if_false(self, line, false)?;
        consume!(self; (TT::THEN));
        self.scoped_block()?;

        match self.next_token() {
            Some(Token { ttype: TT::END, .. }) => {
                if let Some(jmp_idx) = jmp_idx {
                    self.patch_jmp_here(jmp_idx, line)?;
                }
            }
            Some(Token { ttype: TT::ELSE, line: line_else, .. }) => {
                let else_jmp = self.jmp(0, line);
                if let Some(jmp_idx) = jmp_idx {
                    self.patch_jmp_here(jmp_idx, line)?;
                }
                self.scoped_block()?;
                self.patch_jmp_here(else_jmp, line_else)?;
                consume!(self; (TT::END));
            }
            Some(Token { ttype: TT::ELSEIF, .. }) => todo!(),
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.into(),
                    [TT::END, TT::ELSE, TT::ELSEIF].into(),
                ))
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

    // fn for_st(&mut self, line: usize) -> Result<(), ParseError> {
    //     debug_peek_token!(self, TT::FOR);
    //     self.next_token();
    //     let empty_str = self.tokens.vm().new_string([].into());

    //     let id = self.identifier()?;
    //     consume!(self; (TT::ASSIGN));
    //     self.expression(Precedence::Lowest)?;
    //     let from = self.declare_local(empty_str.clone())?;
    //     consume!(self; (TT::COMMA));
    //     self.expression(Precedence::Lowest)?;
    //     let to = self.declare_local(empty_str.clone())?;
    //     if match_token!(self, (TT::COMMA)) {
    //         self.expression(Precedence::Lowest)?;
    //     } else {
    //         self.emit_number(1.0, line)?;
    //     }
    //     let step = self.declare_local(empty_str)?;

    //     let loop_start = self.pc();
    //     self.instruction(I::Le(BinArgs { dst: self.rh, lhs: from.into(), rhs: to.into() }), line);
    //     let exit_jmp = self.jmp_if_false_pop(u16::MAX, line);

    //     self.begin_scope();

    //     let it_var = self.declare_local(id)?;
    //     self.instruction(I::Mv(UnArgs { dst: it_var.into(), src: from.into() }), 0);
    //     self.do_block(false)?;

    //     self.end_scope();

    //     self.instruction(
    //         I::Add(BinArgs { dst: from.into(), lhs: from.into(), rhs: step.into() }),
    //         line,
    //     );

    //     self.loop_to(loop_start, line)?;
    //     self.patch_jmp(exit_jmp, line)?;

    //     self.instruction(I::Pop, line);
    //     self.instruction(I::Pop, line);
    //     self.instruction(I::Pop, line);

    //     self.context.locals.drop(3);
    //     Ok(())
    // }

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
        let offset = Chunk::offset(to, pc).or(Err(ParseError::JmpTooFar(line)))?;
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
        self.current_chunk_mut().negate_cond(instr_idx)
    }

    fn get_jmp_dst(&self, jmp_idx: usize) -> Option<usize> {
        let offset = match self.current_chunk().code()[jmp_idx] {
            Instruction::Jmp(offset) => offset,
            i => unreachable!("non Jmp instruction in jmp list {i:?}"),
        };
        if offset == 0 {
            None
        } else {
            Some((jmp_idx as isize + offset as isize) as usize)
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
                    self.patch_jmp(l, vtarget, 0)?;
                }
                _ => {
                    self.patch_jmp(l, dtarget.unwrap_or_else(||
                        panic!("dtarget should've been computed, jmp at {l} needed val. Code: {:?}", self.current_chunk().code())), 0)?;
                }
            }
            list = next
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
            self.context.locals.make_usable(loc);
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

    // fn function_expr(&mut self, line: usize) -> Result<ExprDesc, ParseError> {
    //     if let Some(Token { ttype: TT::IDENTIFIER(id), line, .. }) = self.peek_token() {
    //         return Err(ParseError::NamedFunctionExpr(id.clone(), *line));
    //     }
    //     let name = self.tokens.vm().new_string([].into());
    //     let (function, upvalues) = self.function(name)?;
    //     self.emit_closure(function, upvalues, line)?;
    //     Ok(todo!())
    // }

    fn function(&mut self, id: RuaString) -> Result<(Function, Upvalues), ParseError> {
        consume!(self; (TT::LPAREN));
        let mut locals = Locals::new();
        let loc = locals.declare(id.clone())?;
        locals.make_usable(loc);
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
            let loc = locals.declare(self.identifier()?)?;
            locals.make_usable(loc);
            match_token!(self, TT::COMMA)
        } {}
        consume!(self; (TT::RPAREN));
        Ok(arity)
    }

    fn table_literal(&mut self, line: usize) -> Result<ExprDesc, ParseError> {
        let mut arr_count = 0;
        let mut total_count = 0;
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
                            return Err(ParseError::UnexpectedToken(
                                t.clone().into(),
                                [TT::ASSIGN].into(),
                            ))
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
                            // I::GetUpvalue(up) => {
                            //     TODO
                            //     let name =
                            //         self.context.resolve_upvalue_name(up, &self.context_stack);
                            //     self.emit_string(name, line)?;
                            // }
                            ExprKind::Local { reg, .. } => {
                                let s = self
                                    .context
                                    .locals
                                    .name_of_reg(reg)
                                    .expect("Got non existing local");
                                self.new_string_constant(s.clone())?;
                                ExprKind::String(s)
                            }
                            ExprKind::Global(s) | ExprKind::String(s) => ExprKind::String(s),
                            _ => return Err(ParseError::InvalidAssignLHS(line)),
                        };
                    }
                    desc
                }
                None => return Err(ParseError::UnexpectedEOF),
            };

            total_count += 1;
            match self.peek_token() {
                Some(Token { ttype: TT::ASSIGN, .. }) => {
                    self.next_token();
                    let mut rhs_desc = self.expression(Precedence::Lowest)?;
                    let mut lhs_desc = desc;
                    let lhs = lhs_desc.to_any_reg(self)?;
                    let rhs = rhs_desc.to_any_reg(self)?;
                    self.instruction(I::InsertKeyVal { table, key: lhs, val: rhs }, line);
                    self.free2reg(lhs, rhs);
                }
                Some(Token { line, .. }) => {
                    let line = *line;
                    let mut rhs_desc = desc;
                    let rhs = rhs_desc.to_any_reg(self)?;
                    let lhs = self.tmp()?;
                    arr_count += 1;
                    self.emit_number(lhs, f64::from(arr_count), line)?;
                    self.instruction(I::InsertKeyVal { table, key: lhs, val: rhs }, line);
                    self.free2reg(lhs, rhs);
                }
                None => {}
            }

            if !match_token!(self, TT::COMMA, TT::SEMICOLON) {
                break;
            }
        }
        consume!(self; (TT::RBRACE));

        self.current_chunk_mut()
            .replace_instruction(I::NewTable { dst: table, capacity: total_count }, table_instr);

        Ok(ExprDesc::new(ExprKind::Tmp { reg: table, instr_idx: None }))
    }
}

pub fn compile<C: Iterator<Item = u8> + Clone>(
    chars: C,
    vm: &mut Vm,
) -> Result<Closure, ParseError> {
    let tokens = Tokenizer::new(chars, vm);
    let compiler = Compiler::new(tokens);
    compiler.compile().map(Closure::new)
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Or,
    And,
    Comparator,  // >, <, <=, >=, ~=, ==
    Dotdot,      // ..
    Sum,         // +, -
    Product,     // *, /, %
    Prefix,      // not, #, - (unary)
    Exp,         // ^
    Call,        // foo(...)
    FieldAccess, // foo.bar, foo[bar]
}

fn precedence_of_binary(op: &TT) -> Precedence {
    match op {
        TT::OR => Precedence::Or,
        TT::AND => Precedence::And,
        TT::EQ | TT::NEQ | TT::LE | TT::GE | TT::LT | TT::GT => Precedence::Comparator,
        TT::DOTDOT => Precedence::Dotdot,
        TT::PLUS | TT::MINUS => Precedence::Sum,
        TT::TIMES | TT::DIV | TT::MOD => Precedence::Product,
        TT::EXP => Precedence::Exp,
        TT::LBRACK => Precedence::FieldAccess,
        _ => unreachable!("Invalid binary"),
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
        Self {
            locals,
            chunk: Chunk::default(),
            upvalues: Upvalues::default(),
            arity,
            name,
            max_used_regs: 0,
            rh: 0,
        }
    }

    fn resolve_local(&self, id: &RuaString) -> Option<LocalHandle> {
        self.locals.resolve(id)
    }

    fn resolve_upvalue(
        &mut self,
        id: &RuaString,
        context_stack: &mut [Self],
    ) -> Result<Option<UpvalueHandle>, ParseError> {
        if let Some((parent, tail)) = context_stack.split_last_mut() {
            if let Some(local) = parent.resolve_local(id) {
                parent.capture(local);
                Some(self.add_upvalue(Left(local))).transpose()
            } else {
                parent
                    .resolve_upvalue(id, tail)?
                    .map(|upvalue| self.add_upvalue(Right(upvalue)))
                    .transpose()
            }
        } else {
            Ok(None)
        }
    }

    fn add_upvalue(
        &mut self,
        upvalue: Either<LocalHandle, UpvalueHandle>,
    ) -> Result<UpvalueHandle, ParseError> {
        self.upvalues.find_or_add(upvalue)
    }

    fn capture(&mut self, local: LocalHandle) {
        self.locals.capture(local);
    }

    fn resolve_upvalue_name(&self, up: UpvalueHandle, context_stack: &[Self]) -> RuaString {
        match self.upvalues.get(up).location() {
            Left(local) => {
                if let Some((parent, _)) = context_stack.split_last() {
                    parent.locals.get(local)
                } else {
                    unreachable!("Should've found upvalue before getting to the top")
                }
            }
            Right(upvalue) => {
                if let Some((parent, tail)) = context_stack.split_last() {
                    parent.resolve_upvalue_name(upvalue, tail)
                } else {
                    unreachable!("Should've found upvalue before getting to the top")
                }
            }
        }
    }
}

#[derive(Debug)]
enum ExprKind {
    Local { reg: u8, instr_idx: Option<usize> }, // instr_idx = None means expr is non relocable
    Global(RuaString),
    Tmp { reg: u8, instr_idx: Option<usize> }, // instr_idx = None means expr is non relocable
    Number(f64),
    String(RuaString),
    Function(FnHandle),
    True,
    False,
    Nil,
    Jmp { instr_idx: usize },
}

#[derive(Debug)]
struct ExprDesc {
    kind: ExprKind,
    t_jmp: Option<usize>,
    f_jmp: Option<usize>,
}

impl ExprDesc {
    const fn new(kind: ExprKind) -> Self {
        Self { kind, t_jmp: None, f_jmp: None }
    }

    fn to_any_reg<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
    ) -> Result<u8, ParseError> {
        // TODO dischage_vars for handling globals/upvals
        match &self.kind {
            ExprKind::Tmp { reg, instr_idx: None } | ExprKind::Local { reg, .. } => {
                let reg = *reg;
                if !self.has_jmps() {
                    return Ok(reg);
                } else if compiler.is_tmp(reg) {
                    self.to_reg(compiler, reg)?;
                    return Ok(reg);
                }
            }
            _ => {}
        };
        self.to_next_reg(compiler)
    }

    fn free_reg<T: Iterator<Item = u8> + Clone>(&self, compiler: &mut Compiler<'_, T>) {
        if let ExprKind::Tmp { reg, .. } = &self.kind {
            if *reg != u8::MAX {
                compiler.free_reg(*reg)
            }
        }
    }

    fn skip_if_false<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
        line: usize,
        store_val: bool,
    ) -> Result<Option<usize>, ParseError> {
        let jmp = match &self.kind {
            ExprKind::Jmp { instr_idx } => Some(*instr_idx),
            ExprKind::Number(_) | ExprKind::String(_) | ExprKind::Function(_) | ExprKind::True => {
                None
            }
            _ => {
                let src = self.to_any_reg(compiler)?;
                compiler.free_reg(src);
                if store_val {
                    let jmp_idx = compiler.cond_jmp(I::TestSet { dst: u8::MAX, src }, line);
                    self.kind = ExprKind::Tmp { reg: u8::MAX, instr_idx: Some(jmp_idx - 1) };
                    Some(jmp_idx)
                } else {
                    let jmp_idx = compiler.cond_jmp(I::Test { src }, line);
                    Some(jmp_idx)
                }
            }
        };
        compiler.concat_jmp_lists(&mut self.f_jmp, jmp)?;
        compiler.patch_list_to_here(self.t_jmp.take())?;
        Ok(jmp)
    }

    fn skip_if_true<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
        line: usize,
    ) -> Result<Option<usize>, ParseError> {
        let jmp = match &self.kind {
            ExprKind::Jmp { instr_idx } => {
                compiler.negate_cond(*instr_idx - 1);
                Some(*instr_idx)
            }
            ExprKind::False | ExprKind::Nil => None,
            _ => {
                let src = self.to_any_reg(compiler)?;
                compiler.free_reg(src);
                let jmp_idx = compiler.cond_jmp(I::UntestSet { dst: u8::MAX, src }, line);
                self.kind = ExprKind::Tmp { reg: u8::MAX, instr_idx: Some(jmp_idx - 1) };
                Some(jmp_idx)
            }
        };
        compiler.concat_jmp_lists(&mut self.t_jmp, jmp)?;
        compiler.patch_list_to_here(self.f_jmp.take())?;
        Ok(jmp)
    }

    fn has_jmps(&self) -> bool {
        !matches!((self.t_jmp, self.f_jmp), (None, None))
    }

    fn to_reg<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
        dst: u8,
    ) -> Result<(), ParseError> {
        match &self.kind {
            ExprKind::Local { reg, instr_idx } => {
                if *reg != dst {
                    if let Some(instr_idx) = instr_idx {
                        compiler.current_chunk_mut().chg_dst_of(*instr_idx, dst);
                    } else {
                        compiler.instruction(I::Mv(UnArgs { dst, src: *reg }), 0);
                    }
                }
            }
            ExprKind::Tmp { reg, instr_idx } => {
                if *reg != dst {
                    if let Some(instr_idx) = instr_idx {
                        compiler.current_chunk_mut().chg_dst_of(*instr_idx, dst);
                    } else {
                        compiler.instruction(I::Mv(UnArgs { dst, src: *reg }), 0);
                    }
                }
            }
            ExprKind::Global(s) => {
                let src = compiler.new_string_constant(s.clone())?;
                compiler.instruction(I::GetGlobal { dst, src }, 0);
            }
            ExprKind::Number(n) => {
                compiler.emit_number(dst, *n, 0)?;
            }
            ExprKind::String(s) => {
                compiler.emit_string(dst, s.clone(), 0)?;
            }
            ExprKind::Function(f) => todo!(),
            ExprKind::True => {
                compiler.instruction(I::True { dst }, 0);
            }
            ExprKind::False => {
                compiler.instruction(I::False { dst }, 0);
            }
            ExprKind::Nil => {
                compiler.instruction(I::Nil { dst }, 0);
            }
            ExprKind::Jmp { instr_idx } => {
                compiler.negate_cond(*instr_idx - 1);
                compiler.concat_jmp_lists(&mut self.t_jmp, Some(*instr_idx))?;
            }
        };
        if self.has_jmps() {
            let (mut pos_f, mut pos_t) = (None, None);
            if compiler.list_needs_val(self.t_jmp) || compiler.list_needs_val(self.f_jmp) {
                let fj = if let ExprKind::Jmp { .. } = self.kind {
                    None
                } else {
                    Some(compiler.instruction(I::Jmp(i16::MAX), 0))
                };
                pos_f = Some(compiler.instruction(I::LFalseSkip { dst }, 0));
                pos_t = Some(compiler.instruction(I::True { dst }, 0));
                if let Some(list) = fj {
                    compiler.patch_jmp_here(list, 0)?;
                }
            }
            let end = compiler.pc();
            compiler.patch_list_aux(self.f_jmp, end, dst, pos_f)?;
            compiler.patch_list_aux(self.t_jmp, end, dst, pos_t)?;
        }
        (self.f_jmp, self.t_jmp) = (None, None);
        self.kind = ExprKind::Tmp { reg: dst, instr_idx: None };
        Ok(())
    }

    fn to_next_reg<T: Iterator<Item = u8> + Clone>(
        &mut self,
        compiler: &mut Compiler<'_, T>,
    ) -> Result<u8, ParseError> {
        let tmp = compiler.tmp()?;
        self.to_reg(compiler, tmp)?;
        Ok(tmp)
    }
}
