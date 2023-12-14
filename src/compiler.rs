use std::fmt::Debug;

use either::Either::{self, Left, Right};

use crate::{
    compiler::bytecode::BinArgs,
    eval::{
        vals::{closure::Closure, function::Function, string::RuaString},
        Vm,
    },
    lex::{
        tokens::{BinaryOp, Token, TokenType as TT, UnaryOp},
        Tokenizer,
    },
};

use self::{
    bytecode::{Chunk, FnHandle, Instruction as I, Instruction, ParseError, StringHandle, UnArgs},
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
    rh: u8,
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
            rh: 0,
        }
    }

    fn compile_fn(&mut self) -> Result<(Function, Upvalues), ParseError> {
        self.scoped_block()?;
        self.instruction(I::ReturnNil, 0);
        if let Some(ctxt) = self.context_stack.pop() {
            let fn_ctxt = std::mem::replace(&mut self.context, ctxt);
            let arity = fn_ctxt.arity;
            let name = fn_ctxt.name;
            let func = Function::new(fn_ctxt.chunk, arity, name, fn_ctxt.upvalues.len());
            Ok((func, fn_ctxt.upvalues))
        } else {
            unreachable!("compile_fn must have a parent context")
        }
    }

    pub fn compile(mut self) -> Result<Function, ParseError> {
        self.scoped_block()?;
        self.instruction(I::ReturnNil, 0);

        debug_assert!(self.context_stack.is_empty());
        let arity = self.context.arity;
        let name = self.context.name;
        let func = Function::new(self.context.chunk, arity, name, 0);
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
                    // self.function_st(line, false)?;
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

    fn expression(
        &mut self,
        precedence: Precedence,
        dst: Option<ExprDst>,
    ) -> Result<Item, ParseError> {
        let item = self.prefix_exp(dst)?;
        let item = self.infix_exp(item, precedence, dst)?;
        match dst {
            Some(dst) if self.rh == 0 => {
                let reg = self.into_reg(&item, dst, true)?;
                Ok(Item::Reg(reg))
            }
            _ => Ok(item),
        }
    }

    fn group_expression(&mut self, dst: Option<ExprDst>) -> Result<Item, ParseError> {
        let rh = self.rh;
        let item = self.expression(Precedence::Lowest, dst)?;
        debug_assert_eq!(self.rh, rh);
        consume!(self; (TT::RPAREN));
        Ok(item)
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.context.chunk
    }

    fn emit_number(&mut self, val: f64, line: usize, dst: ExprDst) -> Result<u8, ParseError> {
        self.current_chunk().add_number(dst.0, val, line)?;
        Ok(dst.0)
    }

    fn emit_string(&mut self, val: RuaString, line: usize, dst: ExprDst) -> Result<u8, ParseError> {
        self.current_chunk().add_string(dst.0, val, line)?;
        Ok(dst.0)
    }

    fn tmp(&mut self) -> Result<u8, ParseError> {
        let dst = (self.rh as u16 + self.context.locals.len() as u16)
            .try_into()
            .map_err(|_| ParseError::TooManyLocals)?;
        Ok(dst)
    }

    fn new_string_constant(&mut self, val: RuaString) -> Result<StringHandle, ParseError> {
        self.current_chunk().new_string_constant(val)
    }

    fn emit_closure(
        &mut self,
        val: Function,
        upvalues: Upvalues,
        line: usize,
    ) -> Result<(), ParseError> {
        self.current_chunk().add_closure(val, upvalues, line)
    }

    fn into_reg(&mut self, item: &Item, dst: ExprDst, force: bool) -> Result<u8, ParseError> {
        match item {
            Item::Reg(r) => {
                if force {
                    self.instruction(I::Mv(UnArgs { dst: dst.0, src: *r }), 0);
                    Ok(dst.0)
                } else {
                    Ok(*r)
                }
            }
            Item::Number(n) => self.emit_number(*n, 0, dst),
            Item::String(s) => self.emit_string(s.clone(), 0, dst),
            Item::Function(f) => todo!(),
            Item::True => {
                self.instruction(I::True { dst: dst.0 }, 0);
                Ok(dst.0)
            }
            Item::False => {
                self.instruction(I::False { dst: dst.0 }, 0);
                Ok(dst.0)
            }
            Item::Nil => {
                self.instruction(I::Nil { dst: dst.0 }, 0);
                Ok(dst.0)
            }
        }
    }

    fn into_tmp_reg(&mut self, item: &Item) -> Result<u8, ParseError> {
        let tmp = self.tmp()?;
        self.into_reg(item, ExprDst(tmp), false)
    }

    fn dst_or_tmp(&mut self, dst: Option<ExprDst>) -> Result<u8, ParseError> {
        match dst {
            Some(d) => Ok(d.0),
            None => self.tmp(),
        }
    }

    fn unary(
        &mut self,
        op: UnaryOp,
        line: usize,
        dst: Option<ExprDst>,
    ) -> Result<Item, ParseError> {
        let rh = self.rh;
        let item = self.expression(Precedence::Prefix, dst)?;
        debug_assert_eq!(self.rh, rh);
        let dst = self.dst_or_tmp(dst)?;
        let src = self.into_reg(&item, ExprDst(dst), false)?;
        match op {
            UnaryOp::NOT => self.instruction(I::Not(UnArgs { dst, src }), line),
            UnaryOp::LEN => self.instruction(I::Len(UnArgs { dst, src }), line),
        };
        Ok(Item::Reg(dst))
    }

    fn is_tmp(&self, reg: u8) -> bool {
        reg >= self.context.locals.len()
    }

    fn binary(
        &mut self,
        item: Item,
        op: BinaryOp,
        line: usize,
        dst: Option<ExprDst>,
    ) -> Result<Item, ParseError> {
        let precedence = precedence_of_binary(op);
        macro_rules! binary_operator {
            ($compiler: expr, $precedence: expr, $instr: ident, $line: expr) => {{
                let dst = self.dst_or_tmp(dst)?;
                let lhs = self.into_reg(&item, ExprDst(dst), false)?;
                if self.is_tmp(lhs) {
                    self.rh += 1;
                }
                let rhs = $compiler.expression($precedence, None)?;
                let rhs = self.into_tmp_reg(&rhs)?;
                if self.is_tmp(lhs) {
                    self.rh -= 1;
                }
                $compiler.instruction(I::$instr(BinArgs { dst, lhs, rhs }), $line);
                return Ok(Item::Reg(dst));
            }};
        }
        match op {
            BinaryOp::PLUS => binary_operator!(self, precedence, Add, line),
            BinaryOp::TIMES => binary_operator!(self, precedence, Mul, line),
            BinaryOp::DIV => binary_operator!(self, precedence, Div, line),
            BinaryOp::MOD => binary_operator!(self, precedence, Mod, line),
            BinaryOp::EXP => binary_operator!(self, precedence, Pow, line),
            BinaryOp::EQ => binary_operator!(self, precedence, Eq, line),
            BinaryOp::NEQ => binary_operator!(self, precedence, Neq, line),
            BinaryOp::LE => binary_operator!(self, precedence, Le, line),
            BinaryOp::GE => binary_operator!(self, precedence, Ge, line),
            BinaryOp::LT => binary_operator!(self, precedence, Lt, line),
            BinaryOp::GT => binary_operator!(self, precedence, Gt, line),
            BinaryOp::AND => self.and(line),
            BinaryOp::OR => self.or(line),
            BinaryOp::DOTDOT => binary_operator!(self, precedence, StrConcat, line),
        }
    }

    fn prefix_exp(&mut self, dst: Option<ExprDst>) -> Result<Item, ParseError> {
        match self.next_token() {
            Some(Token { ttype: TT::UNARY_OP(op), line, .. }) => return self.unary(op, line, dst),
            Some(Token { ttype: TT::MINUS, line, .. }) => {
                let item = self.expression(Precedence::Prefix, dst)?;
                let dst = self.dst_or_tmp(dst)?;
                if let Item::Number(n) = item {
                    return Ok(Item::Number(-n));
                }
                let src = self.into_reg(&item, ExprDst(dst), false)?;
                self.instruction(I::Neg(UnArgs { dst, src }), line);
                Ok(Item::Reg(dst))
            }
            Some(Token { ttype: TT::IDENTIFIER(id), line, .. }) => self.named_variable(id, line),
            Some(Token { ttype: TT::NUMBER(n), line, .. }) => {
                // self.emit_number(n, line)?;
                Ok(Item::Number(n))
            }
            Some(Token { ttype: TT::STRING(s), line, .. }) => {
                // self.emit_string(s, line)?;
                Ok(Item::String(s))
            }
            Some(Token { ttype: TT::TRUE, line, .. }) => Ok(Item::True),
            Some(Token { ttype: TT::FALSE, line, .. }) => Ok(Item::False),
            Some(Token { ttype: TT::NIL, line, .. }) => Ok(Item::Nil),
            Some(Token { ttype: TT::LPAREN, .. }) => return self.group_expression(dst),
            // Some(Token { ttype: TT::LBRACE, line, .. }) => return self.table_literal(line),
            // Some(Token { ttype: TT::FUNCTION, line, .. }) => return self.function_expr(line),
            Some(t) => {
                let line = t.line;
                return Err(ParseError::UnexpectedTokenWithErrorMsg(
                    Box::new(t),
                    "an expression".into(),
                    line,
                ));
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }

    fn named_variable(&mut self, id: RuaString, line: usize) -> Result<Item, ParseError> {
        if let Some(local) = self.context.resolve_local(&id) {
            Ok(Item::Reg(local.into()))
        } else if let Some(upvalue) = self.context.resolve_upvalue(&id, &mut self.context_stack)? {
            let dst = self.tmp()?;
            self.instruction(I::GetUpvalue(upvalue), line);
            Ok(Item::Reg(dst))
        } else {
            let id = self.new_string_constant(id)?;
            let dst = self.tmp()?;
            self.instruction(I::GetGlobal { dst, src: id }, line);
            Ok(Item::Reg(dst))
        }
    }

    fn instruction(&mut self, instr: Instruction, line: usize) -> usize {
        self.current_chunk().add_instruction(instr, line)
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        self.current_chunk().pop_instruction()
    }

    fn infix_exp(
        &mut self,
        mut item: Item,
        precedence: Precedence,
        dst: Option<ExprDst>,
    ) -> Result<Item, ParseError> {
        loop {
            match self.peek_token() {
                Some(Token { ttype: TT::BINARY_OP(op), line, .. }) => {
                    let line = *line;
                    let op = *op;
                    if precedence < precedence_of_binary(op) {
                        self.next_token();
                        item = self.binary(item, op, line, dst)?;
                    } else {
                        break Ok(item);
                    }
                }
                Some(Token { ttype: TT::MINUS, line, .. }) => {
                    let line = *line;
                    if precedence < Precedence::Sum {
                        self.next_token();

                        let dst = self.dst_or_tmp(dst)?;
                        let lhs = self.into_reg(&item, ExprDst(dst), false)?;
                        if self.is_tmp(lhs) {
                            self.rh += 1;
                        }
                        let rhs = self.expression(Precedence::Sum, None)?;
                        let rhs = self.into_tmp_reg(&rhs)?;
                        if self.is_tmp(lhs) {
                            self.rh -= 1;
                        }
                        self.instruction(I::Sub(BinArgs { dst, lhs, rhs }), line);
                        item = Item::Reg(dst);
                    } else {
                        break Ok(item);
                    }
                }
                Some(Token { ttype: TT::LPAREN | TT::STRING(_) | TT::LBRACE, line, .. }) => {
                    let line = *line;
                    if precedence < Precedence::Call {
                        self.call_expr(line)?;
                    } else {
                        break Ok(item);
                    }
                }
                Some(Token { ttype: TT::DOT, line, .. }) => {
                    let line = *line;
                    if precedence < Precedence::FieldAccess {
                        self.next_token();
                        let id = self.identifier()?;
                        self.rh += 1;
                        let key = self.emit_string(id, line, ExprDst(self.rh))?;
                        self.rh -= 1;
                        let rh = self.rh;
                        let lhs = self.into_tmp_reg(&item)?;
                        self.instruction(I::Index(BinArgs { dst: rh, lhs, rhs: key }), line);
                        item = Item::Reg(rh);
                    } else {
                        break Ok(item);
                    }
                }
                Some(Token { ttype: TT::LBRACK, line, .. }) => {
                    let line = *line;
                    if precedence < Precedence::FieldAccess {
                        self.next_token();
                        self.rh += 1;
                        let key = self.expression(Precedence::Lowest, dst)?;
                        let key = self.into_tmp_reg(&key)?;
                        self.rh -= 1;
                        let rh = self.rh;
                        let lhs = self.into_tmp_reg(&item)?;
                        self.instruction(I::Index(BinArgs { dst: rh, lhs, rhs: key }), line);
                        item = Item::Reg(rh);
                        consume!(self; (TT::RBRACK));
                    } else {
                        break Ok(item);
                    }
                }
                _ => break Ok(item),
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

        let item = self.expression(Precedence::Lowest, None)?;
        debug_assert_eq!(self.rh, 0);
        match self.peek_token() {
            Some(Token { ttype: TT::END | TT::ELSE | TT::ELSEIF, .. }) | None => {}
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.clone().into(),
                    [TT::END, TT::ELSE, TT::ELSEIF].into(),
                ))
            }
        }
        let src = self.into_tmp_reg(&item)?;
        self.instruction(I::Return { src }, line);
        Ok(())
    }

    fn local_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::LOCAL);
        self.next_token();

        let mut locals = match self.next_token() {
            // Some(Token { ttype: TT::FUNCTION, line, .. }) => return self.function_st(line, true),
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
                    let handle = self.context.locals.declare(local)?;
                    handles.push(handle);
                    Some(ExprDst(handle.into()))
                } else {
                    None
                };
                self.expression(Precedence::Lowest, dst)?;
                debug_assert_eq!(self.rh, 0);
                match_token!(self, TT::COMMA)
            } {}
        }
        for local in locals {
            let handle = self.context.locals.declare(local)?;
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

    #[must_use = "Use consume!(self; (TT::IDENTIFIER(_))) if you don't care about the value"]
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
        let code_len = self.current_chunk().code().len();
        let item = self.expression(Precedence::Lowest, None)?;
        debug_assert_eq!(self.rh, 0);
        match self.peek_token() {
            Some(Token { ttype: TT::ASSIGN, line, .. }) => {
                let line = *line;
                let instr = self.current_chunk().code().last();
                if matches!(instr, Some(&I::Index(..))) {
                    // return self.multiassign(line);
                }
                self.next_token();
                let (dst, op) = self.convert_to_assign(code_len, &item, line)?;
                let val = self.expression(Precedence::Lowest, dst)?;
                debug_assert_eq!(self.rh, 0);
                if let Some(mut op) = op {
                    let src = self.into_tmp_reg(&val)?;
                    op.change_src(src);
                    self.instruction(op, line);
                }
            }
            Some(Token { ttype: TT::COMMA, line, .. }) => {
                let line = *line;
                // return self.multiassign(line);
            }
            _ => {
                match self.current_chunk().code().last().expect("Just parsed an expression") {
                    // TODO not all expressions output instructions now
                    I::Call { .. } => {
                        // It's a call statement, gotta pop the return value
                        self.instruction(I::Pop, line);
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

    fn convert_to_assign(
        &mut self,
        prev_code_len: usize,
        item: &Item,
        line: usize,
    ) -> Result<(Option<ExprDst>, Option<Instruction>), ParseError> {
        if let Item::Reg(dst) = item {
            if prev_code_len == self.current_chunk().code().len() {
                return Ok((Some(ExprDst(*dst)), None));
            }
            match self.pop_instruction().expect("Code is larger than it was") {
                I::GetGlobal { dst, src } => Ok((None, Some(I::SetGlobal { dst: src, src: dst }))),
                I::GetUpvalue(..) => todo!(), //Ok((None, Some(I::SetUpvalue(up)))),
                I::Index(..) => todo!(),      //Ok(I::InsertKeyVal),
                _ => Err(ParseError::InvalidAssignLHS(line)),
            }
        } else {
            return Err(ParseError::InvalidAssignLHS(line));
        }
    }

    fn call_expr(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::LPAREN | TT::STRING(_) | TT::LBRACE);
        let rh = self.rh;
        self.rh += 1;
        let cant_args = match self.next_token() {
            Some(Token { ttype: TT::LPAREN, .. }) => self.arg_list()?,
            Some(Token { ttype: TT::STRING(s), line, .. }) => {
                self.emit_string(s, line, ExprDst(self.rh))?;
                self.rh += 1;
                1
            }
            Some(Token { ttype: TT::LBRACE, line, .. }) => {
                // self.table_literal(line)?;
                1
            }
            _ => unreachable!("Invalid token at call_expr"),
        };

        self.rh -= cant_args + 1;
        debug_assert_eq!(self.rh, rh);
        let base = self.tmp()?;
        self.instruction(I::Call { base, nargs: cant_args }, line);
        Ok(())
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
            let item = self.expression(Precedence::Lowest, None)?;
            self.into_tmp_reg(&item)?;
            self.rh += 1;
            match_token!(self, TT::COMMA)
        } {}
        consume!(self; (TT::RPAREN));
        Ok(cant_args)
    }

    fn if_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::IF);
        self.next_token();
        self.expression(Precedence::Lowest, None)?;
        debug_assert_eq!(self.rh, 0);
        consume!(self; (TT::THEN));
        let if_jmp = self.jmp_if_false_pop(u16::MAX, line);
        self.scoped_block()?;

        match self.next_token() {
            Some(Token { ttype: TT::END, .. }) => {
                self.patch_jmp(if_jmp, line)?;
            }
            Some(Token { ttype: TT::ELSE, line: line_else, .. }) => {
                let else_jmp = self.jmp(0, line);
                self.patch_jmp(if_jmp, line)?;
                self.scoped_block()?;
                self.patch_jmp(else_jmp, line_else)?;
                consume!(self; (TT::END));
            }
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
        let loop_start = self.current_chunk().code().len();
        self.expression(Precedence::Lowest, None)?;
        debug_assert_eq!(self.rh, 0);
        let exit_jmp = self.jmp_if_false_pop(u16::MAX, line);
        self.do_block(true)?;

        self.loop_to(loop_start, line)?;
        self.patch_jmp(exit_jmp, line)?;

        Ok(())
    }

    // fn for_st(&mut self, line: usize) -> Result<(), ParseError> {
    //     debug_peek_token!(self, TT::FOR);
    //     self.next_token();
    //     let empty_str = self.tokens.vm().new_string([].into());

    //     let id = self.identifier()?;
    //     consume!(self; (TT::ASSIGN));
    //     self.expression(Precedence::Lowest)?;
    //     let from = self.context.locals.declare(empty_str.clone())?;
    //     consume!(self; (TT::COMMA));
    //     self.expression(Precedence::Lowest)?;
    //     let to = self.context.locals.declare(empty_str.clone())?;
    //     if match_token!(self, (TT::COMMA)) {
    //         self.expression(Precedence::Lowest)?;
    //     } else {
    //         self.emit_number(1.0, line)?;
    //     }
    //     let step = self.context.locals.declare(empty_str)?;

    //     let loop_start = self.current_chunk().code().len();
    //     self.instruction(I::Le(BinArgs { dst: self.rh, lhs: from.into(), rhs: to.into() }), line);
    //     let exit_jmp = self.jmp_if_false_pop(u16::MAX, line);

    //     self.begin_scope();

    //     let it_var = self.context.locals.declare(id)?;
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

    fn jmp(&mut self, to: u16, line: usize) -> usize {
        self.instruction(I::Jmp(to), line)
    }

    fn loop_to(&mut self, target: usize, line: usize) -> Result<usize, ParseError> {
        let offset = Chunk::offset(self.current_chunk().code().len(), target)
            .or(Err(ParseError::JmpTooFar(line)))?;
        Ok(self.instruction(I::Loop(offset), line))
    }

    fn jmp_if_false_pop(&mut self, to: u16, line: usize) -> usize {
        self.instruction(I::JmpIfFalsePop(to), line)
    }

    fn jmp_if_false(&mut self, to: u16, line: usize) -> usize {
        self.instruction(I::JmpIfFalse(to), line)
    }

    fn jmp_if_true(&mut self, to: u16, line: usize) -> usize {
        self.instruction(I::JmpIfTrue(to), line)
    }

    fn patch_jmp(&mut self, jmp: usize, line: usize) -> Result<(), ParseError> {
        self.current_chunk().patch_jmp(jmp, line)
    }

    fn and(&mut self, line: usize) -> Result<Item, ParseError> {
        let jmp = self.jmp_if_false(0, line);
        self.instruction(I::Pop, line);

        self.expression(Precedence::And, None)?;

        self.patch_jmp(jmp, line)?;
        Ok(todo!())
    }

    fn or(&mut self, line: usize) -> Result<Item, ParseError> {
        let jmp = self.jmp_if_true(0, line);
        self.instruction(I::Pop, line);

        self.expression(Precedence::Or, None)?;

        self.patch_jmp(jmp, line)?;

        Ok(todo!())
    }

    // fn function_st(&mut self, line: usize, local: bool) -> Result<(), ParseError> {
    //     let id = self.identifier().map_or(Err(ParseError::UnnamedFunctionSt(line)), Ok)?;
    //     let (function, upvalues) = self.function(id.clone())?;

    //     self.emit_closure(function, upvalues, line)?;
    //     if local {
    //         self.context.locals.declare(id)?;
    //     } else {
    //         let id = self.new_string_constant(id)?;
    //         let src = self.dst()?;
    //         self.instruction(I::SetGlobal { dst: id, src }, line);
    //     }
    //     Ok(())
    // }

    // fn function_expr(&mut self, line: usize) -> Result<Item, ParseError> {
    //     if let Some(Token { ttype: TT::IDENTIFIER(id), line, .. }) = self.peek_token() {
    //         return Err(ParseError::NamedFunctionExpr(id.clone(), *line));
    //     }
    //     let name = self.tokens.vm().new_string([].into());
    //     let (function, upvalues) = self.function(name)?;
    //     self.emit_closure(function, upvalues, line)?;
    //     Ok(todo!())
    // }

    // fn function(&mut self, id: RuaString) -> Result<(Function, Upvalues), ParseError> {
    //     consume!(self; (TT::LPAREN));
    //     let mut locals = Locals::new();
    //     locals.declare(id.clone())?;
    //     let arity = self.parameter_list(&mut locals)?;

    //     let old_ctxt =
    //         std::mem::replace(&mut self.context, CompilerCtxt::for_function(locals, arity, id));
    //     self.context_stack.push(old_ctxt);
    //     let retval = self.compile_fn();
    //     consume!(self; (TT::END));
    //     retval
    // }

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

    // fn table_literal(&mut self, line: usize) -> Result<Item, ParseError> {
    //     let mut ops = Vec::new();
    //     let mut arr_count = 0;
    //     loop {
    //         match self.peek_token() {
    //             Some(Token { ttype: TT::LBRACK, .. }) => {
    //                 self.next_token();
    //                 self.expression(Precedence::Lowest)?;
    //                 consume!(self; (TT::RBRACK));
    //                 match self.peek_token() {
    //                     Some(Token { ttype: TT::ASSIGN, .. }) => {}
    //                     Some(t) => {
    //                         return Err(ParseError::UnexpectedToken(
    //                             t.clone().into(),
    //                             [TT::ASSIGN].into(),
    //                         ))
    //                     }
    //                     None => return Err(ParseError::UnexpectedEOF),
    //                 }
    //             }
    //             Some(Token { ttype: TT::RBRACE, .. }) => {
    //                 break;
    //             }
    //             Some(Token { line, .. }) => {
    //                 let line = *line;
    //                 self.expression(Precedence::Lowest)?;
    //                 if peek_token_is!(self, TT::ASSIGN) {
    //                     match self
    //                         .current_chunk()
    //                         .code()
    //                         .last()
    //                         .copied()
    //                         .expect("Just parsed an expression")
    //                     {
    //                         // I::GetGlobal(s) => {
    //                         //     self.pop_instruction();
    //                         //     let s = self.current_chunk().read_string(s);
    //                         //     self.emit_string(s, line)?;
    //                         // }
    //                         I::GetUpvalue(up) => {
    //                             self.pop_instruction();
    //                             let name =
    //                                 self.context.resolve_upvalue_name(up, &self.context_stack);
    //                             self.emit_string(name, line)?;
    //                         }
    //                         // TODO
    //                         // I::GetLocal(idx) => {
    //                         //     self.pop_instruction();
    //                         //     let name = self.context.locals.get(idx);
    //                         //     self.emit_string(name, line)?;
    //                         // }
    //                         I::String { .. } => {}
    //                         _ => return Err(ParseError::InvalidAssignLHS(line)),
    //                     }
    //                 }
    //             }
    //             None => return Err(ParseError::UnexpectedEOF),
    //         }

    //         match self.peek_token() {
    //             Some(Token { ttype: TT::ASSIGN, .. }) => {
    //                 self.next_token();
    //                 self.expression(Precedence::Lowest)?;
    //                 ops.push(I::InsertKeyVal);
    //             }
    //             Some(Token { line, .. }) => {
    //                 let line = *line;
    //                 ops.push(I::InsertValKey);
    //                 arr_count += 1;
    //                 self.emit_number(f64::from(arr_count), line)?;
    //             }
    //             None => {}
    //         }

    //         if !match_token!(self, TT::COMMA, TT::SEMICOLON) {
    //             break;
    //         }
    //     }
    //     consume!(self; (TT::RBRACE));

    //     self.instruction(I::NewTable(ops.len().try_into().unwrap_or(u16::MAX)), line);
    //     for op in ops.into_iter().rev() {
    //         self.instruction(op, line);
    //     }

    //     Ok(todo!())
    // }
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

const fn precedence_of_binary(op: BinaryOp) -> Precedence {
    match op {
        BinaryOp::OR => Precedence::Or,
        BinaryOp::AND => Precedence::And,
        BinaryOp::EQ
        | BinaryOp::NEQ
        | BinaryOp::LE
        | BinaryOp::GE
        | BinaryOp::LT
        | BinaryOp::GT => Precedence::Comparator,
        BinaryOp::DOTDOT => Precedence::Dotdot,
        BinaryOp::PLUS => Precedence::Sum,
        BinaryOp::TIMES | BinaryOp::DIV | BinaryOp::MOD => Precedence::Product,
        BinaryOp::EXP => Precedence::Exp,
    }
}

struct CompilerCtxt {
    locals: Locals,
    chunk: Chunk,
    upvalues: Upvalues,
    arity: u8,
    name: RuaString,
}

impl CompilerCtxt {
    fn main(name: RuaString) -> Self {
        Self {
            locals: Locals::default(),
            chunk: Chunk::default(),
            upvalues: Upvalues::default(),
            arity: 0,
            name,
        }
    }

    fn for_function(locals: Locals, arity: u8, name: RuaString) -> Self {
        Self { locals, chunk: Chunk::default(), upvalues: Upvalues::default(), arity, name }
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
enum Item {
    Reg(u8),
    Number(f64),
    String(RuaString),
    Function(FnHandle),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone, Copy)]
struct ExprDst(u8);
