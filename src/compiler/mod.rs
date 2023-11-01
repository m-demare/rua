use std::{fmt::Debug, iter::Peekable};

use crate::{
    compiler::utils::{consume, debug_peek_token, peek_token_is},
    eval::vals::{string::RuaString, RuaVal},
    lex::{
        tokens::{BinaryOp, Token, TokenType as TT, UnaryOp},
        Tokenizer,
    },
};

use self::{
    bytecode::{Constant, Instruction as I, Instruction, ParseError, Program},
    locals::Locals,
};

pub mod bytecode;
mod locals;
mod tests;
mod utils;

pub struct Compiler<'vm, T: Iterator<Item = char> + Clone> {
    tokens: Peekable<Tokenizer<'vm, T>>,
    code: Vec<Instruction>,
    constants: Vec<RuaVal>,
    lines: Vec<(usize, usize)>,
    locals: Locals,
}

#[allow(unused_parens)]
impl<'vm, T: Iterator<Item = char> + Clone> Compiler<'vm, T> {
    pub fn new(tokens: Tokenizer<'vm, T>) -> Self {
        Self {
            tokens: tokens.peekable(),
            code: Vec::new(),
            constants: Vec::new(),
            lines: vec![(0, 0)],
            locals: Locals::new(),
        }
    }

    pub fn compile(mut self) -> Result<Program, ParseError> {
        self.block()?;
        self.instruction(I::ReturnNil, 0);
        Ok(Program::new(self.code, self.constants, self.lines))
    }

    fn block(&mut self) -> Result<(), ParseError> {
        self.locals.begin_scope();
        let mut end_line = 0;
        while let Some(token) = self.peek_token() {
            match token.clone() {
                Token { ttype: TT::RETURN, line, .. } => self.return_st(line)?,
                Token { ttype: TT::LOCAL, line, .. } => self.local_st(line)?,
                Token { ttype: TT::IDENTIFIER(..) | TT::LPAREN, line, .. } => {
                    self.assign_or_call_st(line)?
                }
                Token { ttype: TT::END | TT::ELSE | TT::ELSEIF, line, .. } => {
                    end_line = line;
                    break;
                }
                Token { ttype: TT::IF, line, .. } => self.if_st(line)?,
                Token { ttype: TT::WHILE, line, .. } => self.while_st(line)?,
                t => {
                    return Err(ParseError::UnexpectedToken(
                        Box::new(t),
                        Box::new([
                            TT::LOCAL,
                            TT::RETURN,
                            TT::SEMICOLON,
                            TT::END,
                            TT::ELSE,
                            TT::ELSEIF,
                        ]),
                    ))
                }
            }
        }
        let locals_in_scope = self.locals.end_scope();
        for _ in 0..locals_in_scope {
            self.instruction(I::Pop, end_line); // TODO popn?
        }
        Ok(())
    }

    fn expression(&mut self, precedence: Precedence) -> Result<(), ParseError> {
        self.prefix_exp()?;
        self.infix_exp(precedence)
    }

    fn group_expression(&mut self) -> Result<(), ParseError> {
        self.expression(Precedence::Lowest)?;
        consume!(self, (TT::RPAREN));
        Ok(())
    }

    fn current_chunk(
        &mut self,
    ) -> (&mut Vec<Instruction>, &mut Vec<RuaVal>, &mut Vec<(usize, usize)>) {
        (&mut self.code, &mut self.constants, &mut self.lines)
    }

    fn emit_constant(&mut self, val: RuaVal, line: usize) {
        let c = Constant(self.constants.len().try_into().expect("Too many constants in one chunk")); // TODO handle gracefully
        let (_, constants, _) = self.current_chunk();
        constants.push(val); // TODO maybe reuse constants?
        self.instruction(I::Constant(c), line);
    }

    fn unary(&mut self, op: UnaryOp, line: usize) -> Result<(), ParseError> {
        self.expression(Precedence::Prefix)?;
        match op {
            UnaryOp::NOT => self.instruction(I::Not, line),
            UnaryOp::LEN => self.instruction(I::StrLen, line),
        };
        Ok(())
    }

    fn binary(&mut self, op: BinaryOp, line: usize) -> Result<(), ParseError> {
        let precedence = precedence_of_binary(&op);
        macro_rules! binary_operator {
            ($compiler: expr, $precedence: expr, $instr: expr, $line: expr) => {{
                $compiler.expression($precedence)?;
                $compiler.instruction($instr, $line);
            }};
        }
        match op {
            BinaryOp::PLUS => binary_operator!(self, precedence, I::Add, line),
            BinaryOp::TIMES => binary_operator!(self, precedence, I::Mul, line),
            BinaryOp::DIV => binary_operator!(self, precedence, I::Div, line),
            BinaryOp::MOD => todo!(),
            BinaryOp::EXP => binary_operator!(self, precedence, I::Pow, line),
            BinaryOp::EQ => binary_operator!(self, precedence, I::Eq, line),
            BinaryOp::NEQ => binary_operator!(self, precedence, I::Neq, line),
            BinaryOp::LE => binary_operator!(self, precedence, I::Le, line),
            BinaryOp::GE => binary_operator!(self, precedence, I::Ge, line),
            BinaryOp::LT => binary_operator!(self, precedence, I::Lt, line),
            BinaryOp::GT => binary_operator!(self, precedence, I::Gt, line),
            BinaryOp::AND => self.and(line)?,
            BinaryOp::OR => self.or(line)?,
            BinaryOp::DOTDOT => binary_operator!(self, precedence, I::StrConcat, line),
        };
        Ok(())
    }

    fn prefix_exp(&mut self) -> Result<(), ParseError> {
        match self.next_token() {
            Some(Token { ttype: TT::UNARY_OP(op), line, .. }) => return self.unary(op, line),
            Some(Token { ttype: TT::MINUS, line, .. }) => {
                self.expression(Precedence::Prefix)?;
                self.instruction(I::Neg, line);
            }
            Some(Token { ttype: TT::IDENTIFIER(id), line, .. }) => {
                self.named_variable(id, line);
            }
            Some(Token { ttype: TT::NUMBER(n), line, .. }) => {
                self.emit_constant(n.into(), line);
            }
            Some(Token { ttype: TT::STRING(s), line, .. }) => {
                self.emit_constant(s.into(), line);
            }
            Some(Token { ttype: TT::TRUE, line, .. }) => {
                self.instruction(I::True, line);
            }
            Some(Token { ttype: TT::FALSE, line, .. }) => {
                self.instruction(I::False, line);
            }
            Some(Token { ttype: TT::NIL, line, .. }) => {
                self.instruction(I::Nil, line);
            }
            Some(Token { ttype: TT::LPAREN, .. }) => return self.group_expression(),
            Some(t) => todo!("{t:?}"),
            None => return Err(ParseError::UnexpectedEOF),
        }
        Ok(())
    }

    fn named_variable(&mut self, id: RuaString, line: usize) {
        let local = self.locals.resolve(&id);
        match local {
            Some(local) => {
                self.instruction(I::GetLocal(local), line);
            }
            None => {
                self.emit_constant(id.into(), line);
                self.instruction(I::GetGlobal, line);
            }
        }
    }

    fn instruction(&mut self, instr: Instruction, line: usize) -> usize {
        let (code, _, lines) = self.current_chunk();
        code.push(instr);
        // lines is always non-empty
        let last_line = unsafe { lines.last_mut().unwrap_unchecked() };
        if last_line.0 == line {
            last_line.1 = last_line.1 + 1;
        } else {
            lines.push((line, 1));
        }
        code.len() - 1
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        let (code, _, lines) = self.current_chunk();
        debug_assert!(code.len() > 0 && lines.len() > 0);
        // lines is always non-empty
        let last_line = unsafe { lines.last_mut().unwrap_unchecked() };
        if last_line.1 > 1 {
            last_line.1 = last_line.1 - 1;
        } else {
            lines.pop();
        }
        code.pop()
    }

    fn infix_exp(&mut self, precedence: Precedence) -> Result<(), ParseError> {
        loop {
            match self.peek_token().cloned() {
                Some(Token { ttype: TT::BINARY_OP(op), line, .. }) => {
                    if precedence < precedence_of_binary(&op) {
                        self.next_token();
                        self.binary(op, line)?;
                    } else {
                        break Ok(());
                    }
                }
                Some(Token { ttype: TT::MINUS, line, .. }) => {
                    if precedence < Precedence::Sum {
                        self.next_token();
                        self.expression(precedence)?;
                        self.instruction(I::Sub, line);
                    } else {
                        break Ok(());
                    }
                }
                Some(Token { ttype: TT::LPAREN, line, .. }) => {
                    if precedence < Precedence::Call {
                        self.call_expr(line)?;
                    } else {
                        break Ok(());
                    }
                }
                _ => break Ok(()),
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

        self.expression(Precedence::Lowest)?;
        match self.peek_token() {
            Some(Token { ttype: TT::END | TT::ELSE | TT::ELSEIF, .. }) | None => {}
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.clone().into(),
                    [TT::END, TT::ELSE, TT::ELSEIF].into(),
                ))
            }
        }
        self.instruction(I::Return, line);
        Ok(())
    }

    fn local_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::LOCAL);
        self.next_token();

        let id = self.identifier()?;
        if peek_token_is!(self, TT::ASSIGN) {
            self.next_token();
            self.expression(Precedence::Lowest)?;
        } else {
            self.instruction(I::Nil, line);
        }
        self.locals.declare(id)?;

        Ok(())
    }

    fn identifier(&mut self) -> Result<RuaString, ParseError> {
        match self.next_token() {
            Some(Token { ttype: TT::IDENTIFIER(id), .. }) => Ok(id),
            Some(t) => {
                return Err(ParseError::UnexpectedToken(t.into(), [TT::IDENTIFIER_DUMMY].into()))
            }
            None => return Err(ParseError::UnexpectedEOF),
        }
    }

    fn peek_token(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn next_token(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn assign_or_call_st(&mut self, line: usize) -> Result<(), ParseError> {
        self.expression(Precedence::Lowest)?;
        match self.peek_token().cloned() {
            Some(Token { ttype: TT::ASSIGN, line, .. }) => match self.code.last().cloned() {
                Some(I::GetGlobal) => {
                    self.pop_instruction();
                    debug_assert!(matches!(self.code.last(), Some(I::Constant(_))));
                    let t = self.next_token();
                    debug_assert!(matches!(t, Some(Token { ttype: TT::ASSIGN, .. })));
                    self.expression(Precedence::Lowest)?;
                    self.instruction(I::SetGlobal, line);
                }
                Some(I::GetLocal(idx)) => {
                    self.pop_instruction();
                    let t = self.next_token();
                    debug_assert!(matches!(t, Some(Token { ttype: TT::ASSIGN, .. })));
                    self.expression(Precedence::Lowest)?;
                    self.instruction(I::SetLocal(idx), line);
                }
                Some(_) => todo!("handle field access; return error if invalid"),
                None => unreachable!("Just parsed an expression"),
            },
            _ => {
                match self.code.last().expect("Just parsed an expression") {
                    I::Call(_) => {
                        // It's a call statement, gotta pop the return value
                        self.instruction(I::Pop, line);
                    }
                    _ => return Err(ParseError::UnexpectedExpression),
                }
            }
        }

        Ok(())
    }

    fn call_expr(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::LPAREN);
        self.next_token();
        let mut cant_args = 0;
        loop {
            match self.peek_token() {
                Some(Token { ttype: TT::RPAREN, .. }) => break,
                Some(_) => {
                    if cant_args == u16::MAX {
                        todo!("Return a parse error")
                    }
                    cant_args += 1;
                    self.expression(Precedence::Lowest)?;
                    match self.peek_token() {
                        Some(Token { ttype: TT::RPAREN, .. }) => break,
                        Some(Token { ttype: TT::COMMA, .. }) => {
                            self.next_token();
                        }
                        Some(t) => {
                            return Err(ParseError::UnexpectedToken(
                                t.clone().into(),
                                [TT::RPAREN, TT::COMMA].into(),
                            ))
                        }
                        None => return Err(ParseError::UnexpectedEOF),
                    };
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
        debug_peek_token!(self, TT::RPAREN);
        self.next_token();
        self.instruction(I::Call(cant_args), line);
        Ok(())
    }

    fn if_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::IF);
        self.next_token();
        self.expression(Precedence::Lowest)?;
        consume!(self, (TT::THEN));
        let if_jmp = self.jmp_if_false_pop(i32::MAX, line);
        self.block()?;

        match self.next_token() {
            Some(Token { ttype: TT::END, .. }) => {
                self.patch_jmp(if_jmp)?;
            }
            Some(Token { ttype: TT::ELSE, .. }) => {
                let else_jmp = self.jmp(0, line);
                self.patch_jmp(if_jmp)?;
                self.block()?;
                self.patch_jmp(else_jmp)?;
                consume!(self, (TT::END));
            }
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.clone().into(),
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
        let loop_start = self.current_chunk().0.len();
        self.expression(Precedence::Lowest)?;
        consume!(self, (TT::DO));
        let exit_jmp = self.jmp_if_false_pop(i32::MAX, line);
        self.block()?;

        let offset = Self::offset(loop_start, self.current_chunk().0.len())?;
        self.loop_to(offset, line);
        self.patch_jmp(exit_jmp)?;
        consume!(self, (TT::END));

        Ok(())
    }

    fn jmp(&mut self, to: i32, line: usize) -> usize {
        self.instruction(I::Jmp(to), line)
    }

    fn loop_to(&mut self, to: i32, line: usize) -> usize {
        self.jmp(to, line)
    }

    fn jmp_if_false_pop(&mut self, to: i32, line: usize) -> usize {
        self.instruction(I::JmpIfFalsePop(to), line)
    }

    fn jmp_if_false(&mut self, to: i32, line: usize) -> usize {
        self.instruction(I::JmpIfFalse(to), line)
    }

    fn jmp_if_true(&mut self, to: i32, line: usize) -> usize {
        self.instruction(I::JmpIfTrue(to), line)
    }

    fn offset(from: usize, to: usize) -> Result<i32, ParseError> {
        let offset = from as isize - to as isize;
        offset.try_into().or(Err(ParseError::JmpTooFar))
    }

    fn patch_jmp(&mut self, jmp: usize) -> Result<(), ParseError> {
        let (code, _, _) = self.current_chunk();
        debug_assert!(matches!(
            code[jmp],
            I::JmpIfTrue(_) | I::JmpIfFalsePop(_) | I::JmpIfFalse(_) | I::Jmp(_)
        ));

        let offset = Self::offset(code.len(), jmp)?;

        match code[jmp] {
            I::JmpIfFalsePop(_) => code[jmp] = I::JmpIfFalsePop(offset),
            I::JmpIfFalse(_) => code[jmp] = I::JmpIfFalse(offset),
            I::JmpIfTrue(_) => code[jmp] = I::JmpIfTrue(offset),
            I::Jmp(_) => code[jmp] = I::Jmp(offset),
            _ => unreachable!(),
        }

        Ok(())
    }

    fn and(&mut self, line: usize) -> Result<(), ParseError> {
        let jmp = self.jmp_if_false(0, line);
        self.instruction(I::Pop, line);

        self.expression(Precedence::And)?;

        self.patch_jmp(jmp)?;
        Ok(())
    }

    fn or(&mut self, line: usize) -> Result<(), ParseError> {
        let jmp = self.jmp_if_true(0, line);
        self.instruction(I::Pop, line);

        self.expression(Precedence::Or)?;

        self.patch_jmp(jmp)?;

        Ok(())
    }
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

pub(super) const fn precedence_of_binary(op: &BinaryOp) -> Precedence {
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
