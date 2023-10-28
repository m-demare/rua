use std::{fmt::Debug, iter::Peekable};

use crate::{
    compiler::utils::{consume, debug_peek_token, peek_token_is},
    eval::vals::RuaVal,
    lex::{
        tokens::{BinaryOp, Token, TokenType as TT, UnaryOp},
        Tokenizer,
    },
};

use self::bytecode::{Constant, Instruction as I, Instruction, ParseError, Program};

pub mod bytecode;
mod tests;
mod utils;

pub struct Compiler<'vm, T: Iterator<Item = char> + Clone> {
    tokens: Peekable<Tokenizer<'vm, T>>,
    code: Vec<Instruction>,
    constants: Vec<RuaVal>,
    lines: Vec<(usize, usize)>,
}

#[allow(unused_parens)]
impl<'vm, T: Iterator<Item = char> + Clone> Compiler<'vm, T> {
    pub fn new(tokens: Tokenizer<'vm, T>) -> Self {
        Self {
            tokens: tokens.peekable(),
            code: Vec::new(),
            constants: Vec::new(),
            lines: vec![(0, 0)],
        }
    }

    pub fn compile(mut self) -> Result<Program, ParseError> {
        self.block()?;
        Ok(Program::new(self.code, self.constants, self.lines))
    }

    fn block(&mut self) -> Result<(), ParseError> {
        while let Some(token) = self.peek_token() {
            match token.clone() {
                Token { ttype: TT::RETURN, line, .. } => self.return_st(line)?,
                Token { ttype: TT::LOCAL, line, .. } => self.local_st(line)?,
                Token { ttype: TT::IDENTIFIER(..) | TT::LPAREN, line, .. } => {
                    self.assign_or_call_st(line)?
                }
                Token { ttype: TT::END | TT::ELSE | TT::ELSEIF, .. } => break,
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
        }
        Ok(())
    }

    fn binary(&mut self, op: BinaryOp, line: usize) -> Result<(), ParseError> {
        let precedence = precedence_of_binary(&op);
        self.expression(precedence)?;
        match op {
            BinaryOp::PLUS => self.instruction(I::Add, line),
            BinaryOp::TIMES => self.instruction(I::Mul, line),
            BinaryOp::DIV => self.instruction(I::Div, line),
            BinaryOp::MOD => todo!(),
            BinaryOp::EXP => self.instruction(I::Pow, line),
            BinaryOp::EQ => self.instruction(I::Eq, line),
            BinaryOp::NEQ => {
                self.instruction(I::Neq, line);
            }
            BinaryOp::LE => {
                self.instruction(I::Le, line);
            }
            BinaryOp::GE => {
                self.instruction(I::Ge, line);
            }
            BinaryOp::LT => self.instruction(I::Lt, line),
            BinaryOp::GT => self.instruction(I::Gt, line),
            BinaryOp::AND => todo!(),
            BinaryOp::OR => todo!(),
            BinaryOp::DOTDOT => self.instruction(I::StrConcat, line),
        }
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
                self.emit_constant(id.into(), line);
                self.instruction(I::GetGlobal, line);
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

    fn instruction(&mut self, instr: Instruction, line: usize) {
        let (code, _, lines) = self.current_chunk();
        code.push(instr);
        // lines is always non-empty
        let last_line = unsafe { lines.last_mut().unwrap_unchecked() };
        if last_line.0 == line {
            last_line.1 = last_line.1 + 1;
        } else {
            lines.push((line, 1));
        }
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        let (code, _, lines) = self.current_chunk();
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
            self.instruction(I::Return, line);
            return Ok(());
        }

        self.expression(Precedence::Lowest)?;
        consume!(self; allow_eof, (TT::END), (TT::ELSE), (TT::ELSEIF));
        self.instruction(I::Return, line);
        Ok(())
    }

    fn local_st(&mut self, line: usize) -> Result<(), ParseError> {
        debug_peek_token!(self, TT::LOCAL);
        self.next_token();

        self.identifier()?;
        if peek_token_is!(self, TT::ASSIGN) {
            self.next_token();
            self.expression(Precedence::Lowest)?;
        } else {
            self.instruction(I::Nil, line);
        }
        self.instruction(I::DefineLocal, line);
        Ok(())
    }

    fn identifier(&mut self) -> Result<(), ParseError> {
        match self.next_token() {
            Some(Token { ttype: TT::IDENTIFIER(id), line, .. }) => {
                Ok(self.emit_constant(id.into(), line))
            }
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
            Some(Token { ttype: TT::ASSIGN, line, .. }) => {
                match self.code.last().expect("Just parsed an expression") {
                    I::GetGlobal => {
                        self.pop_instruction();
                        debug_assert!(matches!(self.code.last(), Some(I::Constant(_))));
                        let t = self.next_token();
                        debug_assert!(matches!(t, Some(Token { ttype: TT::ASSIGN, .. })));
                        self.expression(Precedence::Lowest)?;
                        self.instruction(I::SetGlobal, line);
                    }
                    _ => todo!("handle field access; return error if invalid"),
                }
            }
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
