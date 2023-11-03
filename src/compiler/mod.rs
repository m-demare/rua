use std::fmt::Debug;

use crate::{
    eval::{
        vals::{function::Function, string::RuaString, RuaVal},
        Vm,
    },
    lex::{
        tokens::{BinaryOp, Token, TokenType as TT, UnaryOp},
        Tokenizer,
    },
};

use self::{
    bytecode::{Chunk, Instruction as I, Instruction, ParseError},
    locals::Locals,
    utils::{consume, debug_peek_token, match_token, peek_token_is},
};

pub mod bytecode;
mod locals;
mod tests;
mod utils;

enum FnType {
    Function(u8, RuaString),
    Script,
}

pub struct Compiler<'vm, T: Iterator<Item = char> + Clone> {
    tokens: &'vm mut MyPeekable<Tokenizer<'vm, T>>,
    current_chunk: Chunk,
    locals: Locals,
    fn_type: FnType,
}

#[allow(unused_parens)]
impl<'vm, T: Iterator<Item = char> + Clone> Compiler<'vm, T> {
    fn new(tokens: &'vm mut MyPeekable<Tokenizer<'vm, T>>) -> Self {
        Self {
            tokens,
            current_chunk: Chunk::default(),
            locals: Locals::new(),
            fn_type: FnType::Script,
        }
    }

    fn for_function(
        tokens: &'vm mut MyPeekable<Tokenizer<'vm, T>>,
        locals: Locals,
        name: RuaString,
        arity: u8,
    ) -> Self {
        Self {
            tokens,
            current_chunk: Chunk::default(),
            locals,
            fn_type: FnType::Function(arity, name),
        }
    }

    pub fn compile(mut self) -> Result<Function, ParseError> {
        self.block()?;
        self.instruction(I::ReturnNil, 0);
        Ok(match self.fn_type {
            FnType::Function(arity, name) => Function::new(self.current_chunk, arity, name),
            FnType::Script => {
                Function::new(self.current_chunk, 0, self.tokens.inner().vm().new_string("".into()))
            }
        })
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
                Token { ttype: TT::FUNCTION, line, .. } => {
                    self.next_token();
                    self.function_st(line, false)?
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

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.current_chunk
    }

    fn emit_constant(&mut self, val: RuaVal, line: usize) {
        self.current_chunk().add_constant(val, line);
    }

    fn unary(&mut self, op: UnaryOp, line: usize) -> Result<(), ParseError> {
        self.expression(Precedence::Prefix)?;
        match op {
            UnaryOp::NOT => self.instruction(I::Not, line),
            UnaryOp::LEN => self.instruction(I::Len, line),
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
        self.current_chunk().add_instruction(instr, line)
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        self.current_chunk().pop_instruction()
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

        let id = match self.next_token() {
            Some(Token { ttype: TT::FUNCTION, line, .. }) => return self.function_st(line, true),
            Some(Token { ttype: TT::IDENTIFIER(id), .. }) => id.clone(),
            Some(t) => {
                return Err(ParseError::UnexpectedToken(
                    t.clone().into(),
                    [TT::IDENTIFIER_DUMMY, TT::FUNCTION].into(),
                ))
            }
            None => return Err(ParseError::UnexpectedEOF),
        };
        if match_token!(self, TT::ASSIGN) {
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
            Some(Token { ttype: TT::ASSIGN, line, .. }) => {
                match self.current_chunk().code().last().cloned() {
                    Some(I::GetGlobal) => {
                        self.pop_instruction();
                        debug_assert!(matches!(
                            self.current_chunk().code().last(),
                            Some(I::Constant(_))
                        ));
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
                }
            }
            _ => {
                match self.current_chunk().code().last().expect("Just parsed an expression") {
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
        let cant_args = self.arg_list()?;
        self.instruction(I::Call(cant_args), line);
        Ok(())
    }

    fn arg_list(&mut self) -> Result<u16, ParseError> {
        let mut cant_args = 0;
        if match_token!(self, TT::RPAREN) {
            return Ok(cant_args);
        }
        while ({
            cant_args += 1;
            self.expression(Precedence::Lowest)?;
            match_token!(self, TT::COMMA)
        }) {}
        consume!(self, (TT::RPAREN));
        Ok(cant_args)
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
        let loop_start = self.current_chunk().code().len();
        self.expression(Precedence::Lowest)?;
        consume!(self, (TT::DO));
        let exit_jmp = self.jmp_if_false_pop(i32::MAX, line);
        self.block()?;

        let offset = Chunk::offset(loop_start, self.current_chunk().code().len())?;
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

    fn patch_jmp(&mut self, jmp: usize) -> Result<(), ParseError> {
        self.current_chunk().patch_jmp(jmp)
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

    fn function_st(&mut self, line: usize, local: bool) -> Result<(), ParseError> {
        let id = self.identifier()?;
        let function = self.function(id.clone())?;

        if local {
            self.emit_constant(RuaVal::Function(function), line);
            self.locals.declare(id)?;
        } else {
            self.emit_constant(id.into(), line);
            self.emit_constant(RuaVal::Function(function), line);
            self.instruction(I::SetGlobal, line);
        }
        Ok(())
    }

    fn function(&mut self, id: RuaString) -> Result<Function, ParseError> {
        consume!(self, (TT::LPAREN));
        let mut locals = Locals::new();
        locals.declare(id.clone())?;
        let arity = self.parameter_list(&mut locals)?;

        // I'm not sure why it doesn't realize tokens has the correct lifetime,
        // I may be doing something dumb here
        let tokens = unsafe {
            std::mem::transmute::<
                &mut MyPeekable<Tokenizer<'vm, T>>,
                &'vm mut MyPeekable<Tokenizer<'vm, T>>,
            >(self.tokens)
        };
        let compiler = Self::for_function(tokens, locals, id, arity);
        let retval = compiler.compile();
        consume!(self, (TT::END));
        retval
    }

    fn parameter_list(&mut self, locals: &mut Locals) -> Result<u8, ParseError> {
        let mut arity = 0;
        if match_token!(self, TT::RPAREN) {
            return Ok(arity);
        }
        while ({
            arity += 1;
            locals.declare(self.identifier()?)?;
            match_token!(self, TT::COMMA)
        }) {}
        consume!(self, (TT::RPAREN));
        Ok(arity)
    }
}

pub fn compile<C: Iterator<Item = char> + Clone>(
    chars: C,
    vm: &mut Vm,
) -> Result<Function, ParseError> {
    let tokens = Tokenizer::new(chars, vm);
    let mut tokens = MyPeekable::new(tokens);
    let compiler = Compiler::new(&mut tokens);
    compiler.compile()
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

struct MyPeekable<It: Iterator> {
    inner: It,
    peeked: Option<It::Item>,
}

impl<It: Iterator> MyPeekable<It> {
    fn new(mut inner: It) -> Self {
        let peeked = inner.next();
        Self { inner, peeked }
    }

    fn peek(&mut self) -> Option<&It::Item> {
        self.peeked.as_ref()
    }

    fn inner(&mut self) -> &mut It {
        &mut self.inner
    }
}

impl<It: Iterator> Iterator for MyPeekable<It> {
    type Item = It::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.peeked.take();
        self.peeked = self.inner.next();
        next
    }
}
