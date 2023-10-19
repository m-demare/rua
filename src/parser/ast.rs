use std::{error::Error, fmt, rc::Rc};

use crate::{
    identifiers::Identifier,
    lex::tokens::{BinaryOp, Token, TokenType},
};

#[derive(PartialEq, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(Rc<str>),
    Nil,

    Not(Box<Expression>),
    Len(Box<Expression>),
    Neg(Box<Expression>),

    // Arithmetic operators
    Plus(Box<(Expression, Expression)>),
    Minus(Box<(Expression, Expression)>),
    Times(Box<(Expression, Expression)>),
    Div(Box<(Expression, Expression)>),
    Mod(Box<(Expression, Expression)>),
    Exp(Box<(Expression, Expression)>),

    // Comparison operators
    Eq(Box<(Expression, Expression)>),
    Neq(Box<(Expression, Expression)>),
    Le(Box<(Expression, Expression)>),
    Ge(Box<(Expression, Expression)>),
    Lt(Box<(Expression, Expression)>),
    Gt(Box<(Expression, Expression)>),

    // Logic operators
    And(Box<(Expression, Expression)>),
    Or(Box<(Expression, Expression)>),

    Dotdot(Box<(Expression, Expression)>),
    Function(Rc<[FunctionArg]>, Rc<[Statement]>),

    Call(Box<Expression>, Vec<Expression>),

    FieldAccess(Box<Expression>, Identifier),
}

#[derive(PartialEq, Eq, Debug)]
pub enum FunctionArg {
    Identifier(Identifier),
    Dotdotdot,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Local(Identifier, Option<Box<Expression>>),
    Assign(Identifier, Box<Expression>),
    Return(Option<Box<Expression>>),

    IfThen(Box<Expression>, Vec<Statement>),
    IfThenElse(Box<Expression>, Vec<Statement>, Vec<Statement>),
    IfThenElseIf(Box<[(Expression, Vec<Statement>)]>),

    Call(Box<Expression>, Vec<Expression>),
    While(Box<Expression>, Vec<Statement>),
    Break,
}

#[derive(Debug, PartialEq, PartialOrd, Eq)]
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
    FieldAccess, // foo.bar
}

#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub enum ExpressionContext {
    Assign,
    Call,
    Return,
    Condition,
    Group,
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

#[derive(PartialEq, Eq, Debug)]
pub enum BlockType {
    TopLevel,
    If,
    Else,
    Function,
    While,
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    IllegalToken(Box<str>),
    UnexpectedToken(Box<Token>, Box<[TokenType]>),
    UnexpectedTokenWithErrorMsg(Box<Token>, Box<str>),
    UnexpectedClose(Box<BlockType>, Box<Token>),
    NamedFunctionExpr(Identifier),
    UnnamedFunctionSt,
    UnexpectedExpression,
    UnexpectedEOF,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Self::IllegalToken(s) => format!("Illegal token {s}"),
            Self::UnexpectedToken(got, expected) => format!(
                "Unexpected token. Got {:?}, expected {}{expected:?}",
                got.ttype,
                if expected.len() > 1 { "one of " } else { "" }
            ),
            Self::UnexpectedTokenWithErrorMsg(got, expected) => {
                format!("Unexpected token. Got {:?}, expected {expected}", got.ttype)
            }
            Self::UnexpectedClose(block_type, got) => {
                format!("Cannot close block of type {block_type:?} with token {:?}", got.ttype)
            }
            Self::NamedFunctionExpr(id) => {
                format!("Function expression cannot have a name (got {id:?})")
            }
            Self::UnnamedFunctionSt => "Function statement must have a name".to_string(),
            Self::UnexpectedExpression => "Expected statement, got expression".to_string(),
            Self::UnexpectedEOF => "Unexpected end of file".to_string(),
        };
        write!(f, "{res}")
    }
}

impl Error for ParseError {}
