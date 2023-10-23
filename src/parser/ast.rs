use std::rc::Rc;

use rua_identifiers::Identifier;
use thiserror::Error;

use crate::lex::tokens::{BinaryOp, Token, TokenType};

#[derive(PartialEq, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct TableLiteral(
    pub Vec<Expression>,
    pub Vec<(Identifier, Expression)>,
    pub Vec<(Expression, Expression)>,
);

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(Rc<str>),
    TableLiteral(Box<TableLiteral>),
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
    Index(Box<(Expression, Expression)>),
}

#[derive(PartialEq, Eq, Debug)]
pub enum FunctionArg {
    Identifier(Identifier),
    Dotdotdot,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Local(Vec<Identifier>, Vec<Expression>),
    Assign(Vec<Expression>, Vec<Expression>),
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

#[derive(PartialEq, Eq, Debug)]
pub enum BlockType {
    TopLevel,
    If,
    Else,
    Function,
    While,
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("Illegal token {0}")]
    IllegalToken(Box<str>),
    #[error("Unexpected token. Got {0:?}, expected {}{1:?}", if .1.len() > 1 { "one of " } else { "" })]
    UnexpectedToken(Box<Token>, Box<[TokenType]>),
    #[error("Unexpected token. Got {:?}, expected {1}", .0.ttype)]
    UnexpectedTokenWithErrorMsg(Box<Token>, Box<str>),
    #[error("Cannot close block of type {0:?} with token {:?}", .1.ttype)]
    UnexpectedClose(Box<BlockType>, Box<Token>),
    #[error("Function expression cannot have a name (got {0:?})")]
    NamedFunctionExpr(Identifier),
    #[error("Function statement must have a name")]
    UnnamedFunctionSt,
    #[error("Expected statement, got expression")]
    UnexpectedExpression,
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("Only Identifier, FieldAccess or Index exprssions are allowed as assignment LHS")]
    InvalidAssignLHS,
}
