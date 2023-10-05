use crate::{lex::tokens::BinaryOp, identifiers::Identifier};

#[derive(PartialEq, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Identifier(Identifier),
    NumberLiteral(f64),
    BooleanLiteral(bool),
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
    Function(Box<[FunctionArg]>, Vec<Statement>),

    Call(Box<Expression>, Vec<Expression>),
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
}

#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub enum Precedence {
    Lowest,
    Or,
    And,
    Comparator, // >, <, <=, >=, ~=, ==
    Dotdot,     // ..
    Sum,        // +, -
    Product,    // *, /, %
    Prefix,     // not, #, - (unary)
    Exp,        // ^
    Call,        // foo(...)
}

#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub enum ExpressionContext {
    Assign,
    Call,
    Return,
    Condition,
    Group,
}

pub const fn precedence_of_binary(op: &BinaryOp) -> Precedence {
    match op {
        BinaryOp::OR => Precedence::Or,
        BinaryOp::AND => Precedence::And,
        BinaryOp::EQ | BinaryOp::NEQ | BinaryOp::LE | BinaryOp::GE |
            BinaryOp::LT | BinaryOp::GT => Precedence::Comparator,
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

