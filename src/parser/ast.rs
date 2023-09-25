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
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),
    Exp(Box<Expression>, Box<Expression>),

    // Comparison operators
    Eq(Box<Expression>, Box<Expression>),
    Neq(Box<Expression>, Box<Expression>),
    Le(Box<Expression>, Box<Expression>),
    Ge(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),

    // Logic operators
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),

    Dotdot(Box<Expression>, Box<Expression>),
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

