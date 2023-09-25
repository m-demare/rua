use std::{error::Error, fmt};

use crate::{lex::tokens::{Token, TokenType}, identifiers::Identifier};
use super::ast::BlockType;

macro_rules! peek_token_is {
    ($tokens_it: expr, $($args: pat),+) => {
        match $tokens_it.peek() {
            $(
                Some(Token{ttype: $args}) => true,
            )+
            _ => false,
        }
    };
}

pub(super) use peek_token_is;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    IllegalToken(Box<str>),
    UnexpectedToken(Box<Token>, Box<[TokenType]>),
    UnexpectedTokenWithErrorMsg(Box<Token>, Box<str>),
    UnexpectedClose(Box<BlockType>, Box<Token>),
    NamedFunctionExpr(Identifier),
    UnnamedFunctionSt,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Self::IllegalToken(s) => format!("Illegal token {s}"),
            Self::UnexpectedToken(got, expected) => format!("Unexpected token. Got {:?}, expected {}{expected:?}", got.ttype, if expected.len() > 1 {"one of "} else {""}),
            Self::UnexpectedTokenWithErrorMsg(got, expected) => format!("Unexpected token. Got {:?}, expected {expected}", got.ttype),
            Self::UnexpectedClose(block_type, got) => format!("Cannot close block of type {block_type:?} with token {:?}", got.ttype),
            Self::NamedFunctionExpr(id) => format!("Function expression cannot have a name (got {id:?})"),
            Self::UnnamedFunctionSt => "Function statement must have a name".to_string(),
        };
        write!(f, "{res}")
    }
}

impl Error for ParseError { }

