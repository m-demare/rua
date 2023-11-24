#![cfg(test)]

use pretty_assertions::assert_eq;

use super::tokens::TokenType as TT;
use super::{
    tokens::{BinaryOp, Token, UnaryOp},
    Tokenizer,
};
use crate::eval::Vm;

macro_rules! test_lex {
    ($input: expr, $expected_output: expr) => {
        let mut vm = Vm::new();
        #[allow(unused_macros)]
        macro_rules! id {
            ($s: expr) => {
                match vm.identifiers().add_or_get($s, TT::IDENTIFIER_DUMMY) {
                    TT::IDENTIFIER(i) => TT::IDENTIFIER(i.clone()),
                    t => panic!("Expected identifier, got {t:?}"),
                }
            };
        }
        #[allow(unused_macros)]
        macro_rules! string {
            ($s: expr) => {
                TT::STRING(vm.new_string((*$s).into()))
            };
        }

        let tokens = Tokenizer::new($input.bytes(), &mut vm).collect::<Vec<_>>();
        assert_eq!(tokens, $expected_output)
    };
}

#[test]
fn lex_basic_assignment() {
    test_lex!(
        "local foo = bar",
        vec![
            Token { ttype: TT::LOCAL, line: 1 },
            Token { ttype: id!(b"foo"), line: 1 },
            Token { ttype: TT::ASSIGN, line: 1 },
            Token { ttype: id!(b"bar"), line: 1 },
        ]
    );
}

#[test]
fn lex_ints() {
    test_lex!(
        "58
        0x2ae2
        0b1001
        0b158",
        vec![
            Token { ttype: TT::NUMBER(58.0), line: 1 },
            Token { ttype: TT::NUMBER(f64::from(0x2ae2)), line: 2 },
            Token { ttype: TT::NUMBER(9.0), line: 3 },
            Token { ttype: TT::ILLEGAL("0b15".into()), line: 4 },
            Token { ttype: TT::NUMBER(8.0), line: 4 },
        ]
    );
}

#[test]
fn lex_floats() {
    test_lex!(
        "0.58
        0x0.2A
        0b0.1
        0b0.23",
        vec![
            Token { ttype: TT::NUMBER(0.58), line: 1 },
            Token { ttype: TT::NUMBER(0.164_062_5), line: 2 },
            Token { ttype: TT::ILLEGAL("0b0.".into()), line: 3 },
            Token { ttype: TT::NUMBER(1.0), line: 3 },
            Token { ttype: TT::ILLEGAL("0b0.".into()), line: 4 },
            Token { ttype: TT::NUMBER(23.0), line: 4 },
        ]
    );
}

#[test]
fn lex_exp() {
    test_lex!(
        "
        0.58e5
        .58e+5
        0.58E-52
        10E30
        10e+3
        10e-3",
        vec![
            Token { ttype: TT::NUMBER(0.58e5), line: 2 },
            Token { ttype: TT::NUMBER(0.58e5), line: 3 },
            Token { ttype: TT::NUMBER(0.58e-52), line: 4 },
            Token { ttype: TT::NUMBER(10e30), line: 5 },
            Token { ttype: TT::NUMBER(10e3), line: 6 },
            Token { ttype: TT::NUMBER(10e-3), line: 7 },
        ]
    );
}

#[test]
fn lex_ops() {
    test_lex!(
        "a = a <= b and a+b or a^b
        (c-d>e);#f",
        vec![
            Token { ttype: id!(b"a"), line: 1 },
            Token { ttype: TT::ASSIGN, line: 1 },
            Token { ttype: id!(b"a"), line: 1 },
            Token { ttype: TT::BINARY_OP(BinaryOp::LE), line: 1 },
            Token { ttype: id!(b"b"), line: 1 },
            Token { ttype: TT::BINARY_OP(BinaryOp::AND), line: 1 },
            Token { ttype: id!(b"a"), line: 1 },
            Token { ttype: TT::BINARY_OP(BinaryOp::PLUS), line: 1 },
            Token { ttype: id!(b"b"), line: 1 },
            Token { ttype: TT::BINARY_OP(BinaryOp::OR), line: 1 },
            Token { ttype: id!(b"a"), line: 1 },
            Token { ttype: TT::BINARY_OP(BinaryOp::EXP), line: 1 },
            Token { ttype: id!(b"b"), line: 1 },
            Token { ttype: TT::LPAREN, line: 2 },
            Token { ttype: id!(b"c"), line: 2 },
            Token { ttype: TT::MINUS, line: 2 },
            Token { ttype: id!(b"d"), line: 2 },
            Token { ttype: TT::BINARY_OP(BinaryOp::GT), line: 2 },
            Token { ttype: id!(b"e"), line: 2 },
            Token { ttype: TT::RPAREN, line: 2 },
            Token { ttype: TT::SEMICOLON, line: 2 },
            Token { ttype: TT::UNARY_OP(UnaryOp::LEN), line: 2 },
            Token { ttype: id!(b"f"), line: 2 },
        ]
    );
}

#[test]
fn lex_comment() {
    test_lex!(
        "a = b -- a+b\n--123",
        vec![
            Token { ttype: id!(b"a"), line: 1 },
            Token { ttype: TT::ASSIGN, line: 1 },
            Token { ttype: id!(b"b"), line: 1 },
        ]
    );
}

#[test]
fn lex_dots() {
    test_lex!(
        ".\n..\n...\n.123",
        vec![
            Token { ttype: TT::DOT, line: 1 },
            Token { ttype: TT::BINARY_OP(BinaryOp::DOTDOT), line: 2 },
            Token { ttype: TT::DOTDOTDOT, line: 3 },
            Token { ttype: TT::NUMBER(0.123), line: 4 },
        ]
    );
}

#[test]
fn lex_illegals() {
    test_lex!(
        "0b2 0b1e 5e3a ! @ ~",
        vec![
            Token { ttype: TT::ILLEGAL("0b2".into()), line: 1 },
            Token { ttype: TT::ILLEGAL("0b1e".into()), line: 1 },
            Token { ttype: TT::ILLEGAL("5e3a".into()), line: 1 },
            Token { ttype: TT::ILLEGAL("!".into()), line: 1 },
            Token { ttype: TT::ILLEGAL("@".into()), line: 1 },
            Token { ttype: TT::ILLEGAL("~".into()), line: 1 },
        ]
    );
}

#[test]
fn lex_strings() {
    test_lex!(
        r#"
        "hello'world"
        'foo"bar'
        "test\"string"
        'test\'string'
        'test\\\'
string\'\"'
        'test
    "#,
        vec![
            Token { ttype: string!(b"hello'world"), line: 2 },
            Token { ttype: string!(b"foo\"bar"), line: 3 },
            Token { ttype: string!(b"test\"string"), line: 4 },
            Token { ttype: string!(b"test'string"), line: 5 },
            Token { ttype: string!(b"test\\'\nstring'\""), line: 7 },
            Token { ttype: TT::ILLEGAL("Unclosed string literal".into()), line: 9 },
        ]
    );
}
