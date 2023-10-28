#![cfg(test)]

use pretty_assertions::assert_eq;

use super::{tokens::{Token, BinaryOp, UnaryOp}, Tokenizer};
use super::tokens::TokenType as TT;
use crate::eval::Vm;

macro_rules! test_lex {
    ($input: expr, $expected_output: expr) => {
        let mut vm = Vm::new();
        #[allow(unused_macros)]
        macro_rules! id {
            ($s: expr) => (match vm.identifiers().add_or_get($s, TT::IDENTIFIER_DUMMY) {
                TT::IDENTIFIER(i) => TT::IDENTIFIER(i),
                t => panic!("Expected identifier, got {t:?}")
            })
        }
        #[allow(unused_macros)]
        macro_rules! string {
            ($s: expr) => { TT::STRING(vm.new_string($s.into())) }
        }

        let tokens = Tokenizer::new($input.chars(), &mut vm).collect::<Vec<_>>();
        assert_eq!(tokens, $expected_output)
    };
}

#[test]
fn lex_basic_assignment() {
    test_lex!("local foo = bar", vec![
        Token { ttype: TT::LOCAL, line: 0},
        Token { ttype: id!("foo"), line: 0},
        Token { ttype: TT::ASSIGN, line: 0},
        Token { ttype: id!("bar"), line: 0},
    ]);
}

#[test]
fn lex_ints() {
    test_lex!(
        "58
        0x2A
        0b1001
        0b158", vec![
        Token { ttype: TT::NUMBER(58.0), line: 0},
        Token { ttype: TT::NUMBER(42.0), line: 1},
        Token { ttype: TT::NUMBER(9.0), line: 2},
        Token { ttype: TT::ILLEGAL("0b15".into()), line: 3},
        Token { ttype: TT::NUMBER(8.0), line: 3},
    ]);
}

#[test]
fn lex_floats() {
    test_lex!(
        "0.58
        0x0.2A
        0b0.1
        0b0.23", vec![
        Token { ttype: TT::NUMBER(0.58), line: 0},
        Token { ttype: TT::NUMBER(0.164_062_5), line: 1},
        Token { ttype: TT::ILLEGAL("0b0.".into()), line: 2},
        Token { ttype: TT::NUMBER(1.0), line: 2},
        Token { ttype: TT::ILLEGAL("0b0.".into()), line: 3},
        Token { ttype: TT::NUMBER(23.0), line: 3},
    ]);
}

#[test]
fn lex_ops() {
    test_lex!(
        "a = a <= b and a+b or a^b
        (c-d>e);#f", vec![
            Token { ttype: id!("a"), line: 0},
            Token { ttype: TT::ASSIGN, line: 0},
            Token { ttype: id!("a"), line: 0},
            Token { ttype: TT::BINARY_OP(BinaryOp::LE), line: 0},
            Token { ttype: id!("b"), line: 0},
            Token { ttype: TT::BINARY_OP(BinaryOp::AND), line: 0},
            Token { ttype: id!("a"), line: 0},
            Token { ttype: TT::BINARY_OP(BinaryOp::PLUS), line: 0},
            Token { ttype: id!("b"), line: 0},
            Token { ttype: TT::BINARY_OP(BinaryOp::OR), line: 0},
            Token { ttype: id!("a"), line: 0},
            Token { ttype: TT::BINARY_OP(BinaryOp::EXP), line: 0},
            Token { ttype: id!("b"), line: 0},
            Token { ttype: TT::LPAREN, line: 1},
            Token { ttype: id!("c"), line: 1},
            Token { ttype: TT::MINUS, line: 1},
            Token { ttype: id!("d"), line: 1},
            Token { ttype: TT::BINARY_OP(BinaryOp::GT), line: 1},
            Token { ttype: id!("e"), line: 1},
            Token { ttype: TT::RPAREN, line: 1},
            Token { ttype: TT::SEMICOLON, line: 1},
            Token { ttype: TT::UNARY_OP(UnaryOp::LEN), line: 1},
            Token { ttype: id!("f"), line: 1},
        ]);
}

#[test]
fn lex_comment() {
    test_lex!("a = b -- a+b\n--123", vec![
        Token { ttype: id!("a"), line: 0},
        Token { ttype: TT::ASSIGN, line: 0},
        Token { ttype: id!("b"), line: 0},
    ]);
}

#[test]
fn lex_dots() {
    test_lex!(".\n..\n...\n.123", vec![
        Token { ttype: TT::DOT, line: 0},
        Token { ttype: TT::BINARY_OP(BinaryOp::DOTDOT), line: 1},
        Token { ttype: TT::DOTDOTDOT, line: 2},
        Token { ttype: TT::NUMBER(0.123), line: 3},
    ]);
}

#[test]
fn lex_illegals() {
    test_lex!("0b2 ! @ ~", vec![
        Token { ttype: TT::ILLEGAL("0b2".into()), line: 0},
        Token { ttype: TT::ILLEGAL("!".into()), line: 0},
        Token { ttype: TT::ILLEGAL("@".into()), line: 0},
        Token { ttype: TT::ILLEGAL("~".into()), line: 0},
    ]);
}

#[test]
fn lex_strings() {
    test_lex!(r#"
        "hello'world"
        'foo"bar'
        "test\"string"
        'test\'string'
        'test\\\'
string\'\"'
        'test
    "#, vec![
        Token { ttype: string!("hello'world"), line: 1},
        Token { ttype: string!("foo\"bar"), line: 2},
        Token { ttype: string!("test\"string"), line: 3},
        Token { ttype: string!("test'string"), line: 4},
        Token { ttype: string!("test\\'\nstring'\""), line: 6},
        Token { ttype: TT::ILLEGAL("Unclosed string literal".into()), line: 8},
    ]);
}

