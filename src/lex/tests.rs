#![cfg(test)]

use pretty_assertions::assert_eq;

use super::{tokens::{Token, BinaryOp, UnaryOp}, Tokenizer};
use super::tokens::TokenType as T;
use rua_identifiers::Trie;

macro_rules! test_lex {
    ($input: expr, $expected_output: expr) => {
        let mut identifiers = Trie::new();
        #[allow(unused_macros)]
        macro_rules! id {
            ($s: expr) => (match identifiers.add_or_get($s, |_, _| panic!("Identifier {} not found", $s)) {
                T::IDENTIFIER(i) => T::IDENTIFIER(i),
                t => panic!("Expected identifier, got {t:?}")
            })
        }

        let tokens = Tokenizer::new($input.chars(), &mut identifiers).collect::<Vec<_>>();
        assert_eq!(tokens, $expected_output)
    };
}

#[test]
fn lex_basic_assignment() {
    test_lex!("local foo = bar", vec![
        Token { ttype: T::LOCAL, line: 0},
        Token { ttype: id!("foo"), line: 0},
        Token { ttype: T::ASSIGN, line: 0},
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
        Token { ttype: T::NUMBER(58.0), line: 0},
        Token { ttype: T::NUMBER(42.0), line: 1},
        Token { ttype: T::NUMBER(9.0), line: 2},
        Token { ttype: T::ILLEGAL("0b15".into()), line: 3},
        Token { ttype: T::NUMBER(8.0), line: 3},
    ]);
}

#[test]
fn lex_floats() {
    test_lex!(
        "0.58
        0x0.2A
        0b0.1
        0b0.23", vec![
        Token { ttype: T::NUMBER(0.58), line: 0},
        Token { ttype: T::NUMBER(0.164_062_5), line: 1},
        Token { ttype: T::ILLEGAL("0b0.".into()), line: 2},
        Token { ttype: T::NUMBER(1.0), line: 2},
        Token { ttype: T::ILLEGAL("0b0.".into()), line: 3},
        Token { ttype: T::NUMBER(23.0), line: 3},
    ]);
}

#[test]
fn lex_ops() {
    test_lex!(
        "a = a <= b and a+b or a^b
        (c-d>e);#f", vec![
            Token { ttype: id!("a"), line: 0},
            Token { ttype: T::ASSIGN, line: 0},
            Token { ttype: id!("a"), line: 0},
            Token { ttype: T::BINARY_OP(BinaryOp::LE), line: 0},
            Token { ttype: id!("b"), line: 0},
            Token { ttype: T::BINARY_OP(BinaryOp::AND), line: 0},
            Token { ttype: id!("a"), line: 0},
            Token { ttype: T::BINARY_OP(BinaryOp::PLUS), line: 0},
            Token { ttype: id!("b"), line: 0},
            Token { ttype: T::BINARY_OP(BinaryOp::OR), line: 0},
            Token { ttype: id!("a"), line: 0},
            Token { ttype: T::BINARY_OP(BinaryOp::EXP), line: 0},
            Token { ttype: id!("b"), line: 0},
            Token { ttype: T::LPAREN, line: 1},
            Token { ttype: id!("c"), line: 1},
            Token { ttype: T::MINUS, line: 1},
            Token { ttype: id!("d"), line: 1},
            Token { ttype: T::BINARY_OP(BinaryOp::GT), line: 1},
            Token { ttype: id!("e"), line: 1},
            Token { ttype: T::RPAREN, line: 1},
            Token { ttype: T::SEMICOLON, line: 1},
            Token { ttype: T::UNARY_OP(UnaryOp::LEN), line: 1},
            Token { ttype: id!("f"), line: 1},
        ]);
}

#[test]
fn lex_comment() {
    test_lex!("a = b -- a+b\n--123", vec![
        Token { ttype: id!("a"), line: 0},
        Token { ttype: T::ASSIGN, line: 0},
        Token { ttype: id!("b"), line: 0},
    ]);
}

#[test]
fn lex_dots() {
    test_lex!(".\n..\n...\n.123", vec![
        Token { ttype: T::DOT, line: 0},
        Token { ttype: T::BINARY_OP(BinaryOp::DOTDOT), line: 1},
        Token { ttype: T::DOTDOTDOT, line: 2},
        Token { ttype: T::NUMBER(0.123), line: 3},
    ]);
}

#[test]
fn lex_illegals() {
    test_lex!("0b2 ! @ ~", vec![
        Token { ttype: T::ILLEGAL("0b2".into()), line: 0},
        Token { ttype: T::ILLEGAL("!".into()), line: 0},
        Token { ttype: T::ILLEGAL("@".into()), line: 0},
        Token { ttype: T::ILLEGAL("~".into()), line: 0},
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
        Token { ttype: T::STRING("hello'world".into()), line: 1},
        Token { ttype: T::STRING("foo\"bar".into()), line: 2},
        Token { ttype: T::STRING("test\"string".into()), line: 3},
        Token { ttype: T::STRING("test'string".into()), line: 4},
        Token { ttype: T::STRING("test\\'\nstring'\"".into()), line: 6},
        Token { ttype: T::ILLEGAL("Unclosed string literal".into()), line: 8},
    ]);
}

