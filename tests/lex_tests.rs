use rua::lex::{tokenize, tokens::{Token, BinaryOp, UnaryOp}};
use rua::lex::tokens::TokenType as T;

fn test_lex(input: &str, expected_output: Vec<Token>) {
    let tokens = tokenize(input);
    assert_eq!(tokens, expected_output)
}

#[test]
fn test_basic_assignment() {
    test_lex("local foo = bar", [
        Token { ttype: T::LOCAL },
        Token { ttype: T::IDENTIFIER("foo".to_owned()) },
        Token { ttype: T::ASSIGN },
        Token { ttype: T::IDENTIFIER("bar".to_owned()) },
        Token { ttype: T::EOF },
    ].into());
}

#[test]
fn test_ints() {
    test_lex(
        "58
        0x2A
        0b1001
        0b158", [
        Token { ttype: T::NUMBER(58.0) },
        Token { ttype: T::NUMBER(42.0) },
        Token { ttype: T::NUMBER(9.0) },
        Token { ttype: T::ILLEGAL("0b15".to_owned()) },
        Token { ttype: T::NUMBER(8.0) },
        Token { ttype: T::EOF },
    ].into());
}

#[test]
fn test_floats() {
    test_lex(
        "0.58
        0x0.2A
        0b0.1
        0b0.23", [
        Token { ttype: T::NUMBER(0.58) },
        Token { ttype: T::NUMBER(0.1640625) },
        Token { ttype: T::ILLEGAL("0b0.".to_owned()) },
        Token { ttype: T::NUMBER(1.0) },
        Token { ttype: T::ILLEGAL("0b0.".to_owned()) },
        Token { ttype: T::NUMBER(23.0) },
        Token { ttype: T::EOF },
    ].into());
}

#[test]
fn test_ops() {
    test_lex(
        "a = a <= b and a+b or a^b
        (c-d>e);#f", [
            Token { ttype: T::IDENTIFIER("a".to_owned()) },
            Token { ttype: T::ASSIGN },
            Token { ttype: T::IDENTIFIER("a".to_owned()) },
            Token { ttype: T::BINARY_OP(BinaryOp::LE) },
            Token { ttype: T::IDENTIFIER("b".to_owned()) },
            Token { ttype: T::BINARY_OP(BinaryOp::AND) },
            Token { ttype: T::IDENTIFIER("a".to_owned()) },
            Token { ttype: T::BINARY_OP(BinaryOp::PLUS) },
            Token { ttype: T::IDENTIFIER("b".to_owned()) },
            Token { ttype: T::BINARY_OP(BinaryOp::OR) },
            Token { ttype: T::IDENTIFIER("a".to_owned()) },
            Token { ttype: T::BINARY_OP(BinaryOp::EXP) },
            Token { ttype: T::IDENTIFIER("b".to_owned()) },
            Token { ttype: T::LPAREN },
            Token { ttype: T::IDENTIFIER("c".to_owned()) },
            Token { ttype: T::MINUS },
            Token { ttype: T::IDENTIFIER("d".to_owned()) },
            Token { ttype: T::BINARY_OP(BinaryOp::GT) },
            Token { ttype: T::IDENTIFIER("e".to_owned()) },
            Token { ttype: T::RPAREN },
            Token { ttype: T::SEMICOLON },
            Token { ttype: T::UNARY_OP(UnaryOp::LEN) },
            Token { ttype: T::IDENTIFIER("f".to_owned()) },
            Token { ttype: T::EOF },
        ].into());
}

#[test]
fn test_comment() {
    test_lex("a = b -- a+b\n--123", [
        Token { ttype: T::IDENTIFIER("a".to_owned()) },
        Token { ttype: T::ASSIGN },
        Token { ttype: T::IDENTIFIER("b".to_owned()) },
        Token { ttype: T::EOF },
    ].into());
}

#[test]
fn test_dots() {
    test_lex(".\n..\n...\n.123", [
        Token { ttype: T::DOT },
        Token { ttype: T::BINARY_OP(BinaryOp::DOTDOT) },
        Token { ttype: T::DOTDOTDOT },
        Token { ttype: T::NUMBER(0.123) },
        Token { ttype: T::EOF },
    ].into());
}

#[test]
fn test_illegals() {
    test_lex("0b2 ! @ ~", [
        Token { ttype: T::ILLEGAL("0b2".to_owned()) },
        Token { ttype: T::ILLEGAL("!".to_owned()) },
        Token { ttype: T::ILLEGAL("@".to_owned()) },
        Token { ttype: T::ILLEGAL("~".to_owned()) },
        Token { ttype: T::EOF },
    ].into());
}

