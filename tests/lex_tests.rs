use rua::lex::{tokenize, tokens::{Token, BinaryOp, UnaryOp}};
use rua::lex::tokens::TokenType as T;
use rua::identifiers::Trie;

macro_rules! test_lex {
    ($input: expr, $expected_output: expr) => {
        let mut identifiers = Trie::new();
        #[allow(unused_macros)]
        macro_rules! id {
            ($s: expr) => (match identifiers.find($s) { Some(T::IDENTIFIER(i)) => T::IDENTIFIER(i), t => panic!("Expected identifier, got {t:?}") })
        }

        let tokens = tokenize($input, &mut identifiers);
        assert_eq!(tokens, $expected_output)
    };
}

#[test]
fn test_basic_assignment() {
    test_lex!("local foo = bar", vec![
        Token { ttype: T::LOCAL },
        Token { ttype: id!("foo") },
        Token { ttype: T::ASSIGN },
        Token { ttype: id!("bar") },
        Token { ttype: T::EOF },
    ]);
}

#[test]
fn test_ints() {
    test_lex!(
        "58
        0x2A
        0b1001
        0b158", vec![
        Token { ttype: T::NUMBER(58.0) },
        Token { ttype: T::NUMBER(42.0) },
        Token { ttype: T::NUMBER(9.0) },
        Token { ttype: T::ILLEGAL("0b15".into()) },
        Token { ttype: T::NUMBER(8.0) },
        Token { ttype: T::EOF },
    ]);
}

#[test]
fn test_floats() {
    test_lex!(
        "0.58
        0x0.2A
        0b0.1
        0b0.23", vec![
        Token { ttype: T::NUMBER(0.58) },
        Token { ttype: T::NUMBER(0.1640625) },
        Token { ttype: T::ILLEGAL("0b0.".into()) },
        Token { ttype: T::NUMBER(1.0) },
        Token { ttype: T::ILLEGAL("0b0.".into()) },
        Token { ttype: T::NUMBER(23.0) },
        Token { ttype: T::EOF },
    ]);
}

#[test]
fn test_ops() {
    test_lex!(
        "a = a <= b and a+b or a^b
        (c-d>e);#f", vec![
            Token { ttype: id!("a") },
            Token { ttype: T::ASSIGN },
            Token { ttype: id!("a") },
            Token { ttype: T::BINARY_OP(BinaryOp::LE) },
            Token { ttype: id!("b") },
            Token { ttype: T::BINARY_OP(BinaryOp::AND) },
            Token { ttype: id!("a") },
            Token { ttype: T::BINARY_OP(BinaryOp::PLUS) },
            Token { ttype: id!("b") },
            Token { ttype: T::BINARY_OP(BinaryOp::OR) },
            Token { ttype: id!("a") },
            Token { ttype: T::BINARY_OP(BinaryOp::EXP) },
            Token { ttype: id!("b") },
            Token { ttype: T::LPAREN },
            Token { ttype: id!("c") },
            Token { ttype: T::MINUS },
            Token { ttype: id!("d") },
            Token { ttype: T::BINARY_OP(BinaryOp::GT) },
            Token { ttype: id!("e") },
            Token { ttype: T::RPAREN },
            Token { ttype: T::SEMICOLON },
            Token { ttype: T::UNARY_OP(UnaryOp::LEN) },
            Token { ttype: id!("f") },
            Token { ttype: T::EOF },
        ]);
}

#[test]
fn test_comment() {
    test_lex!("a = b -- a+b\n--123", vec![
        Token { ttype: id!("a") },
        Token { ttype: T::ASSIGN },
        Token { ttype: id!("b") },
        Token { ttype: T::EOF },
    ]);
}

#[test]
fn test_dots() {
    test_lex!(".\n..\n...\n.123", vec![
        Token { ttype: T::DOT },
        Token { ttype: T::BINARY_OP(BinaryOp::DOTDOT) },
        Token { ttype: T::DOTDOTDOT },
        Token { ttype: T::NUMBER(0.123) },
        Token { ttype: T::EOF },
    ]);
}

#[test]
fn test_illegals() {
    test_lex!("0b2 ! @ ~", vec![
        Token { ttype: T::ILLEGAL("0b2".into()) },
        Token { ttype: T::ILLEGAL("!".into()) },
        Token { ttype: T::ILLEGAL("@".into()) },
        Token { ttype: T::ILLEGAL("~".into()) },
        Token { ttype: T::EOF },
    ]);
}

