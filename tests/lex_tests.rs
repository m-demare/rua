use rua::lex::{tokenize, tokens::{TokenType::*, Token}};

fn test_lex(input: &str, expected_output: Vec<Token>) {
    let tokens = tokenize(input);
    assert_eq!(tokens, expected_output)
}

#[test]
fn test_basic_assignment() {
    test_lex("local foo = bar", [
        Token { ttype: LOCAL },
        Token { ttype: IDENTIFIER("foo".to_owned()) },
        Token { ttype: ASSIGN },
        Token { ttype: IDENTIFIER("bar".to_owned()) },
        Token { ttype: EOF },
    ].into());
}

#[test]
fn test_ints() {
    test_lex(
        "58
        0x2A
        0b1001
        0b58", [
        Token { ttype: INT(58) },
        Token { ttype: INT(0x2A) },
        Token { ttype: INT(0b1001) },
        Token { ttype: ILLEGAL("0b58".to_owned()) },
        Token { ttype: EOF },
    ].into());
}

#[test]
fn test_floats() {
    test_lex(
        "0.58
        0x0.2A
        0b0.1001
        0b0.23", [
        Token { ttype: FLOAT(0.58) },
        Token { ttype: FLOAT(0.1640625) },
        Token { ttype: FLOAT(0.5625) },
        Token { ttype: ILLEGAL("0b0.23".to_owned()) },
        Token { ttype: EOF },
    ].into());
}

#[test]
fn test_ops() {
    test_lex(
        "a = a <= b and a+b or a^b
        (c-d>e);#f", [
            Token { ttype: IDENTIFIER("a".to_owned()) },
            Token { ttype: ASSIGN },
            Token { ttype: IDENTIFIER("a".to_owned()) },
            Token { ttype: LE },
            Token { ttype: IDENTIFIER("b".to_owned()) },
            Token { ttype: AND },
            Token { ttype: IDENTIFIER("a".to_owned()) },
            Token { ttype: PLUS },
            Token { ttype: IDENTIFIER("b".to_owned()) },
            Token { ttype: OR },
            Token { ttype: IDENTIFIER("a".to_owned()) },
            Token { ttype: EXP },
            Token { ttype: IDENTIFIER("b".to_owned()) },
            Token { ttype: LPAREN },
            Token { ttype: IDENTIFIER("c".to_owned()) },
            Token { ttype: MINUS },
            Token { ttype: IDENTIFIER("d".to_owned()) },
            Token { ttype: GT },
            Token { ttype: IDENTIFIER("e".to_owned()) },
            Token { ttype: RPAREN },
            Token { ttype: SEMICOLON },
            Token { ttype: LEN },
            Token { ttype: IDENTIFIER("f".to_owned()) },
            Token { ttype: EOF },
        ].into());
}

#[test]
fn test_comment() {
    test_lex("a = b -- a+b\n--123", [
        Token { ttype: IDENTIFIER("a".to_owned()) },
        Token { ttype: ASSIGN },
        Token { ttype: IDENTIFIER("b".to_owned()) },
        Token { ttype: EOF },
    ].into());
}

#[test]
fn test_dots() {
    test_lex(".\n..\n...\n.123", [
        Token { ttype: DOT },
        Token { ttype: DOTDOT },
        Token { ttype: DOTDOTDOT },
        Token { ttype: FLOAT(0.123) },
        Token { ttype: EOF },
    ].into());
}

#[test]
fn test_illegals() {
    test_lex("0b210 ! @ ~", [
        Token { ttype: ILLEGAL("0b210".to_owned()) },
        Token { ttype: ILLEGAL("!".to_owned()) },
        Token { ttype: ILLEGAL("@".to_owned()) },
        Token { ttype: ILLEGAL("~".to_owned()) },
        Token { ttype: EOF },
    ].into());
}

