#[derive(Debug, PartialEq)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum TokenType {
    ILLEGAL(Box<[u8]>),
    EOF,

    INT(i32),
    FLOAT(f32),
    IDENTIFIER(Box<[u8]>),

    // Arithmetic operators
    PLUS,
    MINUS,
    TIMES,
    DIV,
    MOD,
    EXP,
    // Logic operators
    EQ,
    NEQ,
    LE,
    GE,
    LT,
    GT,

    ASSIGN,
    LEN,

    DOT,
    DOTDOT,
    DOTDOTDOT,

    // Delimiters
    COMMA,
    SEMICOLON,
    COLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACK,
    RBRACK,

    // Keywords
    AND,
    BREAK,
    DO,
    ELSE,
    ELSEIF,
    END,
    FALSE,
    FOR,
    FUNCTION,
    IF,
    IN,
    LOCAL,
    NIL,
    NOT,
    OR,
    REPEAT,
    RETURN,
    THEN,
    TRUE,
    UNTIL,
    WHILE,
}

#[derive(Debug, PartialEq)]
pub struct Token{
    pub ttype: TokenType,
}

pub(super) fn lookup_ident(identifier: &String) -> TokenType {
    use TokenType::{IDENTIFIER, AND, BREAK, DO, ELSE, ELSEIF, END, FALSE, FOR, FUNCTION, IF, IN, LOCAL, NIL, NOT, OR, REPEAT, RETURN, THEN, TRUE, UNTIL, WHILE};
    match identifier.as_str() {
        "and" => AND,
        "break" => BREAK,
        "do" => DO,
        "else" => ELSE,
        "elseif" => ELSEIF,
        "end" => END,
        "false" => FALSE,
        "for" => FOR,
        "function" => FUNCTION,
        "if" => IF,
        "in" => IN,
        "local" => LOCAL,
        "nil" => NIL,
        "not" => NOT,
        "or" => OR,
        "repeat" => REPEAT,
        "return" => RETURN,
        "then" => THEN,
        "true" => TRUE,
        "until" => UNTIL,
        "while" => WHILE,
        _ => IDENTIFIER(identifier.as_bytes().into()),
    }
}

#[inline]
pub(super) fn lookup_char(ch: u8) -> TokenType {
    use TokenType::{ILLEGAL, PLUS, MINUS, TIMES, DIV, MOD, EXP, ASSIGN, LEN, COMMA, SEMICOLON, COLON, LPAREN, RPAREN, LBRACE, RBRACE, LBRACK, RBRACK};
    match ch {
        b'+' => PLUS,
        b'-' => MINUS,
        b'*' => TIMES,
        b'/' => DIV,
        b'%' => MOD,
        b'^' => EXP,
        b'#' => LEN,

        b'=' => ASSIGN,
        b'(' => LPAREN,
        b')' => RPAREN,
        b'{' => LBRACE,
        b'}' => RBRACE,
        b'[' => LBRACK,
        b']' => RBRACK,
        b';' => SEMICOLON,
        b':' => COLON,
        b',' => COMMA,

        _ => ILLEGAL([ch].into()),
    }
}

#[inline]
pub(super) fn lookup_comparison(ch: u8, has_eq: bool) -> TokenType {
    use TokenType::{ILLEGAL, EQ, NEQ, LE, GE, LT, GT, ASSIGN};
    debug_assert!(matches!(ch, b'<' | b'>' | b'=' | b'~'));

    if has_eq {
        match ch {
            b'<' => LE,
            b'>' => GE,
            b'=' => EQ,
            b'~' => NEQ,
            _ => ILLEGAL([ch, b'='].into()),
        }
    } else {
        match ch {
            b'<' => LT,
            b'>' => GT,
            b'=' => ASSIGN,
            _ => ILLEGAL([ch].into()),
        }
    }
}

