use crate::eval::vals::string::RuaString;

#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum TokenType {
    ILLEGAL(Box<str>),

    NUMBER(f64),
    STRING(RuaString),
    IDENTIFIER(RuaString),

    BINARY_OP(BinaryOp),
    UNARY_OP(UnaryOp),
    MINUS,

    ASSIGN,

    DOT,
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
    REPEAT,
    RETURN,
    THEN,
    TRUE,
    UNTIL,
    WHILE,

    IDENTIFIER_DUMMY,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum BinaryOp {
    // Arithmetic operators
    PLUS,
    TIMES,
    DIV,
    MOD,
    EXP,

    // Comparison operators
    EQ,
    NEQ,
    LE,
    GE,
    LT,
    GT,

    // Logic operators
    AND,
    OR,

    DOTDOT,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum UnaryOp {
    NOT,
    LEN,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub line: usize,
}

pub const fn lookup_keyword(identifier: &[u8]) -> Option<TokenType> {
    use TokenType as T;
    Some(match identifier {
        b"and" => T::BINARY_OP(BinaryOp::AND),
        b"break" => T::BREAK,
        b"do" => T::DO,
        b"else" => T::ELSE,
        b"elseif" => T::ELSEIF,
        b"end" => T::END,
        b"false" => T::FALSE,
        b"for" => T::FOR,
        b"function" => T::FUNCTION,
        b"if" => T::IF,
        b"in" => T::IN,
        b"local" => T::LOCAL,
        b"nil" => T::NIL,
        b"not" => T::UNARY_OP(UnaryOp::NOT),
        b"or" => T::BINARY_OP(BinaryOp::OR),
        b"repeat" => T::REPEAT,
        b"return" => T::RETURN,
        b"then" => T::THEN,
        b"true" => T::TRUE,
        b"until" => T::UNTIL,
        b"while" => T::WHILE,
        _ => return None,
    })
}

#[inline]
pub(super) fn lookup_char(ch: u8) -> TokenType {
    use TokenType as T;
    match ch {
        b'+' => T::BINARY_OP(BinaryOp::PLUS),
        b'-' => T::MINUS,
        b'*' => T::BINARY_OP(BinaryOp::TIMES),
        b'/' => T::BINARY_OP(BinaryOp::DIV),
        b'%' => T::BINARY_OP(BinaryOp::MOD),
        b'^' => T::BINARY_OP(BinaryOp::EXP),
        b'#' => T::UNARY_OP(UnaryOp::LEN),

        b'=' => T::ASSIGN,
        b'(' => T::LPAREN,
        b')' => T::RPAREN,
        b'{' => T::LBRACE,
        b'}' => T::RBRACE,
        b'[' => T::LBRACK,
        b']' => T::RBRACK,
        b';' => T::SEMICOLON,
        b':' => T::COLON,
        b',' => T::COMMA,

        _ => T::ILLEGAL((ch as char).to_string().into_boxed_str()),
    }
}

#[inline]
pub(super) fn lookup_comparison(ch: u8, has_eq: bool) -> TokenType {
    use TokenType as T;
    debug_assert!(matches!(ch, b'<' | b'>' | b'=' | b'~'));

    if has_eq {
        match ch {
            b'<' => T::BINARY_OP(BinaryOp::LE),
            b'>' => T::BINARY_OP(BinaryOp::GE),
            b'=' => T::BINARY_OP(BinaryOp::EQ),
            b'~' => T::BINARY_OP(BinaryOp::NEQ),
            _ => T::ILLEGAL(((ch as char).to_string() + "=").into_boxed_str()),
        }
    } else {
        match ch {
            b'<' => T::BINARY_OP(BinaryOp::LT),
            b'>' => T::BINARY_OP(BinaryOp::GT),
            b'=' => T::ASSIGN,
            _ => T::ILLEGAL((ch as char).to_string().into_boxed_str()),
        }
    }
}
