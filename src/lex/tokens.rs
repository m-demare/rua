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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

pub fn lookup_keyword(identifier: &str) -> Option<TokenType> {
    use TokenType as T;
    Some(match identifier {
        "and" => T::BINARY_OP(BinaryOp::AND),
        "break" => T::BREAK,
        "do" => T::DO,
        "else" => T::ELSE,
        "elseif" => T::ELSEIF,
        "end" => T::END,
        "false" => T::FALSE,
        "for" => T::FOR,
        "function" => T::FUNCTION,
        "if" => T::IF,
        "in" => T::IN,
        "local" => T::LOCAL,
        "nil" => T::NIL,
        "not" => T::UNARY_OP(UnaryOp::NOT),
        "or" => T::BINARY_OP(BinaryOp::OR),
        "repeat" => T::REPEAT,
        "return" => T::RETURN,
        "then" => T::THEN,
        "true" => T::TRUE,
        "until" => T::UNTIL,
        "while" => T::WHILE,
        _ => return None,
    })
}

#[inline]
pub(super) fn lookup_char(ch: char) -> TokenType {
    use TokenType as T;
    match ch {
        '+' => T::BINARY_OP(BinaryOp::PLUS),
        '-' => T::MINUS,
        '*' => T::BINARY_OP(BinaryOp::TIMES),
        '/' => T::BINARY_OP(BinaryOp::DIV),
        '%' => T::BINARY_OP(BinaryOp::MOD),
        '^' => T::BINARY_OP(BinaryOp::EXP),
        '#' => T::UNARY_OP(UnaryOp::LEN),

        '=' => T::ASSIGN,
        '(' => T::LPAREN,
        ')' => T::RPAREN,
        '{' => T::LBRACE,
        '}' => T::RBRACE,
        '[' => T::LBRACK,
        ']' => T::RBRACK,
        ';' => T::SEMICOLON,
        ':' => T::COLON,
        ',' => T::COMMA,

        _ => T::ILLEGAL(ch.to_string().into_boxed_str()),
    }
}

#[inline]
pub(super) fn lookup_comparison(ch: char, has_eq: bool) -> TokenType {
    use TokenType as T;
    debug_assert!(matches!(ch, '<' | '>' | '=' | '~'));

    if has_eq {
        match ch {
            '<' => T::BINARY_OP(BinaryOp::LE),
            '>' => T::BINARY_OP(BinaryOp::GE),
            '=' => T::BINARY_OP(BinaryOp::EQ),
            '~' => T::BINARY_OP(BinaryOp::NEQ),
            _ => T::ILLEGAL((ch.to_string() + "=").into_boxed_str()),
        }
    } else {
        match ch {
            '<' => T::BINARY_OP(BinaryOp::LT),
            '>' => T::BINARY_OP(BinaryOp::GT),
            '=' => T::ASSIGN,
            _ => T::ILLEGAL(ch.to_string().into_boxed_str()),
        }
    }
}
