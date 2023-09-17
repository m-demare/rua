#[derive(Debug, PartialEq)]
#[allow(clippy::upper_case_acronyms, non_camel_case_types)]
pub enum TokenType {
    ILLEGAL(String),
    EOF,

    NUMBER(f64),
    IDENTIFIER(String), // TODO Stop storing the String for every identifier

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

pub(super) fn lookup_ident(identifier: String) -> TokenType {
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
        _ => IDENTIFIER(identifier),
    }
}

#[inline]
pub(super) fn lookup_char(ch: char) -> TokenType {
    use TokenType::{ILLEGAL, PLUS, MINUS, TIMES, DIV, MOD, EXP, ASSIGN, LEN, COMMA, SEMICOLON, COLON, LPAREN, RPAREN, LBRACE, RBRACE, LBRACK, RBRACK};
    match ch {
        '+' => PLUS,
        '-' => MINUS,
        '*' => TIMES,
        '/' => DIV,
        '%' => MOD,
        '^' => EXP,
        '#' => LEN,

        '=' => ASSIGN,
        '(' => LPAREN,
        ')' => RPAREN,
        '{' => LBRACE,
        '}' => RBRACE,
        '[' => LBRACK,
        ']' => RBRACK,
        ';' => SEMICOLON,
        ':' => COLON,
        ',' => COMMA,

        _ => ILLEGAL(ch.to_string()),
    }
}

#[inline]
pub(super) fn lookup_comparison(ch: char, has_eq: bool) -> TokenType {
    use TokenType::{ILLEGAL, EQ, NEQ, LE, GE, LT, GT, ASSIGN};
    debug_assert!(matches!(ch, '<' | '>' | '=' | '~'));

    if has_eq {
        match ch {
            '<' => LE,
            '>' => GE,
            '=' => EQ,
            '~' => NEQ,
            _ => ILLEGAL(ch.to_string() + "="),
        }
    } else {
        match ch {
            '<' => LT,
            '>' => GT,
            '=' => ASSIGN,
            _ => ILLEGAL(ch.to_string()),
        }
    }
}

