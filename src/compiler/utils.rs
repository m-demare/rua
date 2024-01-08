use crate::lex::tokens::TokenType as TT;

macro_rules! peek_token_is {
    ($compiler: expr, $($args: pat),+) => {
        match $compiler.peek_token() {
            $(
                Some(Token{ttype: $args, ..}) => true,
            )+
            _ => false,
        }
    };
}

macro_rules! debug_peek_token {
    ($compiler: expr, $($args: pat),+) => {
        if cfg!(debug_assertions) && !peek_token_is!($compiler, $($args)+) {
            panic!("debug_peek_token failed");
        }
    };
}

macro_rules! match_token{
    ($compiler: expr, $($args: pat),+) => ({
        match $compiler.peek_token() {
            $(
                Some(Token{ttype: $args, ..}) => {$compiler.next_token(); true},
            )+
            _ => false,
        }
    });
}
macro_rules! consume {
    ($compiler: expr, $allow_eof: expr, $($args: tt),+) => {
        match $compiler.next_token() {
            $(
                Some(Token{ttype: $args, ..}) => {},
            )+
            Some(t) => return Err(ParseError::UnexpectedToken(UnexpectedToken{
                got: Box::new(t),
                expected: [$($args,)+].into()
            })),
            None if $allow_eof => {},
            None => return Err(ParseError::UnexpectedEOF),
        }
    };
    ($compiler: expr; $($args: tt),+) => {
        consume!($compiler, false, $($args),+)
    };
    ($compiler: expr; allow_eof, $($args: tt),+) => {
        consume!($compiler, true, $($args),+)
    };
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Clone, Copy)]
pub(super) enum Precedence {
    Lowest,
    Or,
    And,
    Comparator,  // >, <, <=, >=, ~=, ==
    Dotdot,      // ..
    Sum,         // +, -
    Product,     // *, /, %
    Prefix,      // not, #, - (unary)
    Exp,         // ^
    Call,        // foo(...)
    FieldAccess, // foo.bar, foo[bar]
}

pub(super) fn precedence_of_binary(op: &TT) -> Precedence {
    match op {
        TT::OR => Precedence::Or,
        TT::AND => Precedence::And,
        TT::EQ | TT::NEQ | TT::LE | TT::GE | TT::LT | TT::GT => Precedence::Comparator,
        TT::DOTDOT => Precedence::Dotdot,
        TT::PLUS | TT::MINUS => Precedence::Sum,
        TT::TIMES | TT::DIV | TT::MOD => Precedence::Product,
        TT::EXP => Precedence::Exp,
        TT::LBRACK | TT::DOT => Precedence::FieldAccess,
        _ => unreachable!("Invalid binary"),
    }
}

pub(super) use consume;
pub(super) use debug_peek_token;
pub(super) use match_token;
pub(super) use peek_token_is;
