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
        if !peek_token_is!($compiler, $($args)+) {
            debug_assert!(false);
        }
    };
}

macro_rules! consume {
    ($compiler: expr, $allow_eof: expr, $($args: tt),+) => {
        match $compiler.next_token() {
            $(
                Some(Token{ttype: $args, ..}) => {},
            )+
            Some(t) => return Err(ParseError::UnexpectedToken(Box::new(t), [$($args,)+].into())),
            None if $allow_eof => {},
            None => return Err(ParseError::UnexpectedEOF),
        }
    };
    ($compiler: expr, $($args: tt),+) => {
        consume!($compiler, false, $($args),+)
    };
    ($compiler: expr; allow_eof, $($args: tt),+) => {
        consume!($compiler, true, $($args),+)
    };
}

pub(super) use consume;
pub(super) use debug_peek_token;
pub(super) use peek_token_is;
