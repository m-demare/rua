macro_rules! peek_token_is {
    ($tokens_it: expr, $($args: pat),+) => {
        match $tokens_it.peek() {
            $(
                Some(Token{ttype: $args, ..}) => true,
            )+
            _ => false,
        }
    };
}

macro_rules! debug_peek_token {
    ($tokens_it: expr, $($args: pat),+) => {
        if !peek_token_is!($tokens_it, $($args)+) {
            debug_assert!(false);
        }
    };
}

macro_rules! assert_next_token_is {
    ($tokens_it: expr, $($args: tt),+) => {
        match $tokens_it.next() {
            $(
                t@Some(Token{ttype: $args, ..}) => {t},
            )+
            Some(t) => return Err(ParseError::UnexpectedToken(Box::new(t), [$($args,)+].into())),
            None => return Err(ParseError::UnexpectedEOF),
        }
    };
}

pub(super) use assert_next_token_is;
pub(super) use debug_peek_token;
pub(super) use peek_token_is;
