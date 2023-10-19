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

pub(super) use debug_peek_token;
pub(super) use peek_token_is;
