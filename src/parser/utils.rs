
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

pub(super) use peek_token_is;

