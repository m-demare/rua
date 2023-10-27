pub const fn is_numeric(ch: char) -> bool {
    ch.is_ascii_digit()
}

pub const fn is_alphabetic(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z' | '_')
}

pub const fn is_space(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\r')
}
