pub const fn is_numeric(ch: u8) -> bool {
    ch.is_ascii_digit()
}

pub const fn is_alphabetic(ch: u8) -> bool {
    matches!(ch, b'A'..=b'Z' | b'a'..=b'z' | b'_')
}

pub const fn is_space(ch: u8) -> bool {
    matches!(ch, b' ' | b'\t' | b'\r')
}
