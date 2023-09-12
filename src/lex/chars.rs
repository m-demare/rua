#[inline]
pub const fn is_numeric(ch: u8) -> bool{
    ch.is_ascii_digit()
}

#[inline]
pub const fn is_alphabetic(ch: u8) -> bool{
    matches!(ch, b'A'..=b'Z' | b'a'..=b'z' | b'_')
}

#[inline]
pub const fn is_space(ch: u8) -> bool{
    matches!(ch, b' ' | b'\n' | b'\t' | b'\r')
}

#[inline]
pub const fn is_numeric_base(ch: u8) -> bool{
    matches!(ch, b'x' | b'b')
}

#[inline]
pub const fn is_hex(ch: u8) -> bool{
    ch.is_ascii_hexdigit()
}

