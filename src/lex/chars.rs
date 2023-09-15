#![allow(clippy::trivially_copy_pass_by_ref)]

#[inline]
pub const fn is_numeric(ch: &char) -> bool {
    ch.is_ascii_digit()
}

#[inline]
pub const fn is_alphabetic(ch: &char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z' | '_')
}

#[inline]
pub const fn is_space(ch: &char) -> bool {
    matches!(ch, ' ' | '\n' | '\t' | '\r')
}

#[inline]
pub const fn is_numeric_base(ch: &char) -> bool {
    matches!(ch, 'x' | 'b')
}

#[inline]
pub const fn is_hex(ch: &char) -> bool {
    ch.is_ascii_hexdigit()
}

