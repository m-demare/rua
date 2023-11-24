#![allow(clippy::unused_peekable)]

use super::chars::{is_alphabetic, is_numeric};
use std::iter::Peekable;

#[inline]
pub(super) fn eat_while_peeking<T>(chars: &mut Peekable<T>, pred: &impl Fn(&u8) -> bool)
where
    T: Iterator<Item = u8> + Clone,
{
    while chars.next_if(pred).is_some() {}
}

#[allow(clippy::cast_possible_truncation)]
pub(super) fn read_decimals<T>(
    chars: &mut Peekable<T>,
    radix: u8,
    int_part: f64,
) -> Result<f64, Box<str>>
where
    T: Iterator<Item = u8> + Clone,
{
    let clone_it = chars.clone();
    let mut i = 0;

    let mut decimal_part = 0f64;
    let mut multiplier = 1f64;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(ch) || is_alphabetic(ch)) {
            break;
        }
        if radix == 10 && matches!(ch, b'e' | b'E') {
            return Ok(pow10(int_part + decimal_part, parse_exp(chars, clone_it, i)?));
        }
        chars.next();
        multiplier /= f64::from(radix);
        decimal_part += match (ch as char).to_digit(radix.into()) {
            Some(n) => f64::from(n) * multiplier,
            None => {
                return Err(String::from_utf8_lossy(&clone_it.take(i).collect::<Vec<_>>()).into())
            }
        };
    }
    #[allow(clippy::cast_precision_loss)]
    let precision: f64 = u64::from(radix).pow(i as u32) as f64;
    decimal_part = (decimal_part * precision).round() / precision;
    Ok(int_part + decimal_part)
}

fn format_num(s: &[u8], radix: u8) -> Box<str> {
    format!(
        "{}{}",
        match radix {
            2 => "0b",
            16 => "0x",
            _ => "",
        },
        String::from_utf8_lossy(s)
    )
    .into_boxed_str()
}

pub fn read_number_radix<T>(chars: &mut Peekable<T>, radix: u8) -> Result<f64, Box<str>>
where
    T: Iterator<Item = u8> + Clone,
{
    let clone_it = chars.clone();
    let mut i = 0;

    let mut res = 0f64;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(ch) || is_alphabetic(ch)) {
            break;
        }
        if radix == 10 && matches!(ch, b'e' | b'E') {
            return Ok(pow10(res, parse_exp(chars, clone_it, i)?));
        }
        chars.next();
        res *= f64::from(radix);
        res += match (ch as char).to_digit(radix.into()) {
            Some(n) => f64::from(n),
            None => return Err(format_num(&clone_it.take(i).collect::<Vec<_>>(), radix)),
        };
    }
    if Some(&b'.') == chars.peek() {
        chars.next();
        if radix == 2 {
            return Err(format_num(&clone_it.take(i).collect::<Vec<_>>(), radix));
        }
        return read_decimals(chars, radix, res);
    }
    Ok(res)
}

fn pow10(mut n: f64, e: i32) -> f64 {
    if e.abs() > 15 {
        return n * 10f64.powi(e);
    }

    // experimentally, has less error than n * 10f64.powi(e) for small e
    // TODO check out how others do it
    if e >= 0 {
        for _ in 0..e {
            n *= 10.0;
        }
    } else {
        for _ in e..0 {
            n /= 10.0;
        }
    }
    n
}

fn parse_exp<T>(
    chars: &mut Peekable<T>,
    clone_it: Peekable<T>,
    mut i: usize,
) -> Result<i32, Box<str>>
where
    T: Iterator<Item = u8> + Clone,
{
    let ch = chars.next();
    debug_assert!(matches!(ch, Some(b'e' | b'E')), "Invalid exponent char");
    let sign = match chars.peek() {
        Some(b'+') => {
            chars.next();
            1
        }
        Some(b'-') => {
            chars.next();
            -1
        }
        _ => 1,
    };
    let mut res = 0;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(ch) || is_alphabetic(ch)) {
            break;
        }
        chars.next();
        if ch.is_ascii_digit() {
            res *= 10;
            res += i32::from(ch - b'0');
        } else {
            return Err(format_num(&clone_it.take(i).collect::<Vec<_>>(), 10));
        }
    }
    Ok(res * sign)
}

pub(super) fn read_number<T>(chars: &mut Peekable<T>) -> Result<f64, Box<str>>
where
    T: Iterator<Item = u8> + Clone,
{
    let mut radix = 10;
    if chars.peek() == Some(&b'0') {
        chars.next();
        if let Some(ch) = chars.peek() {
            radix = match ch {
                b'x' => {
                    chars.next();
                    16
                }
                b'b' => {
                    chars.next();
                    2
                }
                _ => 10,
            }
        }
    }
    read_number_radix(chars, radix)
}
