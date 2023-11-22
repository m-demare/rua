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
pub(super) fn read_decimals<T>(chars: &mut Peekable<T>, radix: u8) -> Result<f64, Box<str>>
where
    T: Iterator<Item = u8> + Clone,
{
    let clone_it = chars.clone();
    let mut i = 0;

    let mut res = 0f64;
    let mut multiplier = 1f64;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(ch) || is_alphabetic(ch)) {
            break;
        }
        chars.next();
        multiplier /= f64::from(radix);
        res += match (ch as char).to_digit(radix.into()) {
            Some(n) => f64::from(n),
            None => {
                return Err(String::from_utf8_lossy(&clone_it.take(i).collect::<Vec<_>>()).into())
            }
        } * multiplier;
    }
    let precision = f64::from((u32::from(radix)).pow(i as u32));
    res = (res * precision).round() / precision;
    Ok(res)
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
        res += read_decimals(chars, radix)?;
    }
    Ok(res)
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
