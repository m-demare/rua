#![allow(clippy::unused_peekable)]

use std::iter::Peekable;
use std::str::Chars;
use super::chars::{is_numeric, is_hex};

#[inline]
pub fn take_while_peeking(chars: &mut Peekable<Chars>, pred: &impl Fn(&char)->bool) -> String {
    let mut i = 0;
    let mut clone_it = chars.clone();

    while clone_it.next_if(pred).is_some() {
        i += 1;
    }
    chars.take(i).collect()
}

#[inline]
pub fn eat_while_peeking(chars: &mut Peekable<Chars>, pred: &impl Fn(&char)->bool) {
    while chars.next_if(pred).is_some() { }
}

#[allow(clippy::cast_possible_truncation)]
pub fn read_decimals(chars: &mut Peekable<Chars>, radix: u32) -> Result<f64, String>{
    let clone_it = chars.clone();
    let mut i = 0;

    let mut res = 0f64;
    let mut multiplier = 1f64;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(&ch) || radix == 16 && is_hex(&ch)) { break; }
        chars.next();
        multiplier /= f64::from(radix);
        res += match ch.to_digit(radix) {
            Some(n) => f64::from(n),
            None => return Err(clone_it.take(i).collect()),
        } * multiplier;
    }
    let precision = f64::from(radix.pow(i as u32));
    res = (res * precision).round() / precision;
    Ok(res)
}

pub fn read_number(chars: &mut Peekable<Chars>, radix: u32) -> Result<f64, String>{
    let clone_it = chars.clone();
    let mut i = 0;

    let mut res = 0f64;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(&ch) || radix == 16 && is_hex(&ch)) { break; }
        chars.next();
        res *= f64::from(radix);
        res += match ch.to_digit(radix) {
            Some(n) => f64::from(n),
            None => return Err(clone_it.take(i).collect()),
        };
    }
    if Some(&'.') == chars.peek() {
        chars.next();
        if radix == 2 {
            return Err(clone_it.take(i).collect())
        }
        res += read_decimals(chars, radix)?;
    }
    Ok(res)
}

