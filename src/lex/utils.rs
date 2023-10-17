#![allow(clippy::unused_peekable)]

use std::iter::Peekable;
use super::chars::{is_numeric, is_alphabetic};

#[inline]
pub fn eat_while_peeking<T>(chars: &mut Peekable<T>, pred: &impl Fn(&char)->bool) where T: Iterator<Item = char> + Clone {
    while chars.next_if(pred).is_some() { }
}

#[allow(clippy::cast_possible_truncation)]
pub fn read_decimals<T>(chars: &mut Peekable<T>, radix: u32) -> Result<f64, String> where T: Iterator<Item = char> + Clone {
    let clone_it = chars.clone();
    let mut i = 0;

    let mut res = 0f64;
    let mut multiplier = 1f64;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(ch) || is_alphabetic(ch)) { break; }
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

pub fn read_number<T>(chars: &mut Peekable<T>, radix: u32) -> Result<f64, String> where T: Iterator<Item = char> + Clone {
    let clone_it = chars.clone();
    let mut i = 0;

    let mut res = 0f64;
    while let Some(&ch) = chars.peek() {
        i += 1;
        if !(is_numeric(ch) || is_alphabetic(ch)) { break; }
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


pub fn take_while_peeking<T, F>(chars: &mut Peekable<T>, mut pred: F) -> String where T: Iterator<Item = char> + Clone, F: FnMut(&char) -> bool {
    let mut i = 0;
    let mut clone_it = chars.clone();

    while clone_it.next_if(&mut pred).is_some() {
        i += 1;
    }
    chars.take(i).collect()
}

