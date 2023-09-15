use std::iter::Peekable;
use std::str::Chars;

#[allow(clippy::cast_precision_loss, clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
pub fn parse_float_radix(s: &str, radix: u32) -> Option<f32> {
    let s2 = s.replace('.', "");
    let i = i64::from_str_radix(&s2, radix).ok()?;
    let split = s.split('.');
    let fraction_len: usize = match split.clone().count() {
        1 => 0,
        2 => split.last().expect("Split should have 2 items, wth").len(),
        _ => return None,
    };
    let f = (i as f64) / f64::from(radix).powi(fraction_len as i32);
    Some(f as f32)
}

pub fn to_int(s: &String) -> Option<i32> {
    match s {
        num if num.starts_with("0x") => i32::from_str_radix(num.split_at(2).1, 16).ok(),
        num if num.starts_with("0b") => i32::from_str_radix(num.split_at(2).1, 2).ok(),
        num => num.parse::<i32>().ok(),
    }
}

pub fn to_float(s: &String) -> Option<f32> {
    match s {
        num if num.starts_with("0x") => parse_float_radix(num.split_at(2).1, 16),
        num if num.starts_with("0b") => parse_float_radix(num.split_at(2).1, 2),
        num => num.parse::<f32>().ok(),
    }
}

#[inline]
pub fn take_while_peeking(chars: &mut Peekable<Chars>, pred: &dyn Fn(&char) -> bool) -> String {
    let mut i = 0;
    let mut clone_it = chars.clone();

    while clone_it.next_if(pred).is_some() {
        i += 1;
    }
    chars.take(i).collect()
}

#[inline]
pub fn eat_while_peeking(chars: &mut Peekable<Chars>, pred: &dyn Fn(&char) -> bool) {
    while chars.next_if(pred).is_some() { }
}

