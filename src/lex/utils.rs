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

pub fn to_int(bytes: &[u8]) -> Option<i32> {
    match bytes {
        [b'0', b'x', num @ ..] => i32::from_str_radix(&String::from_utf8(num.to_vec()).ok()?, 16).ok(),
        [b'0', b'b', num @ ..] => i32::from_str_radix(&String::from_utf8(num.to_vec()).ok()?, 2).ok(),
        num => String::from_utf8(num.to_vec()).ok()?.parse::<i32>().ok(),
    }
}

pub fn to_float(bytes: &[u8]) -> Option<f32> {
    match bytes {
        [b'0', b'x', num @ ..] => parse_float_radix(&String::from_utf8(num.to_vec()).ok()?, 16),
        [b'0', b'b', num @ ..] => parse_float_radix(&String::from_utf8(num.to_vec()).ok()?, 2),
        num => String::from_utf8(num.to_vec()).ok()?.parse::<f32>().ok(),
    }
}

#[inline]
pub fn read_while(bytes: &[u8], read_pos: &mut usize, pred: &dyn Fn(u8) -> bool) {
    let length = bytes.len();

    *read_pos += 1; // We assume the first char is valid, so don't check pred()

    while *read_pos < length && pred(bytes[*read_pos]){
        *read_pos += 1;
    }
}

