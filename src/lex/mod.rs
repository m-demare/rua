mod chars;
mod utils;
pub mod tokens;

use crate::lex::tokens::{Token, TokenType, lookup_char, lookup_comparison, lookup_ident};
use crate::lex::chars::{is_alphabetic, is_hex, is_numeric, is_numeric_base, is_space};
use self::utils::{read_while, to_float, to_int};

pub fn tokenize(input: &str) -> Vec<Token> {
    let bytes = input.as_bytes();
    let length = bytes.len();
    let mut read_pos = 0;
    let mut tokens = Vec::with_capacity(100);

    while read_pos < length {
        let token = get_token(bytes, &mut read_pos);
        if let Some(t) = token {
            tokens.push(t);
        }
    }

    tokens.push(Token { ttype: TokenType::EOF });

    tokens
}

fn get_token(bytes: &[u8], read_pos: &mut usize) -> Option<Token> {
    let ch = bytes[*read_pos];
    match ch {
        b'+' | b'*' | b'/' | b'%' | b'^' | b'#' |
        b'(' | b')' | b'{' | b'}' | b'[' | b']' | b';' | b':' | b',' => Some(single_char_token(ch, read_pos)),
        b'=' | b'<' | b'>' | b'~' => Some(read_comparison(bytes, read_pos)),
        b'-' => read_minus(bytes, read_pos),
        b'.' => Some(read_dot(bytes, read_pos)),
        a if is_alphabetic(a) => Some(read_identifier(bytes, read_pos)),
        n if is_numeric(n) => Some(read_number(bytes, read_pos)),
        s if is_space(s) => { eat_spaces(bytes, read_pos); None },
        _ => Some(single_char_token(ch, read_pos)),
    }
}

#[inline]
fn single_char_token(ch: u8, read_pos: &mut usize) -> Token {
    *read_pos += 1;
    Token { ttype: lookup_char(ch) }
}

fn read_comparison(bytes: &[u8], read_pos: &mut usize) -> Token {
    let ch = bytes[*read_pos];
    if *read_pos+1 < bytes.len() && bytes[*read_pos+1] == b'=' {
        *read_pos += 2;
        return Token { ttype: lookup_comparison(ch, true) };
    }
    *read_pos += 1;
    Token { ttype: lookup_comparison(ch, false) }
}

fn read_dot(bytes: &[u8], read_pos: &mut usize) -> Token {
    *read_pos+=1;
    if *read_pos < bytes.len() && bytes[*read_pos] == b'.' {
        *read_pos+=1;
        if *read_pos < bytes.len() && bytes[*read_pos] == b'.' {
            *read_pos+=1;
            return Token { ttype: TokenType::DOTDOTDOT}
        }
        return Token { ttype: TokenType::DOTDOT}
    }
    Token { ttype: TokenType::DOT}
}

fn read_identifier(bytes: &[u8], read_pos: &mut usize) -> Token {
    debug_assert!(is_alphabetic(bytes[*read_pos]));

    // let length = bytes.len();
    let pos = *read_pos;

    read_while(bytes, read_pos, &|ch| is_alphabetic(ch) || is_numeric(ch));
    // *read_pos += 1;
    // let mut ch = char::from(bytes[*read_pos]);

    // while *read_pos < length && (is_alphabetic(ch) || is_numeric(ch)){
    //     *read_pos += 1;
    //     ch = char::from(bytes[*read_pos]);
    // }

    // let identifier = &bytes[pos..*read_pos];

    // *read_pos -= 1;

    let identifier = &bytes[pos..*read_pos];
    let literal = String::from_utf8(identifier.to_vec()).expect("Identifier should contain only [A-Za-z0-9_]");
    Token { ttype: lookup_ident(&literal) }
}

fn read_number(bytes: &[u8], read_pos: &mut usize) -> Token {
    debug_assert!(is_numeric(bytes[*read_pos]));
    let pos = *read_pos;

    read_while(bytes, read_pos, &|ch| is_hex(ch) || is_numeric_base(ch));

    if *read_pos < bytes.len() && bytes[*read_pos] == b'.' {
        // Read float
        read_while(bytes, read_pos, &is_hex);
        let number_bytes = &bytes[pos..*read_pos];
        return match to_float(number_bytes) {
            Some(n) => Token { ttype: TokenType::FLOAT(n) },
            None => Token { ttype: TokenType::ILLEGAL(number_bytes.into()) },
        }
    }

    let number_bytes = &bytes[pos..*read_pos];
    match to_int(number_bytes) {
        Some(n) => Token { ttype: TokenType::INT(n) },
        None => Token { ttype: TokenType::ILLEGAL(number_bytes.into()) },
    }
}

fn read_minus(bytes: &[u8], read_pos: &mut usize) -> Option<Token> {
    debug_assert_eq!(bytes[*read_pos], b'-');

    if *read_pos+1 > bytes.len() || bytes[*read_pos+1] != b'-' {
        *read_pos += 1;
        return Some(Token { ttype: TokenType::MINUS })
    }

    // If -- is found, discard til next \n
    read_while(bytes, read_pos, &|ch| ch != b'\n');
    None
}

#[inline]
fn eat_spaces(bytes: &[u8], read_pos: &mut usize) {
    debug_assert!(is_space(bytes[*read_pos]));

    read_while(bytes, read_pos, &is_space);
}

