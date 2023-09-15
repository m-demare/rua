mod chars;
mod utils;
pub mod tokens;

use std::iter::Peekable;
use std::str::Chars;

use crate::lex::tokens::{Token, TokenType, lookup_char, lookup_comparison, lookup_ident};
use crate::lex::{chars::{is_alphabetic, is_hex, is_numeric, is_numeric_base, is_space}, utils::eat_while_peeking};
use self::utils::{take_while_peeking, to_float, to_int};

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::with_capacity(100);

    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        let token = get_token(ch, chars.by_ref());
        if let Some(t) = token {
            tokens.push(t);
        }
    }

    tokens.push(Token { ttype: TokenType::EOF });

    tokens
}

fn get_token(ch: char, chars: &mut Peekable<Chars>) -> Option<Token> {
    match ch {
        '=' | '<' | '>' | '~' => Some(read_comparison(ch, chars)),
        '-' => read_minus(ch, chars),
        '.' => Some(read_dot(chars)),
        a if is_alphabetic(&a) => Some(read_identifier(ch, chars)),
        n if is_numeric(&n) => Some(read_number(ch, chars)),
        s if is_space(&s) => { eat_spaces(chars); None },
        _ => Some(single_char_token(ch)),
    }
}

#[inline]
fn single_char_token(ch: char) -> Token {
    Token { ttype: lookup_char(ch) }
}

fn read_comparison(ch: char, chars: &mut Peekable<Chars>) -> Token {
    if chars.peek() == Some(&'='){
        chars.next();
        return Token { ttype: lookup_comparison(ch, true) };
    }
    Token { ttype: lookup_comparison(ch, false) }
}

fn read_dot(chars: &mut Peekable<Chars>) -> Token {
    if chars.peek() == Some(&'.') {
        chars.next();
        if chars.peek() == Some(&'.') {
            chars.next();
            return Token { ttype: TokenType::DOTDOTDOT}
        }
        return Token { ttype: TokenType::DOTDOT}
    }

    match chars.peek() {
        Some(n) if is_numeric(n) => {
            let float = take_while_peeking(chars, &|ch| is_hex(ch) || *ch == '.');
            match to_float(&("0.".to_owned() + &float)) {
                Some(n) => Token { ttype: TokenType::FLOAT(n) },
                None => Token { ttype: TokenType::ILLEGAL(float) },
            }
        },
        _ => Token { ttype: TokenType::DOT},
    }
}

fn read_identifier(ch: char, chars: &mut Peekable<Chars>) -> Token {
    debug_assert!(is_alphabetic(&ch));

    let identifier = take_while_peeking(chars, &|ch| is_alphabetic(ch) || is_numeric(ch));

    Token {
        ttype: lookup_ident((ch.to_string() + &identifier).as_str())
    }
}

fn read_number(ch: char, chars: &mut Peekable<Chars>) -> Token {
    debug_assert!(is_numeric(&ch));

    let int = ch.to_string() + &take_while_peeking(chars, &|ch| is_hex(ch) || is_numeric_base(ch));
    if chars.peek() == Some(&'.') {
        let float_part = take_while_peeking(chars, &|ch| is_hex(ch) || *ch == '.');

        let s = int + &float_part;
        return match to_float(&s) {
            Some(n) => Token { ttype: TokenType::FLOAT(n) },
            None => Token { ttype: TokenType::ILLEGAL(s) },
        }
    }

    match to_int(&int) {
        Some(n) => Token { ttype: TokenType::INT(n) },
        None => Token { ttype: TokenType::ILLEGAL(int) },
    }
}

fn read_minus(ch: char, chars: &mut Peekable<Chars>) -> Option<Token> {
    debug_assert_eq!(ch, '-');

    if chars.peek() != Some(&'-') {
        return Some(Token { ttype: TokenType::MINUS })
    }

    // If -- is found, discard til next \n
    eat_while_peeking(chars, &|ch| *ch != '\n');
    None
}

#[inline]
fn eat_spaces(chars: &mut Peekable<Chars>) {
    eat_while_peeking(chars, &is_space);
}

