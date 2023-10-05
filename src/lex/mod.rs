mod chars;
mod utils;
pub mod tokens;

use std::iter::Peekable;
use std::str::Chars;

use self::tokens::{Token, TokenType, lookup_char, lookup_comparison, BinaryOp};
use self::{utils::{read_decimals, eat_while_peeking}, chars::{is_alphabetic, is_numeric, is_space}};
use crate::identifiers::{TrieWalker, Trie};

pub fn tokenize(input: &str, identifiers: &mut Trie) -> Vec<Token> {
    let mut tokens = Vec::with_capacity(100);

    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.peek() {
        let token = match ch {
            '=' | '<' | '>' | '~' => Some(read_comparison(chars.by_ref())),
            '-' => read_minus(chars.by_ref()),
            '.' => Some(read_dot(chars.by_ref())),
            a if is_alphabetic(*a) => Some(read_identifier(chars.by_ref(), identifiers)),
            n if is_numeric(*n) => Some(read_number(chars.by_ref())),
            s if is_space(*s) => { eat_spaces(chars.by_ref()); None },
            _ => Some(single_char_token(chars.by_ref())),
        };
        if let Some(t) = token {
            tokens.push(t);
        }
    }

    tokens.push(Token { ttype: TokenType::EOF });

    tokens
}

#[inline]
fn single_char_token(chars: &mut Peekable<Chars>) -> Token {
    Token { ttype: lookup_char(chars.next().expect("Chars cannot be empty here")) }
}

fn read_comparison(chars: &mut Peekable<Chars>) -> Token {
    let ch = chars.next().expect("Chars cannot be empty here");
    if chars.peek() == Some(&'='){
        chars.next();
        return Token { ttype: lookup_comparison(ch, true) };
    }
    Token { ttype: lookup_comparison(ch, false) }
}

fn read_dot(chars: &mut Peekable<Chars>) -> Token {
    let ch = chars.next();

    debug_assert_eq!(ch, Some('.'));

    if chars.peek() == Some(&'.') {
        chars.next();
        if chars.peek() == Some(&'.') {
            chars.next();
            return Token { ttype: TokenType::DOTDOTDOT}
        }
        return Token { ttype: TokenType::BINARY_OP(BinaryOp::DOTDOT) }
    }

    match chars.peek() {
        Some(n) if is_numeric(*n) => {
            let float = read_decimals(chars, 10);
            match float {
                Ok(n) => Token { ttype: TokenType::NUMBER(n) },
                Err(s) => Token { ttype: TokenType::ILLEGAL(s.into_boxed_str()) },
            }
        },
        _ => Token { ttype: TokenType::DOT},
    }
}

fn read_identifier(chars: &mut Peekable<Chars>, identifiers: &mut Trie) -> Token {
    #![allow(clippy::unused_peekable)]
    let mut i = 0;
    let clone_it = chars.clone();

    let mut trie_walker = TrieWalker::new(identifiers);

    while let Some(ch) = chars.next_if(|ch: &char| is_alphabetic(*ch) || is_numeric(*ch)) {
        i += 1;
        trie_walker.walk(ch);
    }
    let identifier = trie_walker.get_res();
    match identifier {
        Some(id) => Token { ttype: id },
        None => Token { ttype: identifiers.add_or_get(&clone_it.take(i).collect::<String>()) },
    }
}

fn read_number(chars: &mut Peekable<Chars>) -> Token {
    let mut radix = 10;
    if chars.peek() == Some(&'0') {
        chars.next();
        if let Some(ch) = chars.peek() {
            radix = match ch {
                'x' => { chars.next(); 16 },
                'b' => { chars.next(); 2 },
                _ => 10,
            }
        }
    }
    match utils::read_number(chars, radix) {
        Ok(n) => Token { ttype: TokenType::NUMBER(n) },
        Err(s) => Token { ttype: TokenType::ILLEGAL(format!("{}{}", if radix == 2 {"0b"} else {"0x"}, s).into_boxed_str())},
    }
}

fn read_minus(chars: &mut Peekable<Chars>) -> Option<Token> {
    let ch = chars.next();

    debug_assert_eq!(ch, Some('-'));

    if chars.peek() != Some(&'-') {
        return Some(Token { ttype: TokenType::MINUS })
    }

    // If -- is found, discard til next \n
    chars.next();
    eat_while_peeking(chars, &|ch: &char| *ch != '\n');
    chars.next();
    None
}

#[inline]
fn eat_spaces(chars: &mut Peekable<Chars>) {
    chars.next();
    
    eat_while_peeking(chars, &|ch| is_space(*ch));
}

