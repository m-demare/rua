mod chars;
pub mod utils;
pub mod tokens;
mod tests;

use std::iter::Peekable;

use regex_lite::Regex;

use self::tokens::{Token, TokenType, lookup_char, lookup_comparison, BinaryOp};
use self::utils::take_while_peeking;
use self::{utils::{read_decimals, eat_while_peeking}, chars::{is_alphabetic, is_numeric, is_space}};
use crate::identifiers::{TrieWalker, Trie};

pub struct Tokenizer<'ids, T> where T: Iterator<Item = char> + Clone {
    input: Peekable<T>,
    identifiers: &'ids mut Trie,
    str_replace_re: Box<Regex>,
}

impl<'ids, T> Iterator for Tokenizer<'ids, T> where T: Iterator<Item = char> + Clone {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(ch) = self.input.peek() {
            return Some(match ch {
                '=' | '<' | '>' | '~' => self.read_comparison(),
                '-' => match self.read_minus() {
                    Some(t) => t,
                    None => continue,
                },
                '\'' | '"' => self.read_string(),
                '.' => self.read_dot(),
                a if is_alphabetic(*a) => self.read_identifier(),
                n if is_numeric(*n) => self.read_number(),
                s if is_space(*s) => { self.eat_spaces(); continue },
                _ => self.single_char_token(),
            });
        }
        None
    }
}

impl<'ids, T> Tokenizer<'ids, T> where T: Iterator<Item = char> + Clone {
    pub fn new(input: T, identifiers: &'ids mut Trie) -> Self {
        let str_replace_re = Regex::new(r#"\\([\\'"])"#).expect("Regex is valid").into();
        Self { input: input.peekable(), identifiers, str_replace_re }
    }

    fn read_comparison(&mut self) -> Token {
        let ch = self.input.next().expect("Input cannot be empty here");
        if self.input.peek() == Some(&'='){
            self.input.next();
            return Token { ttype: lookup_comparison(ch, true) };
        }
        Token { ttype: lookup_comparison(ch, false) }
    }

    fn read_minus(&mut self) -> Option<Token> {
        let ch = self.input.next();

        debug_assert_eq!(ch, Some('-'));

        if self.input.peek() != Some(&'-') {
            return Some(Token { ttype: TokenType::MINUS })
        }

        // If -- is found, discard til next \n
        self.input.next();
        eat_while_peeking(&mut self.input, &|ch: &char| *ch != '\n');
        self.input.next();
        None
    }

    fn read_dot(&mut self) -> Token {
        let ch = self.input.next();

        debug_assert_eq!(ch, Some('.'));

        if self.input.peek() == Some(&'.') {
            self.input.next();
            if self.input.peek() == Some(&'.') {
                self.input.next();
                return Token { ttype: TokenType::DOTDOTDOT}
            }
            return Token { ttype: TokenType::BINARY_OP(BinaryOp::DOTDOT) }
        }

        match self.input.peek() {
            Some(n) if is_numeric(*n) => {
                let float = read_decimals(&mut self.input, 10);
                match float {
                    Ok(n) => Token { ttype: TokenType::NUMBER(n) },
                    Err(s) => Token { ttype: TokenType::ILLEGAL(s.into_boxed_str()) },
                }
            },
            _ => Token { ttype: TokenType::DOT},
        }
    }

    fn read_identifier(&mut self) -> Token {
        #![allow(clippy::unused_peekable)]
        let mut i = 0;
        let clone_it = self.input.clone();

        let mut trie_walker = TrieWalker::new(self.identifiers);

        while let Some(ch) = self.input.next_if(|ch: &char| is_alphabetic(*ch) || is_numeric(*ch)) {
            i += 1;
            trie_walker.walk(ch);
        }
        let identifier = trie_walker.get_res();
        match identifier {
            Some(id) => Token { ttype: id },
            None => Token { ttype: self.identifiers.add_or_get(&clone_it.take(i).collect::<String>()) },
        }
    }

    fn read_number(&mut self) -> Token {
        match utils::read_number(&mut self.input) {
            Ok(n) => Token { ttype: TokenType::NUMBER(n) },
            Err(s) => Token { ttype: TokenType::ILLEGAL(s)},
        }
    }

    #[inline]
    fn eat_spaces(&mut self) {
        self.input.next();

        eat_while_peeking(&mut self.input, &|ch| is_space(*ch));
    }

    #[inline]
    fn single_char_token(&mut self) -> Token {
        Token { ttype: lookup_char(self.input.next().expect("Input cannot be empty here")) }
    }

    fn read_string(&mut self) -> Token {
        let delimiter = self.input.next().expect("Input cannot be empty here");
        debug_assert!(delimiter == '\'' || delimiter == '"');
        let mut is_escaped = false;
        let s: String = take_while_peeking(&mut self.input, |ch| {
            if !is_escaped && *ch == delimiter {
                return false
            }
            is_escaped = !is_escaped && *ch=='\\';
            true
        });
        match self.input.next() {
            Some(ch) => debug_assert_eq!(ch, delimiter),
            None => return Token { ttype: TokenType::ILLEGAL("Unclosed string literal".into()) },
        };

        let s = self.str_replace_re.replace_all(&s, "$1");

        Token { ttype: TokenType::STRING(s.into()) }
    }
}

