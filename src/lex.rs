mod chars;
mod tests;
pub mod tokens;
pub mod utils;

use std::iter::Peekable;

use once_cell::sync::Lazy;
use regex_lite::Regex;

use crate::eval::Vm;

use self::tokens::{lookup_char, lookup_comparison, lookup_keyword, BinaryOp, Token, TokenType};
use self::utils::take_while_peeking;
use self::{
    chars::{is_alphabetic, is_numeric, is_space},
    utils::{eat_while_peeking, read_decimals},
};
use rua_trie::TrieWalker;

static STR_REPLACE_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"\\([\\'"])"#).expect("Regex is valid"));

pub struct Tokenizer<'vm, T>
where
    T: Iterator<Item = char> + Clone,
{
    input: Peekable<T>,
    vm: &'vm mut Vm,
    line: usize,
}

impl<'vm, T> Iterator for Tokenizer<'vm, T>
where
    T: Iterator<Item = char> + Clone,
{
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
                '\n' => {
                    self.input.next();
                    self.line += 1;
                    continue;
                }
                a if is_alphabetic(*a) => self.read_identifier(),
                n if is_numeric(*n) => self.read_number(),
                s if is_space(*s) => {
                    self.eat_spaces();
                    continue;
                }
                _ => self.single_char_token(),
            });
        }
        None
    }
}

impl<'vm, T> Tokenizer<'vm, T>
where
    T: Iterator<Item = char> + Clone,
{
    pub fn new(input: T, vm: &'vm mut Vm) -> Self {
        Self { input: input.peekable(), vm, line: 0 }
    }

    fn read_comparison(&mut self) -> Token {
        let ch = self.input.next().expect("Input cannot be empty here");
        if self.input.peek() == Some(&'=') {
            self.input.next();
            return Token { ttype: lookup_comparison(ch, true), line: self.line };
        }
        Token { ttype: lookup_comparison(ch, false), line: self.line }
    }

    fn read_minus(&mut self) -> Option<Token> {
        let ch = self.input.next();

        debug_assert_eq!(ch, Some('-'));

        if self.input.peek() != Some(&'-') {
            return Some(Token { ttype: TokenType::MINUS, line: self.line });
        }

        // If -- is found, discard til next \n
        self.input.next();
        eat_while_peeking(&mut self.input, &|ch: &char| *ch != '\n');
        None
    }

    fn read_dot(&mut self) -> Token {
        let ch = self.input.next();

        debug_assert_eq!(ch, Some('.'));

        if self.input.peek() == Some(&'.') {
            self.input.next();
            if self.input.peek() == Some(&'.') {
                self.input.next();
                return Token { ttype: TokenType::DOTDOTDOT, line: self.line };
            }
            return Token { ttype: TokenType::BINARY_OP(BinaryOp::DOTDOT), line: self.line };
        }

        match self.input.peek() {
            Some(n) if is_numeric(*n) => {
                let float = read_decimals(&mut self.input, 10);
                match float {
                    Ok(n) => Token { ttype: TokenType::NUMBER(n), line: self.line },
                    Err(s) => {
                        Token { ttype: TokenType::ILLEGAL(s.into_boxed_str()), line: self.line }
                    }
                }
            }
            _ => Token { ttype: TokenType::DOT, line: self.line },
        }
    }

    fn read_identifier(&mut self) -> Token {
        #![allow(clippy::unused_peekable)]
        let mut i = 0;
        let clone_it = self.input.clone();

        let mut trie_walker = TrieWalker::new(self.vm.identifiers());

        while let Some(ch) = self.input.next_if(|ch: &char| is_alphabetic(*ch) || is_numeric(*ch)) {
            i += 1;
            trie_walker.walk(ch);
        }
        let ttype = trie_walker.get_res();
        if let Some(ttype) = ttype {
            Token { ttype: ttype.clone(), line: self.line }
        } else {
            let s = clone_it.take(i).collect::<String>();
            let id = self.vm.new_string(s.into());
            Token {
                ttype: self
                    .vm
                    .identifiers()
                    .add_or_get(
                        &id,
                        if let Some(token) = lookup_keyword(&id) {
                            token
                        } else {
                            TokenType::IDENTIFIER(id.clone())
                        },
                    )
                    .clone(),
                line: self.line,
            }
        }
    }

    fn read_number(&mut self) -> Token {
        match utils::read_number(&mut self.input) {
            Ok(n) => Token { ttype: TokenType::NUMBER(n), line: self.line },
            Err(s) => Token { ttype: TokenType::ILLEGAL(s), line: self.line },
        }
    }

    #[inline]
    fn eat_spaces(&mut self) {
        self.input.next();

        eat_while_peeking(&mut self.input, &|ch| is_space(*ch));
    }

    #[inline]
    fn single_char_token(&mut self) -> Token {
        Token {
            ttype: lookup_char(self.input.next().expect("Input cannot be empty here")),
            line: self.line,
        }
    }

    fn read_string(&mut self) -> Token {
        let delimiter = self.input.next().expect("Input cannot be empty here");
        debug_assert!(delimiter == '\'' || delimiter == '"');
        let mut is_escaped = false;
        let s: String = take_while_peeking(&mut self.input, |ch| {
            if !is_escaped && *ch == delimiter {
                return false;
            }
            if *ch == '\n' {
                self.line += 1;
            }
            is_escaped = !is_escaped && *ch == '\\';
            true
        });
        match self.input.next() {
            Some(ch) => debug_assert_eq!(ch, delimiter),
            None => {
                return Token {
                    ttype: TokenType::ILLEGAL("Unclosed string literal".into()),
                    line: self.line,
                }
            }
        };

        let s = STR_REPLACE_RE.replace_all(&s, "$1");

        Token { ttype: TokenType::STRING(self.vm.new_string(s.into())), line: self.line }
    }

    pub(crate) fn vm(&mut self) -> &mut Vm {
        self.vm
    }
}
