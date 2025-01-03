mod chars;
mod tests;
pub mod tokens;
pub(crate) mod utils;

use std::iter::Peekable;

use crate::eval::Vm;

use self::tokens::{lookup_char, lookup_comparison, lookup_keyword, Token, TokenType};
use self::{
    chars::{is_alphabetic, is_numeric, is_space},
    utils::{eat_while_peeking, read_decimals},
};
use rua_trie::TrieWalker;

pub struct Tokenizer<'vm, T>
where
    T: Iterator<Item = u8> + Clone,
{
    input: Peekable<T>,
    vm: &'vm mut Vm,
    line: usize,
}

impl<T> Iterator for Tokenizer<'_, T>
where
    T: Iterator<Item = u8> + Clone,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(ch) = self.input.peek() {
            return Some(match ch {
                b'=' | b'<' | b'>' | b'~' => self.read_comparison(),
                b'-' => match self.read_minus() {
                    Some(t) => t,
                    None => continue,
                },
                b'\'' | b'"' => self.read_string(),
                b'.' => self.read_dot(),
                b'\n' => {
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
    T: Iterator<Item = u8> + Clone,
{
    pub fn new(input: T, vm: &'vm mut Vm) -> Self {
        Self { input: input.peekable(), vm, line: 1 }
    }

    fn read_comparison(&mut self) -> Token {
        let ch = self.input.next().expect("Input cannot be empty here");
        if self.input.peek() == Some(&b'=') {
            self.input.next();
            return Token { ttype: lookup_comparison(ch, true), line: self.line };
        }
        Token { ttype: lookup_comparison(ch, false), line: self.line }
    }

    fn read_minus(&mut self) -> Option<Token> {
        let ch = self.input.next();

        debug_assert_eq!(ch, Some(b'-'));

        if self.input.peek() != Some(&b'-') {
            return Some(Token { ttype: TokenType::MINUS, line: self.line });
        }

        // If -- is found, discard til next \n
        self.input.next();
        eat_while_peeking(&mut self.input, &|ch: &u8| *ch != b'\n');
        None
    }

    fn read_dot(&mut self) -> Token {
        let ch = self.input.next();

        debug_assert_eq!(ch, Some(b'.'));

        if self.input.peek() == Some(&b'.') {
            self.input.next();
            if self.input.peek() == Some(&b'.') {
                self.input.next();
                return Token { ttype: TokenType::DOTDOTDOT, line: self.line };
            }
            return Token { ttype: TokenType::DOTDOT, line: self.line };
        }

        match self.input.peek() {
            Some(n) if is_numeric(*n) => {
                let float = read_decimals(&mut self.input, 10, 0f64);
                match float {
                    Ok(n) => Token { ttype: TokenType::NUMBER(n), line: self.line },
                    Err(s) => Token { ttype: TokenType::ILLEGAL(s), line: self.line },
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

        while let Some(ch) = self.input.next_if(|ch: &u8| is_alphabetic(*ch) || is_numeric(*ch)) {
            i += 1;
            trie_walker.walk(ch);
        }
        let ttype = trie_walker.get_res();
        if let Some(ttype) = ttype {
            Token { ttype: ttype.clone(), line: self.line }
        } else {
            let s = clone_it.take(i).collect::<Vec<_>>();
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
        debug_assert!(delimiter == b'\'' || delimiter == b'"');
        let mut is_escaped = false;
        let mut acc = Vec::new();
        for ch in self.input.by_ref() {
            if ch == delimiter && !is_escaped {
                return Token {
                    ttype: TokenType::STRING(self.vm.new_string(acc.into())),
                    line: self.line,
                };
            }
            if ch == b'\n' {
                self.line += 1;
            }
            if is_escaped {
                match ch {
                    b'\'' | b'\"' | b'\\' => acc.push(ch),
                    b'n' => acc.push(b'\n'),
                    b't' => acc.push(b'\t'),
                    b'0' => acc.push(b'\0'),
                    _ => {
                        acc.push(b'\\');
                        acc.push(ch);
                        return Token {
                            ttype: TokenType::ILLEGAL(String::from_utf8_lossy(&acc).into()),
                            line: self.line,
                        };
                    }
                }
                is_escaped = false;
            } else if ch != b'\\' {
                acc.push(ch);
            } else {
                is_escaped = true;
            }
        }
        Token { ttype: TokenType::ILLEGAL("Unclosed string literal".into()), line: self.line }
    }

    pub(crate) fn vm(&mut self) -> &mut Vm {
        self.vm
    }
}
