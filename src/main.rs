#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![allow(clippy::option_if_let_else)]
#![feature(box_patterns)]
#![feature(try_find)]
#![feature(iterator_try_collect)]

use std::{cell::RefCell, path::PathBuf};

use parser::ast::ParseError;

use crate::eval::{eval, scope::Scope};
use rua_identifiers::Trie;

mod cli;
mod eval;
mod lex;
mod parser;
mod repl;

fn main() {
    let cli = cli::parse_args();

    match &cli.path {
        Some(path) => {
            let _ = evaluate(path);
        }
        None => {
            let _ = repl::run();
        }
    }
}

fn evaluate(path: &PathBuf) -> Result<(), ParseError> {
    let contents = std::fs::read_to_string(path) // TODO maybe not read file all at once? Need some perf testing
        .expect("Error: input file does not exist");

    let mut identifiers = Trie::new();

    let tokens = lex::Tokenizer::new(contents.chars(), &mut identifiers);

    let ast = parser::parse(tokens)?;

    let env = Scope::new(RefCell::new(identifiers).into());
    let res = eval(&ast, &RefCell::new(env).into());
    println!("{res:?}");

    Ok(())
}
