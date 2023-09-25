#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![allow(clippy::option_if_let_else)]

use std::path::PathBuf;

use crate::identifiers::Trie;

mod cli;
mod repl;
mod lex;
mod parser;
mod identifiers;

fn main() {
    let cli = cli::parse_args();

    match &cli.path {
        Some(path) => { evaluate(path); },
        None => { let _ = repl::run(); },
    }
}

fn evaluate(path: &PathBuf) {
    let contents = std::fs::read_to_string(path) // TODO maybe not read file all at once? Need some perf testing
        .expect("Error: input file does not exist");

    let mut identifiers = Trie::new();

    println!("Text:\n{contents}");

    let tokens = lex::tokenize(&contents, &mut identifiers);
    println!("Tokens:\n{tokens:?}");
    
    let ast = parser::parse(&tokens);
    println!("AST:\n{ast:?}");
}
