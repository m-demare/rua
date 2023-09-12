#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![allow(clippy::option_if_let_else)]

use std::path::PathBuf;

mod cli;
mod repl;
mod lex;

fn main() {
    let cli = cli::parse_args();

    match &cli.path {
        Some(path) => { evaluate(path); },
        None => { let _ = repl::run(); },
    }
}

fn evaluate(path: &PathBuf) {
    let contents = std::fs::read_to_string(path)
        .expect("Error: input file does not exist");

    println!("Text:\n{contents}");

    println!("Tokens:\n{:?}", lex::tokenize(&contents));
}
