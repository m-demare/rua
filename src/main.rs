#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![deny(unused_must_use)]
#![allow(clippy::option_if_let_else)]

use std::path::PathBuf;

use compiler::{bytecode::ParseError, Compiler};
use eval::Vm;

mod cli;
mod compiler;
mod eval;
mod lex;
mod repl;

fn main() {
    let cli = cli::parse_args();

    match &cli.path {
        Some(path) => {
            match evaluate(path) {
                Ok(()) => {}
                Err(parse_err) => {
                    println!("ParseError: {parse_err}");
                }
            };
        }
        None => {
            let _ = repl::run();
        }
    }
}

fn evaluate(path: &PathBuf) -> Result<(), ParseError> {
    let contents = std::fs::read_to_string(path) // TODO maybe not read file all at once? Need some perf testing
        .expect("Error: input file does not exist");

    let mut vm = Vm::new();

    let mut tokens = lex::Tokenizer::new(contents.chars(), &mut vm);

    let prog = Compiler::new(&mut tokens).compile()?;

    let res = vm.interpret(prog);
    match res {
        Ok(_) => {}
        Err(eval_err) => {
            println!("Exception: {eval_err}");
        }
    };

    Ok(())
}
