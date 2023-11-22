#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![deny(unused_must_use)]
#![deny(clippy::mod_module_files)]
#![allow(clippy::option_if_let_else)]

use std::path::PathBuf;

use compiler::{bytecode::ParseError, compile};
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
    let contents = std::fs::read(path) // TODO maybe not read file all at once? Need some perf testing
        .expect("Error: input file does not exist");

    let mut vm = Vm::new();

    let prog = compile(contents.iter().copied(), &mut vm)?;

    let res = vm.interpret(prog.into());
    match res {
        Ok(_) => {}
        Err(eval_err) => {
            println!("Exception: {eval_err}");
        }
    };

    Ok(())
}
