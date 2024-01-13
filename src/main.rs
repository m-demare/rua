#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![deny(unused_must_use)]
#![deny(clippy::mod_module_files)]
#![allow(clippy::option_if_let_else)]

use std::path::PathBuf;

use clap::Parser;
use cli::Args;
use compiler::{bytecode::ParseError, compile};
use eval::Vm;

mod cli;
pub mod compiler;
pub mod eval;
pub mod lex;
mod repl;

fn main() {
    let args = Args::parse();

    match &args.path {
        Some(path) => {
            match evaluate(path, &args) {
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

fn evaluate(path: &PathBuf, args: &Args) -> Result<(), ParseError> {
    let contents = std::fs::read(path).expect("Error: input file does not exist");

    let mut vm = Vm::new();

    let prog = compile(contents.iter().copied(), &mut vm)?;

    if args.list {
        println!("{:?}", prog.function());
    }
    if args.parse_only {
        return Ok(());
    }

    let res = vm.interpret(prog.into());
    match res {
        Ok(_) => {}
        Err(eval_err) => {
            println!("Exception: {eval_err}");
        }
    };

    Ok(())
}
