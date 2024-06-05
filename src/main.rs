#![warn(clippy::pedantic, clippy::nursery, clippy::unwrap_used, clippy::perf)]
#![deny(unused_must_use)]
#![deny(clippy::mod_module_files)]
#![allow(clippy::option_if_let_else)]
#![feature(hash_raw_entry)]

use std::{io, path::PathBuf, rc::Rc};

use clap::Parser;
use cli::Args;
use compiler::{bytecode::ParseError, compile};
use eval::Vm;
use mimalloc::MiMalloc;

#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

mod cli;
pub mod compiler;
pub mod eval;
pub mod lex;
mod repl;
pub mod wasm;

fn main() {
    let mut args = Args::parse();

    let varargs: Vec<_> = std::mem::take(&mut args.args).into_iter().map(Into::into).collect();
    match &args.path {
        Some(path) => {
            match evaluate(path, &args, &varargs) {
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

fn evaluate(path: &PathBuf, args: &Args, varargs: &[Rc<[u8]>]) -> Result<(), ParseError> {
    let contents = std::fs::read(path).expect("Error: input file does not exist");

    let mut vm = Vm::with_args(varargs, Box::new(io::stdout()));

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
