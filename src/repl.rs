use std::io::{self, Write};

use crate::eval::vals::closure::Closure;
use crate::{
    compiler::{bytecode::ParseError, compile},
    eval::{
        vals::{EvalErrorTraced, RuaVal},
        Vm,
    },
};

pub fn run() -> io::Result<()> {
    println!("Welcome to Rua");
    let mut vm = Vm::new();
    loop {
        let mut input = String::new();
        let stdin = io::stdin();
        print!(">> ");
        io::stdout().flush()?;
        stdin.read_line(&mut input)?;

        if input.lines().count() == 1 {
            // Attempt to parse it as an expression
            let prog = parse_chars("return ".chars().chain(input.chars()), &mut vm);
            if let Ok(prog) = prog {
                print_res(vm.interpret(prog.into()));
                continue;
            }
        }

        let prog = parse_chars(input.chars(), &mut vm);

        match prog {
            Ok(prog) => print_res(vm.interpret(prog.into())),
            Err(err) => println!("Error: {err}"),
        }
    }
}

fn print_res(val: Result<RuaVal, EvalErrorTraced>) {
    match val {
        Ok(v) => println!("{v}"),
        Err(e) => println!("{e}"),
    }
}

fn parse_chars<T: Iterator<Item = char> + Clone>(
    input: T,
    vm: &mut Vm,
) -> Result<Closure, ParseError> {
    compile(input, vm)
}
