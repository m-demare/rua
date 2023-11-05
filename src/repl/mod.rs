use std::io::{self, Write};

use crate::eval::vals::function::Function;
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

        let prog = parse_chars(input.chars(), &mut vm);

        match prog {
            Ok(prog) => {
                print_res(vm.interpret(prog));
            }
            Err(ParseError::UnexpectedExpression(_) | ParseError::UnexpectedEOF) => {
                let prog = parse_chars("return ".chars().chain(input.chars()), &mut vm);
                match prog {
                    Ok(prog) => {
                        print_res(vm.interpret(prog));
                    }
                    Err(err) => println!("Error: {err}"),
                }
            }
            Err(err) => println!("Error: {err}"),
        }
    }
}

fn print_res(val: Result<RuaVal, EvalErrorTraced>) {
    match val {
        Ok(..) => {}
        Err(e) => println!("{e}"),
    }
}

fn parse_chars<T: Iterator<Item = char> + Clone>(
    input: T,
    vm: &mut Vm,
) -> Result<Function, ParseError> {
    compile(input, vm)
}
