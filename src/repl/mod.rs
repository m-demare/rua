use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::eval::{
    eval,
    isolate::Isolate,
    vals::{EvalError, StmtResult},
};
use crate::lex::{
    tokens::{Token, TokenType},
    Tokenizer,
};
use crate::parser::{
    ast::{ParseError, Program, Statement},
    parse,
};
use rua_identifiers::Trie;

pub fn run() -> io::Result<()> {
    println!("Welcome to Rua");
    let identifiers = Trie::new();

    let isolate = Rc::new(RefCell::new(Isolate::new(identifiers)));
    loop {
        let mut input = String::new();
        let stdin = io::stdin();
        print!(">> ");
        io::stdout().flush()?;
        stdin.read_line(&mut input)?;

        let ast = parse_chars(input.chars(), &isolate);

        match ast {
            Ok(ref prog @ Program { ref statements, .. })
                if statements.len() != 1
                    || !matches!(statements.first(), Some(&Statement::Call(..))) =>
            {
                print_res(eval(prog, isolate.clone()));
            }
            Err(
                ParseError::UnexpectedExpression
                | ParseError::UnexpectedToken(box Token { ttype: TokenType::BINARY_OP(_), .. }, _)
                | ParseError::UnexpectedEOF,
            )
            | Ok(Program { .. }) => {
                let ast = parse_chars("return ".chars().chain(input.chars()), &isolate);
                match ast {
                    Ok(expr) => print_res(eval(&expr, isolate.clone())),
                    Err(err) => println!("Error: {err}"),
                }
            }
            Err(err) => println!("Error: {err}"),
        }
    }
}

fn print_res(val: Result<StmtResult, EvalError>) {
    match val {
        Ok(StmtResult::Return(v)) => println!("{v:?}"),
        Ok(..) => {}
        Err(e) => println!("{e}"),
    }
}

fn parse_chars<T: Iterator<Item = char> + Clone>(
    input: T,
    isolate: &Rc<RefCell<Isolate>>,
) -> Result<Program, ParseError> {
    let mut isolate = isolate.borrow_mut();
    let ids = isolate.identifiers_mut();
    let tokens = Tokenizer::new(input, ids);
    parse(tokens)
}
