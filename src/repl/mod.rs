use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::eval::{
    eval,
    scope::Scope,
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
    let env = Rc::new(RefCell::new(Scope::new(RefCell::new(identifiers).into())));
    loop {
        let mut input = String::new();
        let stdin = io::stdin();
        print!(">> ");
        io::stdout().flush()?;
        stdin.read_line(&mut input)?;

        let ast = parse_chars(input.chars(), &env);

        match ast {
            Ok(ref prog @ Program { ref statements, .. })
                if statements.len() != 1
                    || !matches!(statements.first(), Some(&Statement::Call(..))) =>
            {
                print_res(eval(prog, &env));
            }
            Err(
                ParseError::UnexpectedExpression
                | ParseError::UnexpectedToken(box Token { ttype: TokenType::BINARY_OP(_), .. }, _)
                | ParseError::UnexpectedEOF,
            )
            | Ok(Program { .. }) => {
                let ast = parse_chars("return ".chars().chain(input.chars()), &env);
                match ast {
                    Ok(expr) => print_res(eval(&expr, &env)),
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
    env: &Rc<RefCell<Scope>>,
) -> Result<Program, ParseError> {
    let ids = env.borrow_mut().identifiers();
    let mut ids = ids.borrow_mut();
    let tokens = Tokenizer::new(input, &mut ids);
    parse(tokens)
}
