use std::{io::{self, Write}, cell::RefCell, rc::Rc};

use crate::{eval::{scope::Scope, vals::EvalError}, lex::tokens::{TokenType, Token}, identifiers::Trie};
use crate::lex::Tokenizer;
use crate::parser::ast::{Precedence, ExpressionContext, ParseError};
use crate::parser::{parse, parse_expression};
use crate::eval::eval;

pub fn run() -> io::Result<()>{
    println!("Welcome to Rua");
    let identifiers = Trie::new();
    let env = Rc::new(RefCell::new(Scope::new(RefCell::new(identifiers).into())));
    loop {
        let mut input = String::new();
        let stdin = io::stdin();
        print!(">> ");
        io::stdout().flush()?;
        stdin.read_line(&mut input)?;

        let ids = env.borrow_mut().identifiers();
        let mut ids = ids.borrow_mut();
        let tokens = Tokenizer::new(input.chars(), &mut ids);
        let ast = parse(tokens);
        match ast {
            Ok(tree) => print_res(eval(&tree, &env)),
            Err(ParseError::UnexpectedExpression | ParseError::UnexpectedToken(box Token{ttype: TokenType::BINARY_OP(_), ..}, _) | ParseError::UnexpectedEOF) => {
                let tokens = Tokenizer::new(input.chars(), &mut ids);
                let expr = parse_expression(tokens.peekable().by_ref(), &Precedence::Lowest, &ExpressionContext::Return);
                match expr {
                    Ok(exp) => print_res(exp.eval(env.clone())),
                    Err(err) => println!("Error: {err}"),
                }
            },
            Err(err) => println!("Error: {err}"),
        }
    }
}

fn print_res<T: std::fmt::Debug>(val: Result<T, EvalError>) {
    match val {
        Ok(v) => println!("{v:?}"),
        Err(e) => println!("{e}"),
    }
}

