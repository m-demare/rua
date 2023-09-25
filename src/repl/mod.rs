use std::io::{self, Write};

use crate::identifiers::Trie;
use crate::lex::tokenize;
use crate::parser::parse;

pub fn run() -> io::Result<()>{
    println!("Welcome to Rua");
    let mut identifiers = Trie::new();
    loop {
        let mut input = String::new();
        let stdin = io::stdin();
        print!(">> ");
        io::stdout().flush()?;
        stdin.read_line(&mut input)?;

        let tokens = tokenize(&input, &mut identifiers);
        println!("Tokens: {tokens:?}");
        let ast = parse(&tokens);
        match ast {
            Ok(tree) => {println!("AST: {tree:?}")},
            Err(err) => {println!("AST: {err}")},
        }
    }
}

