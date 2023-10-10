use std::io::{self, Write};

use crate::identifiers::Trie;
use crate::lex::Tokenizer;
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

        let tokens = Tokenizer::new(input.chars(), &mut identifiers);
        let ast = parse(tokens);
        match ast {
            Ok(tree) => {println!("AST: {tree:?}")},
            Err(err) => {println!("AST: {err}")},
        }
    }
}

