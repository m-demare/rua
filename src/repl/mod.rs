use std::io::{self, Write};

use rua::lex::tokenize;

pub fn run() -> io::Result<()>{
    println!("Welcome to Rua");
    loop {
        let mut input = String::new();
        let stdin = io::stdin();
        print!(">> ");
        io::stdout().flush()?;
        stdin.read_line(&mut input)?;

        let tokens = tokenize(&input);
        println!("{tokens:?}");
    }
}

