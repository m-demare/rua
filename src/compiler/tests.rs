#![cfg(test)]

use crate::{compiler::bytecode::{Program, Instruction as I, Constant}, lex::Tokenizer, eval::{Vm, vals::IntoRuaVal}};

use pretty_assertions::assert_eq;
use super::{Compiler, bytecode::ParseError};

fn test_compile<F: FnOnce(&mut Vm) -> Result<Program, ParseError>>(input: &str, output: F){
    let mut vm = Vm::new();
    let compiler = Compiler::new(Tokenizer::new(input.chars(), &mut vm));
    let res = compiler.compile();

    assert_eq!(res, output(&mut vm));
}

#[test]
fn test_arithmetic_exprs() {
    let input = "return -5 + 1 * -6 - 2 * (3 + 4)";

    test_compile(input, |_| Ok(Program::new(
        vec![
            I::Constant(Constant(0)),
            I::Neg,
            I::Constant(Constant(1)),
            I::Constant(Constant(2)),
            I::Neg,
            I::Mul,
            I::Add,
            I::Constant(Constant(3)),
            I::Constant(Constant(4)),
            I::Constant(Constant(5)),
            I::Add,
            I::Mul,
            I::Sub,
            I::Return,
        ],
        vec![
            5.0.into(),
            1.0.into(),
            6.0.into(),
            2.0.into(),
            3.0.into(),
            4.0.into(),
    ],
        vec![(0, 14)],
    )));
}

#[test]
fn test_locals() {
    test_compile("
        local foo = 5 + 8
        return foo", |vm| Ok(Program::new(
        vec![
            I::Constant(Constant(0)),
            I::Constant(Constant(1)),
            I::Constant(Constant(2)),
            I::Add,
            I::DefineLocal,
            I::Constant(Constant(3)),
            I::GetGlobal,
            I::Return,
        ],
        vec![
            "foo".into_rua(vm),
            5.0.into(),
            8.0.into(),
            "foo".into_rua(vm),
    ],
        vec![(0, 0), (1, 5), (2, 3)],
    )));
}


#[test]
fn test_assign() {
    test_compile("
        foo = 5 + 8
        ", |vm| Ok(Program::new(
        vec![
            I::Constant(Constant(0)),
            I::Constant(Constant(1)),
            I::Constant(Constant(2)),
            I::Add,
            I::SetGlobal,
        ],
        vec![
            "foo".into_rua(vm),
            5.0.into(),
            8.0.into(),
    ],
        vec![(0, 0), (1, 5)],
    )));
}
