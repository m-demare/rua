#![cfg(test)]

use crate::{compiler::bytecode::{Chunk, Instruction as I, Constant}, eval::{Vm, vals::IntoRuaVal}};

use pretty_assertions::assert_eq;
use super::{bytecode::ParseError, compile, locals::LocalHandle};

fn test_compile<F: FnOnce(&mut Vm) -> Result<Chunk, ParseError>>(input: &str, output: F){
    let mut vm = Vm::new();
    let res = compile(input.chars(), &mut vm);

    match (res, output(&mut vm)) {
        (Ok(func), Ok(chunk)) => assert_eq!(func.chunk(), &chunk),
        (Err(err), Ok(_)) => println!("Failed with error {err}"),
        (Ok(func), Err(err)) => println!("Expedted {err}, got {func:?} instead"),
        (Err(e1), Err(e2)) => assert_eq!(e1, e2),
    }
}

#[test]
fn test_arithmetic_exprs() {
    let input = "return -5 + 1 * -6 - 2 * (3 + 4)";

    test_compile(input, |_| Ok(Chunk::new(
        vec![
            I::CheckStack(0),
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
            I::CheckStack(0),
            I::ReturnNil,
        ],
        vec![
            5.0.into(),
            1.0.into(),
            6.0.into(),
            2.0.into(),
            3.0.into(),
            4.0.into(),
    ],
        vec![(0, 17)],
    )));
}

#[test]
fn test_locals() {
    test_compile("
        local foo = 5 + 8
        return foo", |_| Ok(Chunk::new(
        vec![
            I::CheckStack(0),
            I::Constant(Constant(0)),
            I::Constant(Constant(1)),
            I::Add,
            I::CheckStack(1),
            I::GetLocal(LocalHandle(0)),
            I::Return,
            I::CheckStack(1),
            I::Pop,
            I::ReturnNil,
        ],
        vec![
            5.0.into(),
            8.0.into(),
    ],
        vec![(0, 1), (1, 3), (0, 1), (2, 2), (0, 3)],
    )));
    test_compile("
        local foo = 5 + 8
        local bar = 3
        local foo = foo + bar
        return foo", |_| Ok(Chunk::new(
        vec![
            I::CheckStack(0),
            I::Constant(Constant(0)),
            I::Constant(Constant(1)),
            I::Add,
            I::CheckStack(1),
            I::Constant(Constant(2)),
            I::CheckStack(2),
            I::GetLocal(LocalHandle(0)),
            I::GetLocal(LocalHandle(1)),
            I::Add,
            I::CheckStack(3),
            I::GetLocal(LocalHandle(2)),
            I::Return,
            I::CheckStack(3),
            I::Pop,
            I::Pop,
            I::Pop,
            I::ReturnNil,
        ],
        vec![
            5.0.into(),
            8.0.into(),
            3.0.into(),
    ],
        vec![(0, 1), (1, 3), (0, 1), (2, 1), (0, 1), (3, 3), (0, 1), (4, 2), (0, 5)],
    )));
}


#[test]
fn test_assign() {
    test_compile("
        foo = 5 + 8
        ", |vm| Ok(Chunk::new(
        vec![
            I::CheckStack(0),
            I::Constant(Constant(0)),
            I::Constant(Constant(1)),
            I::Constant(Constant(2)),
            I::Add,
            I::SetGlobal,
            I::CheckStack(0),
            I::ReturnNil,
        ],
        vec![
            "foo".into_rua(vm),
            5.0.into(),
            8.0.into(),
    ],
        vec![(0, 1), (1, 5), (0, 2)],
    )));
}
