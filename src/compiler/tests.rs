#![cfg(all(test, debug_assertions))]

use crate::eval::Vm;

use super::{
    bytecode::{Chunk, Instruction as I, NumberHandle, ParseError, StringHandle},
    compile,
    locals::LocalHandle,
};
use pretty_assertions::assert_eq;

fn test_compile<F: FnOnce(&mut Vm) -> Result<Chunk, ParseError>>(input: &str, output: F) {
    let mut vm = Vm::new();
    let res = compile(input.bytes(), &mut vm);

    match (res, output(&mut vm)) {
        (Ok(closure), Ok(chunk)) => assert_eq!(closure.function().chunk(), &chunk),
        (Err(err), Ok(_)) => println!("Failed with error {err}"),
        (Ok(closure), Err(err)) => println!("Expedted {err}, got {:?} instead", closure.function()),
        (Err(e1), Err(e2)) => assert_eq!(e1, e2),
    }
}

#[test]
fn test_arithmetic_exprs() {
    let input = "return -5 + 1 * -6 - 2 * (3 + 4)";

    test_compile(input, |_| {
        Ok(Chunk::new(
            vec![
                I::CheckStack(0),
                I::Number(NumberHandle(0)),
                I::Neg,
                I::Number(NumberHandle(1)),
                I::Number(NumberHandle(2)),
                I::Neg,
                I::Mul,
                I::Add,
                I::Number(NumberHandle(3)),
                I::Number(NumberHandle(4)),
                I::Number(NumberHandle(5)),
                I::Add,
                I::Mul,
                I::Sub,
                I::Return,
                I::CheckStack(0),
                I::ReturnNil,
            ],
            vec![5.0, 1.0, 6.0, 2.0, 3.0, 4.0],
            Vec::new(),
            Vec::new(),
            vec![(0, 1), (1, 14), (0, 2)],
        ))
    });
}

#[test]
fn test_locals() {
    test_compile(
        "
        local foo = 5 + 8
        return foo",
        |_| {
            Ok(Chunk::new(
                vec![
                    I::CheckStack(0),
                    I::Number(NumberHandle(0)),
                    I::Number(NumberHandle(1)),
                    I::Add,
                    I::CheckStack(1),
                    I::GetLocal(LocalHandle(0)),
                    I::Return,
                    I::CheckStack(1),
                    I::Pop,
                    I::ReturnNil,
                ],
                vec![5.0, 8.0],
                Vec::new(),
                Vec::new(),
                vec![(0, 1), (2, 3), (0, 1), (3, 2), (0, 3)],
            ))
        },
    );
    test_compile(
        "
        local foo = 5 + 8
        local bar = 3
        local foo = foo + bar
        return foo",
        |_| {
            Ok(Chunk::new(
                vec![
                    I::CheckStack(0),
                    I::Number(NumberHandle(0)),
                    I::Number(NumberHandle(1)),
                    I::Add,
                    I::CheckStack(1),
                    I::Number(NumberHandle(2)),
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
                vec![5.0, 8.0, 3.0],
                Vec::new(),
                Vec::new(),
                vec![(0, 1), (2, 3), (0, 1), (3, 1), (0, 1), (4, 3), (0, 1), (5, 2), (0, 5)],
            ))
        },
    );
}

#[test]
fn test_assign() {
    test_compile(
        "
        foo = 5 + 8
        ",
        |vm| {
            Ok(Chunk::new(
                vec![
                    I::CheckStack(0),
                    I::Number(NumberHandle(0)),
                    I::Number(NumberHandle(1)),
                    I::Add,
                    I::SetGlobal(StringHandle(0)),
                    I::CheckStack(0),
                    I::ReturnNil,
                ],
                vec![5.0, 8.0],
                vec![vm.new_string((*b"foo").into())],
                Vec::new(),
                vec![(0, 1), (2, 4), (0, 2)],
            ))
        },
    );
}
