#![cfg(all(test, debug_assertions))]

use crate::eval::Vm;

use super::{
    bytecode::{BinArgs, Chunk, Instruction as I, NumberHandle, ParseError, StringHandle, UnArgs},
    compile,
};
use pretty_assertions::assert_eq;

fn test_compile<F: FnOnce(&mut Vm) -> Result<Chunk, ParseError>>(input: &str, output: F) {
    println!("Program:\n{}\n", input);

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
fn test_locals() {
    let input = "
        local a, b, c
        return -a + b * -c - b * (c + a)";

    test_compile(input, |_| {
        Ok(Chunk::new(
            vec![
                I::Nil { dst: 0 },
                I::Nil { dst: 1 },
                I::Nil { dst: 2 },
                I::Neg(UnArgs { dst: 3, src: 0 }),
                I::Neg(UnArgs { dst: 4, src: 2 }),
                I::Mul(BinArgs { dst: 4, lhs: 1, rhs: 4 }),
                I::Add(BinArgs { dst: 3, lhs: 3, rhs: 4 }),
                I::Add(BinArgs { dst: 4, lhs: 2, rhs: 0 }),
                I::Mul(BinArgs { dst: 4, lhs: 1, rhs: 4 }),
                I::Sub(BinArgs { dst: 3, lhs: 3, rhs: 4 }),
                I::Return { src: 3 },
                I::ReturnNil,
            ],
            Vec::new(),
            Vec::new(),
            Vec::new(),
            vec![(0, 0), (2, 3), (3, 8), (0, 1)],
        ))
    });
}

#[test]
fn test_assign_local() {
    let input = "
        local a = 5
        a = a + 3
        return -a";

    test_compile(input, |_| {
        Ok(Chunk::new(
            vec![
                I::Number { dst: 0, src: NumberHandle(0) },
                I::Number { dst: 1, src: NumberHandle(1) },
                I::Add(BinArgs { dst: 0, lhs: 0, rhs: 1 }),
                I::Neg(UnArgs { dst: 1, src: 0 }),
                I::Return { src: 1 },
                I::ReturnNil,
            ],
            vec![5.0, 3.0],
            Vec::new(),
            Vec::new(),
            vec![(0, 2), (3, 1), (4, 2), (0, 1)],
        ))
    });
}

#[test]
fn test_globals() {
    let input = "
    foo = bar + 8
    return foo";

    test_compile(input, |vm| {
        Ok(Chunk::new(
            vec![
                I::GetGlobal { dst: 0, src: StringHandle(1) },
                I::Number { dst: 1, src: NumberHandle(0) },
                I::Add(BinArgs { dst: 0, lhs: 0, rhs: 1 }),
                I::SetGlobal { dst: StringHandle(0), src: 0 },
                I::GetGlobal { dst: 0, src: StringHandle(0) },
                I::Return { src: 0 },
                I::ReturnNil,
            ],
            vec![8.0],
            vec![vm.new_string((*b"foo").into()), vm.new_string((*b"bar").into())],
            Vec::new(),
            vec![(0, 2), (2, 2), (0, 1), (3, 1), (0, 1)],
        ))
    });
}
