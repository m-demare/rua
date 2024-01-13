#![cfg(all(test, debug_assertions))]

use crate::eval::Vm;

use super::{
    bytecode::{
        BinArgs, Chunk, Instruction as I, NumberHandle, ParseError, StringHandle, UnArgs, VNArgs,
    },
    compile,
};
use pretty_assertions::assert_eq;

fn test_compile<F: FnOnce(&mut Vm) -> Result<Chunk, ParseError>>(input: &str, output: F) {
    println!("Program:\n{input}\n");

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
                I::MulVV(BinArgs { dst: 4, lhs: 1, rhs: 4 }),
                I::AddVV(BinArgs { dst: 3, lhs: 3, rhs: 4 }),
                I::AddVV(BinArgs { dst: 4, lhs: 2, rhs: 0 }),
                I::MulVV(BinArgs { dst: 4, lhs: 1, rhs: 4 }),
                I::SubVV(BinArgs { dst: 3, lhs: 3, rhs: 4 }),
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
                I::AddVN(VNArgs { dst: 0, lhs: 0, rhs: 1 }),
                I::Neg(UnArgs { dst: 1, src: 0 }),
                I::Return { src: 1 },
                I::ReturnNil,
            ],
            vec![5.0, 3.0],
            Vec::new(),
            Vec::new(),
            vec![(0, 1), (3, 1), (4, 2), (0, 1)],
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
                I::GetGlobal { dst: 0, src: StringHandle(0) },
                I::AddVN(VNArgs { dst: 0, lhs: 0, rhs: 0 }),
                I::SetGlobal { dst: StringHandle(1), src: 0 },
                I::GetGlobal { dst: 0, src: StringHandle(1) },
                I::Return { src: 0 },
                I::ReturnNil,
            ],
            vec![8.0],
            vec![vm.new_string((*b"bar").into()), vm.new_string((*b"foo").into())],
            Vec::new(),
            vec![(0, 1), (2, 2), (0, 1), (3, 1), (0, 1)],
        ))
    });
}

#[test]
fn test_number_fold() {
    let input = "return 5 + -8 * (4 + 2) ^ 3 % 28";

    test_compile(input, |_| {
        Ok(Chunk::new(
            vec![I::Number { dst: 0, src: NumberHandle(0) }, I::Return { src: 0 }, I::ReturnNil],
            vec![13.0],
            vec![],
            Vec::new(),
            vec![(0, 1), (1, 1), (0, 1)],
        ))
    });
}

#[test]
fn test_bool_fold() {
    let input = "return not (true and 5>#'asdf')";

    test_compile(input, |_| {
        Ok(Chunk::new(
            vec![I::False { dst: 0 }, I::Return { src: 0 }, I::ReturnNil],
            vec![],
            vec![],
            Vec::new(),
            vec![(0, 1), (1, 1), (0, 1)],
        ))
    });
}

#[test]
fn test_table_literal() {
    let input = "
        local n = 2
        local a = { true; b=3; [n + 3] = 'foo' }
        local b = a.b.c
        return b
";

    let capacity = (2 << 4) + 1;
    test_compile(input, |vm| {
        Ok(Chunk::new(
            vec![
                I::Number { dst: 0, src:NumberHandle(0) },
                I::NewTable { dst: 1, capacity },
                I::True { dst: 2 },
                I::InsertN { table: 1, key: 1, val: 2 },
                I::Number { dst: 2, src: NumberHandle(2) },
                I::InsertS { table: 1, key: 0, val: 2 },
                I::AddVN(VNArgs { dst: 2, lhs: 0, rhs: 2 }),
                I::String { dst: 3, src: StringHandle(1) },
                I::InsertV { table: 1, key: 2, val: 3 },
                I::IndexS(BinArgs { dst: 2, lhs: 1, rhs: 0 }),
                I::IndexS(BinArgs { dst: 2, lhs: 2, rhs: 2 }),
                I::Return { src: 2 },
                I::ReturnNil,
            ],
            vec![2.0, 1.0, 3.0],
            vec![vm.new_string((*b"b").into()), vm.new_string((*b"foo").into()), vm.new_string((*b"c").into())],
            Vec::new(),
            vec![(0, 1), (3, 1), (0, 1), (3, 1), (0, 1), (3, 2), (0, 1), (3, 1), (4, 2), (5, 1), (0, 1)],
        ))
    });
}
