#![cfg(test)]

use crate::{compiler::Compiler, lex::Tokenizer, eval::vals::{RuaVal, EvalError, IntoRuaVal}};

use super::Vm;

fn test_interpret<F: FnOnce(&mut Vm) -> Result<RuaVal, EvalError>>(input: &str, output: F){
    let mut vm = Vm::new();

    let compiler = Compiler::new(Tokenizer::new(input.chars(), &mut vm));
    let prog = compiler.compile().expect("Failed to compile program");

    let res = vm.interpret(prog);
    assert_eq!(res, output(&mut vm));
}


#[test]
fn test_arithmetic_ops() {
    test_interpret("return (5 + -2) * 4", |_| Ok(12.0.into()));
    test_interpret("return - -2 / 4 + 1", |_| Ok(1.5.into()));
    test_interpret("return 2+5 > 6", |_| Ok(true.into()));
}

#[test]
fn test_string_ops() {
    test_interpret("return 'hello' .. ' ' .. 'world'", |vm| Ok(vm.new_string("hello world".into()).into()));
    test_interpret("return 'foo' .. 'bar' ~= 'foobar'", |_| Ok(false.into()));
}

#[test]
fn test_global_vars() {
    test_interpret("
        foo = 5 + 8
        return foo", |_| Ok(13.0.into()));
    test_interpret("
        local bar
        return bar", |_| Ok(().into()));
}

#[test]
fn test_native_functions() {
    test_interpret("return type(tostring(5))", |vm| Ok("string".into_rua(vm)));
    test_interpret("return type(tonumber('5'))", |vm| Ok("number".into_rua(vm)));
    test_interpret("return tostring(5) .. 'foo'", |vm| Ok("5foo".into_rua(vm)));
    test_interpret("return tonumber('110', 2)", |_| Ok(6.0.into()));
    test_interpret("assert(false, 'custom error')", |vm| Err(EvalError::AssertionFailed(Some("custom error".into_rua(vm)))));
    test_interpret("return pcall(print, 5, 'hello world')", |_| Ok(().into()));
    test_interpret("return pcall(assert, false)", |_| Ok(false.into()));
}

