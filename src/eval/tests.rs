#![cfg(test)]

use crate::{compiler::Compiler, lex::Tokenizer, eval::vals::{RuaVal, EvalError, IntoRuaVal}};

use super::Vm;

fn test_interpret<F: FnOnce(&mut Vm) -> Result<RuaVal, EvalError>>(input: &str, output: F){
    let mut vm = Vm::new();

    let mut tokens = Tokenizer::new(input.chars(), &mut vm);
    let compiler = Compiler::new(&mut tokens);
    let prog = compiler.compile().expect("Failed to compile program");

    let res = vm.interpret(prog);
    assert_eq!(res, output(&mut vm));
    // assert_eq!(vm.stack(), &Vec::new());
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
    test_interpret("return bar", |_| Ok(RuaVal::Nil));
    test_interpret("
        a = 1
        a = a + 1
        return a", |_| Ok(2.0.into()));
}

#[test]
fn test_local_vars() {
    test_interpret("
        local foo = 5 + 8
        local bar = 3
        local foo = foo + bar
        return foo
        ", |_| Ok(16.0.into()));
}

#[test]
fn test_native_functions() {
    test_interpret("return type(tostring(5))", |vm| Ok("string".into_rua(vm)));
    test_interpret("return type(tonumber('5'))", |vm| Ok("number".into_rua(vm)));
    test_interpret("return tostring(5) .. 'foo'", |vm| Ok("5foo".into_rua(vm)));
    test_interpret("return tonumber('110', 2)", |_| Ok(6.0.into()));
    test_interpret("assert(false, 'custom error')", |vm| Err(EvalError::AssertionFailed(Some("custom error".into_rua(vm)))));
    test_interpret("return pcall(print, 5, 'hello world')", |_| Ok(RuaVal::Nil));
    test_interpret("return pcall(assert, false)", |_| Ok(false.into()));
}

#[test]
fn test_if() {
   test_interpret("if 5>=5 then return 1 end", |_| Ok(1.0.into()));
   test_interpret("if 4>=5 then return 1 end", |_| Ok(RuaVal::Nil));
   test_interpret("if 4>=5 then return 1 end return true", |_| Ok(true.into()));
}

#[test]
fn test_if_else() {
   test_interpret("if 5<=4 then return 1 else return true end", |_| Ok(true.into()));
   test_interpret("if 4<=5 then return 1 else return true end", |_| Ok(1.0.into()));
}

#[test]
fn test_logic_operators() {
   test_interpret("return false and nil + nil", |_| Ok(false.into()));
   test_interpret("return true and 5", |_| Ok(5.0.into()));
   test_interpret("return 2 or nil + nil", |_| Ok(2.0.into()));
   test_interpret("return false or 'test'", |vm| Ok("test".into_rua(vm)));
}


#[test]
fn test_while() {
    test_interpret("local i = 0
        while i < 42 do
            i = i + 1
        end
        return i", |_| Ok(42.0.into()));
}
