#![cfg(test)]

use crate::compiler::compile;

use super::{Vm, vals::{RuaVal, EvalError, IntoRuaVal, EvalErrorTraced, RuaType}};

use pretty_assertions::assert_eq;

fn test_interpret<F: FnOnce(&mut Vm) -> Result<RuaVal, EvalErrorTraced>>(input: &str, output: F){
    let mut vm = Vm::new();

    let prog = compile(input.chars(), &mut vm).expect("Failed to compile program");

    let res = vm.interpret(prog);
    let expected = output(&mut vm);
    assert_eq!(res, expected);
    assert_eq!(vm.stack(), &Vec::new());
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
    test_interpret("return #'foo' + 1", |_| Ok(4.0.into()));
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
    test_interpret("assert(false, 'custom error')", |vm| Err(EvalErrorTraced::new(
        EvalError::AssertionFailed(Some("custom error".into_rua(vm))),
        vec![("assert".into(), 0), ("<anonymous>".into(), 0)]
    )));
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

#[test]
fn test_functions() {
    test_interpret("
    local function foo()
        return 1337
    end
    return foo()", |_| Ok(1337.0.into()));
    test_interpret("
    local n = 5
    function add(n, m)
        return n + m
    end
    return add(1, 2)", |_| Ok(3.0.into()));
}

#[test]
fn test_recursion() {
    test_interpret("
    local function fact(n)
        if n < 2 then return 1 end
        return n * fact(n-1)
    end
    return fact(5)", |_| Ok(120.0.into()));
    test_interpret("
    function global_fact(n)
        if n < 2 then return 1 end
        return n * global_fact(n-1)
    end
    return global_fact(5)", |_| Ok(120.0.into()));
}

#[test]
fn test_stack_trace() {
    test_interpret("
    function bar()
        return 1+nil
    end
    function foo()
        bar()
    end
    foo()", |_| Err(EvalErrorTraced::new(
        EvalError::TypeError { expected: RuaType::Number, got: RuaType::Nil },
        vec![("bar".into(), 2), ("foo".into(), 5), ("<anonymous>".into(), 7)]
    )));

    test_interpret("function foo()
        assert(false, 'custom error')
    end
    foo()", |vm| Err(EvalErrorTraced::new(
        EvalError::AssertionFailed(Some("custom error".into_rua(vm))),
        vec![("assert".into(), 0), ("foo".into(), 1), ("<anonymous>".into(), 3)]
    )));
}

#[test]
fn test_native_non_native_nested() {
    test_interpret("
    function foo()
        local function bar()
            local function baz()
                return 7
            end
            return baz()
        end
        return pcall(bar)
    end
    return foo()", |_| Ok(7.0.into()));
}


#[test]
fn test_table_index() {
    test_interpret("
        local a = {1, b = true, 2, [2+3] = false}
        return a[1]",
        |_| Ok(1.0.into()));
    test_interpret("
        local a = {1, b = true, 3, [2+3] = false}
        return a[2]",
        |_| Ok(3.0.into()));
    test_interpret("
        local a = {1, b = true, 2, [2+3] = false}
        return a['b']",
        |_| Ok(true.into()));
    test_interpret("
        local a = {1, b = true, 2, [2+3] = false}
        return a[5]",
        |_| Ok(false.into()));
}

#[test]
fn test_table_field_access() {
    test_interpret("
        local a = {1, b = true, 2, [2+3] = false}
        return a.b",
        |_| Ok(true.into()));
    test_interpret("
        local a = {1, b = true, 2, [2+3] = false}
        return a.c",
        |_| Ok(RuaVal::Nil));
    test_interpret("
        local a = {1, b = true, 2, [2+3] = false}
        return a[false]",
        |_| Ok(RuaVal::Nil));
}

#[test]
fn test_functions_with_different_nargs() {
    test_interpret("
    function foo(a, b, c)
        return c
    end
    return foo()", |_| Ok(RuaVal::Nil));
    test_interpret("
    function foo(arg)
        return arg
    end
    return foo(1, 2)", |_| Ok(1.0.into()));
}


