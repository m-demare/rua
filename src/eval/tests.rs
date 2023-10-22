#![cfg(test)]

use std::cell::RefCell;

use pretty_assertions::assert_eq;

use crate::eval::{scope::Scope, isolate::Isolate};
use crate::parser::parse_expression;
use rua_identifiers::Trie;
use crate::{parser::{parse, ast::Precedence}, lex::Tokenizer};

use super::vals::{EvalError, RuaType};
use super::{vals::{StmtResult as R, TypeError, RuaVal as V}, eval};

fn test_eval_expr(input: &str, expected_output: Result<V, EvalError>) {
    let mut identifiers = Trie::new();
    let tokens = Tokenizer::new(input.chars(), &mut identifiers);
    let expr = parse_expression(tokens.peekable().by_ref(), &Precedence::Lowest)
        .expect(&("Invalid test input: ".to_owned() + input));
    let isolate = Isolate::new(identifiers);
    let root_scope = Scope::new(RefCell::new(isolate).into());
    let res = expr.eval(RefCell::new(root_scope).into());
    assert_eq!(res, expected_output);
    drop(expected_output);
}

fn test_eval_stmt(input: &str, expected_output: Result<R, EvalError>) {
    let mut identifiers = Trie::new();
    let tokens = Tokenizer::new(input.chars(), &mut identifiers);
    let prog = parse(tokens).expect(&("Invalid test input: ".to_owned() + input));
    let isolate = Isolate::new(identifiers);
    let res = eval(&prog, RefCell::new(isolate).into());
    assert_eq!(res, expected_output);
    drop(expected_output);
}

#[test]
fn test_expressions() {
   test_eval_expr("1 + 1 - 5 * 2", Ok(V::Number((-8.0).into())));
   test_eval_expr("1 + (1 - 5) * 2", Ok(V::Number((-7.0).into())));
   test_eval_expr("true and 5<2", Ok(V::Bool(false)));
   test_eval_expr("5<=2 or 3-5+2 > -4", Ok(V::Bool(true)));
   test_eval_expr("5 or 1", Ok(V::Number(5.0.into())));
   test_eval_expr("nil or 1", Ok(V::Number(1.0.into())));
   test_eval_expr("5 and 1", Ok(V::Number(1.0.into())));
   test_eval_expr("nil and 1", Ok(V::Nil));
}

#[test]
fn test_if_else() {
   test_eval_stmt("if 5>=5 then return 1 else return nil end", Ok(R::Return(V::Number(1.0.into()))));
   test_eval_stmt("if false then return 1 else return nil end", Ok(R::Return(V::Nil)));
}

#[test]
fn test_type_errors() {
    test_eval_expr("1 + true * nil", Err(TypeError(RuaType::Number, RuaType::Bool).into()));
    test_eval_expr("-false or -true", Err(TypeError(RuaType::Number, RuaType::Bool).into()));
    test_eval_expr("(false or 5) + nil", Err(TypeError(RuaType::Number, RuaType::Nil).into()));
}

#[test]
fn test_local_stmt() {
    test_eval_stmt("local foo = 5; return foo", Ok(R::Return(V::Number(5.0.into()))));
}

#[test]
fn test_unknown_id() {
    test_eval_stmt("return foo", Err(EvalError::UnknownId("foo".into())));
}

#[test]
fn test_while_stmt() {
    test_eval_stmt("local i = 0
        while i < 42 do
            i = i + 1
        end
        return i", Ok(R::Return(V::Number(42.0.into()))));
    test_eval_stmt("local i = 0
        while i < 42 do
            i = i + 1
            if i > 13 then break end
        end
        return i", Ok(R::Return(V::Number(14.0.into()))));
    test_eval_stmt("
        local n = 7
        local i = 3
        local fib = 1
        local aux1 = 1
        local aux2
        while i <= n do
            aux2 = fib
            fib = aux1 + fib
            aux1 = aux2
            i = i + 1
        end

        return fib", Ok(R::Return(V::Number(13.0.into()))));
}

#[test]
fn test_call_expr() {
    test_eval_stmt("local function foo()
        return 1337
    end
    return foo()", Ok(R::Return(V::Number(1337.0.into()))));
    test_eval_stmt("
    local DEFAULT = 1
    local function fact(n)
        if n < 2 then return DEFAULT end
        return n * fact(n-1)
    end
    return fact(5)", Ok(R::Return(V::Number(120.0.into()))));
    test_eval_stmt("
    local add = function(x)
        return function(y)
            return x + y
        end
    end
    local add2 = add(2)
    return add2(5)", Ok(R::Return(V::Number(7.0.into()))));
    test_eval_stmt("
    function apply(fn)
        return fn(4)
    end
    return apply(function(n) return n*n end)", Ok(R::Return(V::Number(16.0.into()))));
}

#[test]
fn test_strings() {
    test_eval_expr(r#""foo" .. "bar""#, Ok(V::String("foobar".into())));
    test_eval_expr(r#" #("foo" .. "bar") "#, Ok(V::Number(6.0.into())));
}

#[test]
fn test_assign_stmt() {
    test_eval_stmt("
    local function foo()
        local n = 2
        local m = 10
        local function asdf()
            local n
            n = 1
            return n
        end
        return n + m + asdf()
    end
    return foo()", Ok(R::Return(V::Number(13.0.into()))));
    test_eval_stmt("
    local function foo()
        local function asdf()
            global = 123
        end
        asdf()
    end
    foo()
    return global", Ok(R::Return(V::Number(123.0.into()))));
}

#[test]
fn test_native_functions() {
    test_eval_stmt("return type(tostring(5))", Ok(R::Return(V::String("string".into()))));
    test_eval_stmt("return type(tonumber('5'))", Ok(R::Return(V::String("number".into()))));
    test_eval_stmt("return tostring(5) .. 'foo'", Ok(R::Return(V::String("5foo".into()))));
    test_eval_stmt("return tonumber('110', 2)", Ok(R::Return(V::Number(6.0.into()))));
    test_eval_stmt("assert(false, 'custom error')", Err(EvalError::AssertionFailed(Some("custom error".into()))));
    test_eval_stmt("return pcall(print, 5, 'hello world')", Ok(R::Return(V::Nil)));
    test_eval_stmt("return pcall(assert, false)", Ok(R::Return(V::Bool(false))));
}

#[test]
fn test_table_field_access() {
    test_eval_stmt("
        local a = {1, b = true, 2, [2+3] = false}
        return a[1]",
        Ok(R::Return(V::Number(1.0.into()))));
    test_eval_stmt("
        local a = {1, b = true, 3, [2+3] = false}
        return a[2]",
        Ok(R::Return(V::Number(3.0.into()))));
    test_eval_stmt("
        local a = {1, b = true, 2, [2+3] = false}
        return a['b']",
        Ok(R::Return(V::Bool(true))));
    test_eval_stmt("
        local a = {1, b = true, 2, [2+3] = false}
        return a[5]",
        Ok(R::Return(V::Bool(false))));
    test_eval_stmt("
        local a = {1, b = true, 2, [2+3] = false}
        return a.b",
        Ok(R::Return(V::Bool(true))));
    test_eval_stmt("
        local a = {1, b = true, 2, [2+3] = false}
        return a.c",
        Ok(R::Return(V::Nil)));
    test_eval_stmt("
        local a = {1, b = true, 2, [2+3] = false}
        return a[false]",
        Ok(R::Return(V::Nil)));
}
