#![cfg(test)]

use std::cell::RefCell;

use pretty_assertions::assert_eq;

use crate::eval::scope::Scope;
use crate::parser::parse_expression;
use crate::{parser::{parse, ast::{Precedence, ExpressionContext}}, lex::Tokenizer, identifiers::Trie};

use super::vals::{EvalError, LuaType};
use super::{vals::{StmtResult as R, TypeError, LuaVal as V}, eval};

fn test_eval_expr(input: &str, expected_output: Result<V, EvalError>) {
    let mut identifiers = Trie::new();
    let tokens = Tokenizer::new(input.chars(), &mut identifiers);
    let expr = parse_expression(tokens.peekable().by_ref(), &Precedence::Lowest, &ExpressionContext::Group)
        .expect(&("Invalid test input: ".to_owned() + input));
    let env = Scope::new(RefCell::new(identifiers).into());
    let res = expr.eval(RefCell::new(env).into());
    assert_eq!(res, expected_output);
    drop(expected_output);
}

fn test_eval_stmt(input: &str, expected_output: Result<R, EvalError>) {
    let mut identifiers = Trie::new();
    let tokens = Tokenizer::new(input.chars(), &mut identifiers);
    let prog = parse(tokens).expect(&("Invalid test input: ".to_owned() + input));
    let env = Scope::new(RefCell::new(identifiers).into());
    let res = eval(&prog, &RefCell::new(env).into());
    assert_eq!(res, expected_output);
    drop(expected_output);
}

#[test]
fn test_expressions() {
   test_eval_expr("1 + 1 - 5 * 2", Ok(V::Number(-8.0)));
   test_eval_expr("1 + (1 - 5) * 2", Ok(V::Number(-7.0)));
   test_eval_expr("true and 5<2", Ok(V::Bool(false)));
   test_eval_expr("5<=2 or 3-5+2 > -4", Ok(V::Bool(true)));
   test_eval_expr("5 or 1", Ok(V::Number(5.0)));
   test_eval_expr("nil or 1", Ok(V::Number(1.0)));
   test_eval_expr("5 and 1", Ok(V::Number(1.0)));
   test_eval_expr("nil and 1", Ok(V::Nil));
}

#[test]
fn test_if_else() {
   test_eval_stmt("if 5>=5 then return 1 else return nil end", Ok(R::Return(V::Number(1.0))));
   test_eval_stmt("if false then return 1 else return nil end", Ok(R::Return(V::Nil)));
}

#[test]
fn test_type_errors() {
    test_eval_expr("1 + true * nil", Err(TypeError(LuaType::Number, LuaType::Bool).into()));
    test_eval_expr("-false or -true", Err(TypeError(LuaType::Number, LuaType::Bool).into()));
    test_eval_expr("(false or 5) + nil", Err(TypeError(LuaType::Number, LuaType::Nil).into()));
}

#[test]
fn test_local_stmt() {
    test_eval_stmt("local foo = 5; return foo", Ok(R::Return(V::Number(5.0))));
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
        return i", Ok(R::Return(V::Number(42.0))));
    test_eval_stmt("local i = 0
        while i < 42 do
            i = i + 1
            if i > 13 then break end
        end
        return i", Ok(R::Return(V::Number(14.0))));
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

        return fib", Ok(R::Return(V::Number(13.0))));
}


#[test]
fn test_call_expr() {
    test_eval_stmt("local function foo()
        return 1337
    end
    return foo()", Ok(R::Return(V::Number(1337.0))));
    test_eval_stmt("
    local DEFAULT = 1
    local function fact(n)
        if n < 2 then return DEFAULT end
        return n * fact(n-1)
    end
    return fact(5)", Ok(R::Return(V::Number(120.0))));
    test_eval_stmt("
    local add = function(x)
        return function(y)
            return x + y
        end
    end
    local add2 = add(2)
    return add2(5)", Ok(R::Return(V::Number(7.0))));
    test_eval_stmt("
    function apply(fn)
        return fn(4)
    end
    return apply(function(n) return n*n end)", Ok(R::Return(V::Number(16.0))));
}

