#![cfg(test)]

use pretty_assertions::assert_eq;

use crate::lex::{Tokenizer, tokens::TokenType};
use super::{ast::{Statement as S, Expression as E, FunctionArg as FA, Program}, parse};
use crate::identifiers::Trie;

macro_rules! b {
    ($e: expr) => {
        {Box::new($e)}
    };
}

macro_rules! n {
    ($e: expr) => {
        {(E::NumberLiteral($e).into())}
    };
}

macro_rules! test_parse {
    ($input: expr, $expected_output: expr) => {
        let mut identifiers = Trie::new();
        #[allow(unused_macros)]
        macro_rules! id {
            ($s: expr) => (match identifiers.find($s) { Some(TokenType::IDENTIFIER(i)) => i, t => panic!("Expected identifier, got {t:?}") })
        }

        let prog = parse(Tokenizer::new($input.chars(), &mut identifiers));
        assert_eq!(prog, $expected_output)
    };
}

#[test]
fn parse_local_statement() {
    test_parse!("
        local bar
        ", Ok(Program{
        statements: [
            S::Local(id!("bar"), None),
        ].into()
    }));
}

#[test]
fn parse_identifier_exp() {
    test_parse!("
        local foo = bar
        ", Ok(Program{
        statements: [
            S::Local(id!("foo"), Some(b!(E::Identifier(id!("bar"))))),
        ].into()
    }));
}


#[test]
fn parse_number_exp() {
    test_parse!("
        local baz = 5", Ok(Program{
        statements: [
            S::Local(id!("baz"), Some(b!(E::NumberLiteral(5.0)))),
        ].into()
    }));
}

#[test]
fn parse_return_statement() {
    test_parse!("return 5", Ok(Program{
        statements: [
            S::Return(Some(b!(E::NumberLiteral(5.0)))),
        ].into()
    }));
        
    test_parse!("return;", Ok(Program{
        statements: [
            S::Return(None),
        ].into()
    }));
    test_parse!("return asdf", Ok(Program{
        statements: [
            S::Return(Some(b!(E::Identifier(id!("asdf"))))),
        ].into()
    }));
    test_parse!("return", Ok(Program{
        statements: [
            S::Return(None),
        ].into()
    }));
}

#[test]
fn parse_prefix_exprs() {
    test_parse!("
        local foo = not asd
        local bar = #foo
", Ok(Program{
        statements: [
            S::Local(id!("foo"), Some(b!(E::Not(b!(E::Identifier(id!("asd"))))))),
            S::Local(id!("bar"), Some(b!(E::Len(b!(E::Identifier(id!("foo"))))))),
        ].into()
    }));
}

#[test]
fn parse_infix_exprs() {
    test_parse!("
        local foo = 5 + 7
        local bar = 5 * 7
        local baz = 5 <= 7
", Ok(Program{
        statements: [
            S::Local(id!("foo"), Some(b!(E::Plus(b!((n!(5.0), n!(7.0))))))),
            S::Local(id!("bar"), Some(b!(E::Times(b!((n!(5.0), n!(7.0))))))),
            S::Local(id!("baz"), Some(b!(E::Le(b!((n!(5.0), n!(7.0))))))),
        ].into()
    }));
}

#[test]
fn parse_infix_combined_exprs() {
    test_parse!("
        local foo = 5 + 7 * 2
        local bar = 5 * 7 + 2
        local baz = 5 <= 7 and 0 + 2^3 ==8
", Ok(Program{
        statements: [
            S::Local(id!("foo"),
                Some(b!(E::Plus(b!((n!(5.0),
                    E::Times(b!((n!(7.0), n!(2.0)))))))))),
            S::Local(id!("bar"),
                Some(b!(E::Plus(b!((E::Times(b!((n!(5.0), n!(7.0)))),
                    n!(2.0))))))),
            S::Local(id!("baz"),
                Some(b!(E::And(b!((
                    E::Le(b!((n!(5.0), n!(7.0)))),
                    E::Eq(b!((E::Plus(b!((n!(0.0), E::Exp(b!((n!(2.0), n!(3.0))))))), n!(8.0)))))))))),
        ].into()
    }));
}

#[test]
fn parse_minus_exprs() {
    test_parse!("
        local foo = -5 + 7 * -2
        local bar = -5 - -7 - 2
        local baz = -2^2
", Ok(Program{
        statements: [
            S::Local(id!("foo"),
                Some(b!(E::Plus(b!((E::Neg(n!(5.0)),
                    E::Times(b!((n!(7.0), E::Neg(n!(2.0))))))))))),
            S::Local(id!("bar"),
                Some(b!(E::Minus(b!((E::Minus(b!((E::Neg(n!(5.0)), E::Neg(n!(7.0))))),
                    n!(2.0))))))),
            S::Local(id!("baz"),
                    Some(b!(E::Neg(b!(E::Exp(b!((n!(2.0), n!(2.0))))))))),
        ].into()
    }));
}

#[test]
fn parse_boolean_exprs() {
    test_parse!("
        local foo = true
        local bar = not false
", Ok(Program{
        statements: [
            S::Local(id!("foo"),
                Some(b!(E::BooleanLiteral(true)))),
            S::Local(id!("bar"),
                Some(b!(E::Not(b!(E::BooleanLiteral(false)))))),
        ].into()
    }));
}

#[test]
fn parse_parentheses_exprs() {
    test_parse!("
        local foo = (5 + 7) * 2
        local bar = not ((true and false) or not true)
", Ok(Program{
        statements: [
            S::Local(id!("foo"), Some(b!(E::Times(b!((E::Plus(b!((n!(5.0), n!(7.0)))), n!(2.0))))))),
            S::Local(id!("bar"), Some(b!(E::Not(b!(E::Or(b!((
                E::And(b!((E::BooleanLiteral(true), E::BooleanLiteral(false)))),
                E::Not(b!(E::BooleanLiteral(true))))))))))),
        ].into()
    }));
}

#[test]
fn parse_if_statement() {
    test_parse!("
        if true then
            local a = false
        end
        if false then
            local a = false
            return true
        else
            return false
        end
        if false then
            local a = false
            return true
        elseif true then
        elseif 5 > 2 then
            return false
        end
", Ok(Program{
        statements: [
            S::IfThen(b!(E::BooleanLiteral(true)), [S::Local(id!("a"), Some(b!(E::BooleanLiteral(false))))].into()),
            S::IfThenElse(b!(E::BooleanLiteral(false)),
                [
                    S::Local(id!("a"), Some(b!(E::BooleanLiteral(false)))),
                    S::Return(Some(b!(E::BooleanLiteral(true))))].into(),
                [S::Return(Some(b!(E::BooleanLiteral(false))))].into()),
            S::IfThenElseIf(b!([
                (E::BooleanLiteral(false), [
                    S::Local(id!("a"), Some(b!(E::BooleanLiteral(false)))),
                    S::Return(Some(b!(E::BooleanLiteral(true))))].into()),
                (E::BooleanLiteral(true), [].into()),
                (E::Gt(b!((n!(5.0), n!(2.0)))), [S::Return(Some(b!(E::BooleanLiteral(false))))].into())])),
            ].into()
        }));
}

#[test]
fn parse_func_exp() {
    test_parse!("
        local a = function()
            local b
            return true
        end
        local c = function(foo, bar)
        end
        local d = function(foo, ...)
        end
", Ok(Program{
        statements: [
                S::Local(id!("a"), Some(b!(E::Function([].into(), [
                    S::Local(id!("b"), None),
                    S::Return(Some(b!(E::BooleanLiteral(true)))),
                ].into())))),
                S::Local(id!("c"), Some(b!(E::Function([FA::Identifier(id!("foo")), FA::Identifier(id!("bar"))].into(), [].into())))),
                S::Local(id!("d"), Some(b!(E::Function([FA::Identifier(id!("foo")), FA::Dotdotdot].into(), [].into())))),
            ].into()
        }));
}

#[test]
fn parse_func_statement() {
    test_parse!("
        function a()
            local b
            return true
        end
        local function c(foo, bar)
        end
", Ok(Program{
        statements: [
                S::Assign(id!("a"), b!(E::Function([].into(), [
                    S::Local(id!("b"), None),
                    S::Return(Some(b!(E::BooleanLiteral(true)))),
                ].into()))),
                S::Local(id!("c"), Some(b!(E::Function([FA::Identifier(id!("foo")), FA::Identifier(id!("bar"))].into(), [].into())))),
            ].into()
        }));
}

#[test]
fn parse_assignment_statement() {
    test_parse!("
        a = true or false
        b = 5 + 3 * 8
", Ok(Program{
        statements: [
                S::Assign(id!("a"), b!(E::Or(b!((E::BooleanLiteral(true), E::BooleanLiteral(false)))))),
                S::Assign(id!("b"), b!(E::Plus(b!((n!(5.0), E::Times(b!((n!(3.0), n!(8.0))))))))),
            ].into()
        }));
}

#[test]
fn parse_call_statement() {
    test_parse!("
        foo()
        bar(1, 2+3)
        a(true)(false);
        (1+2)(false)
        a.b(true)
", Ok(Program{
        statements: [
                S::Call(b!(E::Identifier(id!("foo"))), [].into()),
                S::Call(b!(E::Identifier(id!("bar"))), [E::NumberLiteral(1.0), E::Plus(b!((n!(2.0), n!(3.0))))].into()),
                S::Call(b!(E::Call(b!(E::Identifier(id!("a"))), [E::BooleanLiteral(true)].into())),
                    [E::BooleanLiteral(false)].into()),
                S::Call(b!(E::Plus(b!((n!(1.0), n!(2.0))))), [E::BooleanLiteral(false)].into()),
                S::Call(b!(E::FieldAccess(b!(E::Identifier(id!("a"))), id!("b"))), [E::BooleanLiteral(true)].into()),
            ].into()
        }));
}

#[test]
fn parse_call_exp() {
    test_parse!("
        local a = foo(true)
        return 1 + foo(true)(false)
", Ok(Program{
        statements: [
                S::Local(id!("a"), Some(b!(E::Call(b!(E::Identifier(id!("foo"))), [E::BooleanLiteral(true)].into())))),
                S::Return(Some(b!(E::Plus(b!((n!(1.0), E::Call(b!(E::Call(b!(E::Identifier(id!("foo"))), [E::BooleanLiteral(true)].into())),
                [E::BooleanLiteral(false)].into()))))))),
            ].into()
        }));
}

#[test]
fn parse_while_statement() {
    test_parse!("
        while 1 < 5 do
            i = i + 1
        end
", Ok(Program{
        statements: [
                S::While(b!(E::Lt(b!((n!(1.0), n!(5.0))))), [S::Assign(id!("i"), b!(E::Plus(b!((E::Identifier(id!("i")), n!(1.0))))))].into()),
            ].into()
        }));
}

#[test]
fn parse_field_access_exp() {
    test_parse!("
        local a = foo.bar.baz()
", Ok(Program{
        statements: [
                S::Local(id!("a"), Some(b!(E::Call(b!(E::FieldAccess(b!(E::FieldAccess(b!(E::Identifier(id!("foo"))), id!("bar"))), id!("baz"))), [].into())))),
            ].into()
        }));
}
