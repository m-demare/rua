#![cfg(test)]

use crate::compiler::compile;

use super::{
    vals::{EvalError, EvalErrorTraced, IntoRuaVal, RuaType, RuaVal},
    Vm,
};

use pretty_assertions::assert_eq;

fn test_interpret_aux<F: FnOnce(&mut Vm) -> Result<RuaVal, EvalErrorTraced>>(
    input: &str,
    output: F,
) {
    let mut vm = Vm::new();

    let prog = compile(input.chars(), &mut vm).expect("Failed to compile program");

    let res = vm.interpret(prog.into());
    let expected = output(&mut vm);
    assert_eq!(res, expected);
    assert_eq!(vm.stack(), &Vec::new());
}

macro_rules! test_interpret {
    ($name: ident, $input: expr, $output: expr) => {
        #[test]
        fn $name() {
            test_interpret_aux($input, $output);
        }
    };
}

test_interpret!(arithmetic_ops1, "return (5 + -2) * 4", |_| Ok(12.0.into()));
test_interpret!(arithmetic_ops2, "return - -2 / 4 + 1", |_| Ok(1.5.into()));
test_interpret!(arithmetic_ops3, "return 2+5 > 6", |_| Ok(true.into()));
test_interpret!(arithmetic_ops4, "return 2+6 % 4", |_| Ok(4.0.into()));

test_interpret!(string_concat, "return 'hello' .. ' ' .. 'world'", |vm| Ok(vm
    .new_string("hello world".into())
    .into()));
test_interpret!(string_equality, "return 'foo' .. 'bar' ~= 'foobar'", |_| Ok(false.into()));
test_interpret!(string_length, "return #'foo' + 1", |_| Ok(4.0.into()));

test_interpret!(
    global_vars,
    "
    foo = 5 + 8
    return foo",
    |_| Ok(13.0.into())
);
test_interpret!(undefined_global, "return bar", |_| Ok(RuaVal::Nil));
test_interpret!(
    global_mutation,
    "
    a = 1
    a = a + 1
    return a",
    |_| Ok(2.0.into())
);

test_interpret!(
    local_vars,
    "
    local foo = 5 + 8
    local bar = 3
    local foo = foo + bar
    return foo
    ",
    |_| Ok(16.0.into())
);

test_interpret!(nativefn_tostring, "return type(tostring(5))", |vm| Ok("string".into_rua(vm)));
test_interpret!(nativefn_tonumber, "return type(tonumber('5'))", |vm| Ok("number".into_rua(vm)));
test_interpret!(nativefn_tostring2, "return tostring(5) .. 'foo'", |vm| Ok("5foo".into_rua(vm)));
test_interpret!(nativefn_tonumbre_binary, "return tonumber('110', 2)", |_| Ok(6.0.into()));
test_interpret!(nativefn_assert, "assert(false, 'custom error')", |vm| Err(EvalErrorTraced::new(
    EvalError::AssertionFailed(Some("custom error".into_rua(vm))),
    vec![("assert".into(), 0), ("<main>".into(), 0)]
)));
test_interpret!(nativefn_pcall_ok, "return pcall(print, 5, 'hello world')", |_| Ok(RuaVal::Nil));
test_interpret!(nativefn_pcall_err, "return pcall(assert, false)", |_| Ok(false.into()));

test_interpret!(if_true, "if 5>=5 then return 1 end", |_| Ok(1.0.into()));
test_interpret!(if_false, "if 4>=5 then return 1 end", |_| Ok(RuaVal::Nil));
test_interpret!(if_false2, "if 4>=5 then return 1 end return true", |_| Ok(true.into()));

test_interpret!(if_else_false, "if 5<=4 then return 1 else return true end", |_| Ok(true.into()));
test_interpret!(if_else_true, "if 4<=5 then return 1 else return true end", |_| Ok(1.0.into()));

test_interpret!(and, "return true and 5", |_| Ok(5.0.into()));
test_interpret!(and_shortciruit, "return false and nil + nil", |_| Ok(false.into()));
test_interpret!(or, "return false or 'test'", |vm| Ok("test".into_rua(vm)));
test_interpret!(or_shortciruit, "return 2 or nil + nil", |_| Ok(2.0.into()));

test_interpret!(
    while_statement,
    "local i = 0
    while i < 42 do
        i = i + 1
    end
    return i",
    |_| Ok(42.0.into())
);

test_interpret!(
    function,
    "
local function foo()
    return 1337
end
return foo()",
    |_| Ok(1337.0.into())
);
test_interpret!(
    function_shadows_outer_var,
    "
local n = 5
function add(n, m)
    return n + m
end
return add(1, 2)",
    |_| Ok(3.0.into())
);

test_interpret!(
    recursion,
    "
local function fact(n)
    if n < 2 then return 1 end
    return n * fact(n-1)
end
return fact(5)",
    |_| Ok(120.0.into())
);
test_interpret!(
    recursion2,
    "
function global_fact(n)
    if n < 2 then return 1 end
    return n * global_fact(n-1)
end
return global_fact(5)",
    |_| Ok(120.0.into())
);

test_interpret!(
    stack_trace1,
    "
function bar()
    return 1+nil
end
function foo()
    bar()
end
foo()",
    |_| Err(EvalErrorTraced::new(
        EvalError::TypeError { expected: RuaType::Number, got: RuaType::Nil },
        vec![("bar".into(), 2), ("foo".into(), 5), ("<main>".into(), 7)]
    ))
);
test_interpret!(
    stack_trace2,
    "function foo()
    assert(false, 'custom error')
end
foo()",
    |vm| Err(EvalErrorTraced::new(
        EvalError::AssertionFailed(Some("custom error".into_rua(vm))),
        vec![("assert".into(), 0), ("foo".into(), 1), ("<main>".into(), 3)]
    ))
);

test_interpret!(
    native_non_native_nested_call,
    "
function foo()
    local function bar()
        local function baz()
            return 7
        end
        return baz()
    end
    return pcall(bar)
end
return foo()",
    |_| Ok(7.0.into())
);

test_interpret!(
    table_index_num1,
    "
    local a = {1, b = true, 2, [2+3] = false}
    return a[1]",
    |_| Ok(1.0.into())
);
test_interpret!(
    table_index_num2,
    "
    local a = {1, b = true, 3, [2+3] = false}
    return a[2]",
    |_| Ok(3.0.into())
);
test_interpret!(
    table_index_char,
    "
    local a = {1, b = true, 2, [2+3] = false}
    return a['b']",
    |_| Ok(true.into())
);
test_interpret!(
    table_index_num3,
    "
    local a = {1, b = true, 2, [2+3] = false}
    return a[5]",
    |_| Ok(false.into())
);
test_interpret!(
    table_index_bool,
    "
    local a = {1, b = true, 2, [2+3] = false}
    return a[false]",
    |_| Ok(RuaVal::Nil)
);

test_interpret!(
    field_access1,
    "
    local a = {1, b = true, 2, [2+3] = false}
    return a.b",
    |_| Ok(true.into())
);
test_interpret!(
    field_access2,
    "
    local a = {1, b = true, 2, [2+3] = false}
    return a.c",
    |_| Ok(RuaVal::Nil)
);

test_interpret!(
    table_with_local_key,
    "
    local b = 'test'
    local a = {1, b = true, 2, [2+3] = false}
    return a.b",
    |_| Ok(RuaVal::Bool(true))
);

test_interpret!(
    table_with_upvalue_key,
    "
    local b = 'test'
    function foo()
        local a = {1, b = true, 2, [2+3] = false}
        return a.b
    end
    return foo()",
    |_| Ok(RuaVal::Bool(true))
);

test_interpret!(
    call_with_less_args,
    "
function foo(a, b, c)
    return c
end
return foo()",
    |_| Ok(RuaVal::Nil)
);
test_interpret!(
    call_with_more_args,
    "
function foo(arg)
    return arg
end
return foo(1, 2)",
    |_| Ok(1.0.into())
);

test_interpret!(
    closure_read_local,
    "
local val = 1337
local function foo()
    return val
end
return foo()",
    |_| Ok(1337.0.into())
);
test_interpret!(
    closure_write_local,
    "
local val = 1337
local function foo()
    val = 123
end
foo()
return val",
    |_| Ok(123.0.into())
);

test_interpret!(
    more_closures1,
    "
local function foo(n)
    local function bar(m)
        n = n + 1
        return n + m
    end
    return bar
end
local f = foo(3)
return f(5) + f(2)",
    |_| Ok(16.0.into())
);
test_interpret!(
    more_closures2,
    "
local function sum(n)
    return function(m)
        return n + m
    end
end
local sum41 = sum(41)
return sum41(1)",
    |_| Ok(42.0.into())
);

test_interpret!(
    many_closures_reference_same_local,
    "
local function foo()
    local val = 3
    local function bar()
        return val
    end
    local function baz()
        val = val + 2
        return val
    end
    local ret = bar()
    ret = ret + baz()
    ret = ret * bar()
    return ret
end
return foo()",
    |_| Ok(40.0.into())
);

test_interpret!(
    closure_that_outlive_variable_from_block,
    "
local function foo()
    local func
    if true then
        local val = 1337
        local function bar()
            return val
        end
        func = bar
    end
    return func()
end
return foo()",
    |_| Ok(1337.0.into())
);

test_interpret!(
    closure_that_outlive_variable_from_parent_function,
    "
local function foo()
    local val = 1337
    local function bar()
        return val
    end
    return bar
end
local b = foo()
return b()",
    |_| Ok(1337.0.into())
);

test_interpret!(
    closures_share_closed_over_variable,
    "
local function foo()
    local val = 1
    local function create_set_and_get()
        local function set(v)
            val = v
        end
        local function get()
            return val
        end
        return {set, get}
    end
    return create_set_and_get()
end
local t = foo()
set = t[1]
get = t[2]
local res = get()
set(5)
res = res + get()
return res",
    |_| Ok(6.0.into())
);

test_interpret!(
    table_field_assignment1,
    "
    local a = {}
    a[3 + 2] = 8
    return a[5]
",
    |_| Ok(8.0.into())
);

test_interpret!(
    table_field_assignment2,
    "
    local a = {}
    a.b = 9
    return a['b']
",
    |_| Ok(9.0.into())
);
