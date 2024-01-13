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

    let prog = compile(input.bytes(), &mut vm).expect("Failed to compile program");

    let res = vm.interpret(prog.into());
    let expected = output(&mut vm);
    assert_eq!(res, expected);
    assert_eq!(vm.stack(), &Vec::new());
}

macro_rules! test_interpret {
    ($name: ident, $input: expr, $output: expr) => {
        #[test]
        #[inline(never)]
        fn $name() {
            println!("Program:\n{}", $input);
            test_interpret_aux($input, $output);
        }
    };
}

test_interpret!(arithmetic_ops1, "return (5 + -2) * 4", |_| Ok(12.0.into()));
test_interpret!(arithmetic_ops2, "return - -2 / 4 + 1", |_| Ok(1.5.into()));
test_interpret!(arithmetic_ops3, "return 2+6 % 4", |_| Ok(4.0.into()));
test_interpret!(arithmetic_ops4, "return 2 - 1 + 3", |_| Ok(4.0.into()));
test_interpret!(arithmetic_ops5, "return 4 / 2 * 3", |_| Ok(6.0.into()));

test_interpret!(comparison_ops1, "return 2+5 > 6", |_| Ok(true.into()));
test_interpret!(comparison_ops2, "return 2+5 < 6", |_| Ok(false.into()));
test_interpret!(comparison_ops3, "return 2+5 <= 6+1", |_| Ok(true.into()));
test_interpret!(comparison_ops4, "return 2+5 >= 6+1", |_| Ok(true.into()));

test_interpret!(comparison_ops5, "a = 7; return a > 6", |_| Ok(true.into()));
test_interpret!(comparison_ops6, "a = 7; return a < 6", |_| Ok(false.into()));
test_interpret!(comparison_ops7, "a = 7; return a <= 6+1", |_| Ok(true.into()));
test_interpret!(comparison_ops8, "a = 7; return a >= 6+1", |_| Ok(true.into()));

test_interpret!(comparison_ops9, "a = 7; b = 6; return a > b", |_| Ok(true.into()));
test_interpret!(comparison_ops10, "a = 7; b = 6; return a < b", |_| Ok(false.into()));
test_interpret!(comparison_ops11, "a = 7; b = 6; return a <= b+1", |_| Ok(true.into()));
test_interpret!(comparison_ops12, "a = 7; b = 6; return a >= b+1", |_| Ok(true.into()));

test_interpret!(string_concat, "return 'hello' .. ' ' .. 'world'", |vm| Ok(vm
    .new_string((*b"hello world").into())
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
test_interpret!(undefined_global, "return bar", |_| Ok(RuaVal::nil()));
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
    local foo, bar = 5 + 8, 3
    local foo = foo + bar
    return foo
    ",
    |_| Ok(16.0.into())
);

test_interpret!(
    modifying_used_local,
    "
    local a = 5
    a = a + a + a
    return a",
    |_| Ok(15.0.into())
);

test_interpret!(nativefn_tostring, "return type(tostring(5))", |vm| Ok((*b"string").into_rua(vm)));
test_interpret!(
    nativefn_tonumber,
    "return type(tonumber('5'))",
    |vm| Ok((*b"number").into_rua(vm))
);
test_interpret!(
    nativefn_tostring2,
    "return tostring(5) .. 'foo'",
    |vm| Ok((*b"5foo").into_rua(vm))
);
test_interpret!(nativefn_tonumbre_binary, "return tonumber('110', 2)", |_| Ok(6.0.into()));
test_interpret!(nativefn_assert, "assert(false, 'custom error')", |vm| Err(EvalErrorTraced::new(
    EvalError::AssertionFailed(Some((*b"custom error").into_rua(vm))),
    vec![("assert".into(), 0), ("<main>".into(), 1)]
)));
test_interpret!(nativefn_pcall_ok, "return pcall(print, 5, 'hello world')", |_| Ok(RuaVal::nil()));
test_interpret!(nativefn_pcall_err, "return pcall(assert, false)", |_| Ok(false.into()));
test_interpret!(nativefn_math_sqrt, "return math.sqrt(math.pi)", |_| Ok(
    1.772_453_850_905_515_9.into()
));
test_interpret!(nativefn_math_cos, "return math.cos(math.pi)", |_| Ok((-1.0).into()));
test_interpret!(nativefn_math_sin, "return math.sin(0)", |_| Ok(0.0.into()));
test_interpret!(nativefn_math_exp, "return math.exp(1)", |_| Ok(std::f64::consts::E.into()));

test_interpret!(if_eq_true, "a = 5; if 5==a then return 1 end", |_| Ok(1.0.into()));
test_interpret!(if_eq_false, "a = 4; if 5==a then return 1 end", |_| Ok(RuaVal::nil()));

test_interpret!(if_ge_true, "a = 5; if 5>=a then return 1 end", |_| Ok(1.0.into()));
test_interpret!(if_ge_false, "a = 4; if a>=5 then return 1 end", |_| Ok(RuaVal::nil()));
test_interpret!(if_gt_true, "a = 4; if 5>a then return 1 end return true", |_| Ok(1.0.into()));

test_interpret!(if_le_true, "a = 4; if a<=5 then return 1 end", |_| Ok(1.0.into()));
test_interpret!(if_lt_false, "a = 4; if 5<a then return 1 end return true", |_| Ok(true.into()));

test_interpret!(if_else_false, "if 5<=4 then return 1 else return true end", |_| Ok(true.into()));
test_interpret!(if_else_true, "if 4<=5 then return 1 else return true end", |_| Ok(1.0.into()));

test_interpret!(
    if_elseif1,
    "
if 4<=5 then
    return 1;
elseif assert(false) then
    assert(false)
elseif nil+nil<2 then
    assert(false)
else
    assert(false)
end",
    |_| Ok(1.0.into())
);

test_interpret!(
    if_elseif2,
    "
if 5<=4 then
    assert(false)
elseif 123 then
    return 5
elseif nil+nil<2 then
    assert(false)
else
    assert(false)
end",
    |_| Ok(5.0.into())
);

test_interpret!(
    if_elseif3,
    "
if 5<=4 then
    assert(false)
elseif false then
    return nil + nil
elseif 2<5 and 'asdf' == 'as' .. 'df' then
    return 7
else
    assert(false)
end",
    |_| Ok(7.0.into())
);

test_interpret!(
    if_elseif4,
    "
if 5<=4 then
    assert(false)
elseif false then
    return nil + nil
elseif 2<5 and 'asdf' ~= 'as' .. 'df' then
    assert(false)
else
    return 18
end",
    |_| Ok(18.0.into())
);

test_interpret!(if_local_true, "local a = 2; if a then return 1 else return 2 end", |_| Ok(
    1.0.into()
));
test_interpret!(if_local_false, "local a = false; if a then return 1 else return 2 end", |_| Ok(
    2.0.into()
));

test_interpret!(and, "local a = true; return a and 5", |_| Ok(5.0.into()));
test_interpret!(and2, "local a = 3; return a>2 and 5", |_| Ok(5.0.into()));
test_interpret!(
    and3,
    "
    a = 1; b = 2
    return a + b and 5 + b",
    |_| Ok(7.0.into())
);

test_interpret!(and_shortciruit, "local a = false; return a and nil + nil", |_| Ok(false.into()));
test_interpret!(and_shortciruit2, "return false and nil + nil", |_| Ok(false.into()));

test_interpret!(or, "return false or 'test'", |vm| Ok((*b"test").into_rua(vm)));
test_interpret!(or_shortciruit, "return 2 or nil + nil", |_| Ok(2.0.into()));

test_interpret!(
    many_and,
    "
    local t, f = true, false;
    return t and (2 and f)",
    |_| Ok(false.into())
);

test_interpret!(
    many_and2,
    "
    local t, f = true, false;
    return (t and t) and f",
    |_| Ok(false.into())
);

test_interpret!(
    many_and3,
    "
    t = true; f= false;
    return (f and t) and t",
    |_| Ok(false.into())
);

test_interpret!(
    many_or,
    "
    t = true; f= false;
    return (f or t) or t",
    |_| Ok(true.into())
);

test_interpret!(
    many_or2,
    "
    t = true; f= false;
    return (f or f) or t",
    |_| Ok(true.into())
);

test_interpret!(
    many_or3,
    "
    t = true; f= false;
    return (f or t) or f",
    |_| Ok(true.into())
);

test_interpret!(
    many_or4,
    "
    t = true; f= false;
    return f or (t or f)",
    |_| Ok(true.into())
);

test_interpret!(
    if_and,
    "
    t = true; f= false;
    if t and t then return 1
    else return 2 end",
    |_| Ok(1.0.into())
);

test_interpret!(
    if_and2,
    "
    a = 4; b = 5
    if a>2 and b<=5 then return 1
    else return 2 end",
    |_| Ok(1.0.into())
);

test_interpret!(
    if_and3,
    "
    a = 4; b = 5
    if a>4 and b<=5 then return 1
    else return 2 end",
    |_| Ok(2.0.into())
);

test_interpret!(
    if_or,
    "
    t = true; f= false;
    if f or t then return 1
    else return 2 end",
    |_| Ok(1.0.into())
);

test_interpret!(
    if_or2,
    "
    a = 4; b = 5
    if a>4 or b>=5 then return 1
    else return 2 end",
    |_| Ok(1.0.into())
);

test_interpret!(
    if_or3,
    "
    a = 4; b = 5
    if a>4 or b>=6 then return 1
    else return 2 end",
    |_| Ok(2.0.into())
);

test_interpret!(
    while_statement,
    "local i = 0
    while i < 42 do
        i = i + 1
    end
    print(i)
    return i",
    |_| Ok(42.0.into())
);

test_interpret!(
    repeat_statement1,
    "local i = 0
    repeat
        i = i + 1
    until i>42
    return i",
    |_| Ok(43.0.into())
);

test_interpret!(
    repeat_statement2,
    "local i = 0
    repeat
        if i > 25 then return i end
        i = i + 1
    until false
    assert(false)",
    |_| Ok(26.0.into())
);

test_interpret!(
    repeat_statement3,
    "local i = 0
    repeat
        i = i + 1
    until true
    return i",
    |_| Ok(1.0.into())
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
        vec![("bar".into(), 3), ("foo".into(), 6), ("<main>".into(), 8)]
    ))
);
test_interpret!(
    stack_trace2,
    "function foo()
    assert(false, 'custom error')
end
foo()",
    |vm| Err(EvalErrorTraced::new(
        EvalError::AssertionFailed(Some((*b"custom error").into_rua(vm))),
        vec![("assert".into(), 0), ("foo".into(), 2), ("<main>".into(), 4)]
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
    |_| Ok(RuaVal::nil())
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
    |_| Ok(RuaVal::nil())
);

test_interpret!(
    table_with_local_key,
    "
    local b = 'test'
    local a = {1, b = true, 2, [2+3] = false}
    return a.b",
    |_| Ok(true.into())
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
    |_| Ok(true.into())
);

test_interpret!(
    call_with_less_args,
    "
function foo(a, b, c)
    return c
end
return foo()",
    |_| Ok(RuaVal::nil())
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
        print(n)
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

test_interpret!(
    multiassign_globals,
    "
    a, b, c = 2, 5, 10
    assert(a==2)
    assert(b==5)
    assert(c==10)
    return a + c - b
",
    |_| Ok(7.0.into())
);

test_interpret!(
    multiassign_locals,
    "
    local a
    local b
    local c
    a, b, c = 2, 5, 10
    assert(a==2)
    assert(b==5)
    assert(c==10)
    return a + c - b
",
    |_| Ok(7.0.into())
);

test_interpret!(
    multiassign_upvalues,
    "
    local a
    local b
    local c
    function foo()
        a, b, c = 2, 5, 10
    end
    foo()
    assert(a==2)
    assert(b==5)
    assert(c==10)
    return a + c - b
",
    |_| Ok(7.0.into())
);

test_interpret!(
    multiassign_fields,
    "
    local a = {}
    a.a, a.b, a['c'] = 2, 5, 10
    assert(a.a==2)
    assert(a.b==5)
    assert(a.c==10)
    return a.a + a.c - a.b
",
    |_| Ok(7.0.into())
);

test_interpret!(
    multiassign_mixed,
    "
    local a = {}
    local d
    a.a, b, a['c'], d = 2, 5, 10, 1
    assert(a.a==2)
    assert(b==5)
    assert(a.c==10)
    assert(d==1)
    return a.a + a.c - b + d
",
    |_| Ok(8.0.into())
);

test_interpret!(
    multiassign_more_lhs,
    "
    local a = {}
    local e = {h=5}
    local d = 3
    a.a, b, a['c'], d, e.h = 2, 5, 10
    assert(a.a==2)
    assert(b==5)
    assert(a.c==10)
    assert(d==nil)
    return e.h
",
    |_| Ok(RuaVal::nil())
);

test_interpret!(
    multiassign_more_rhs,
    "
    local flag = false
    function asd() flag = true; return 3 end
    local a = {}
    a.a, b = 2, 5, 10, asd()
    return a.a + b
",
    |_| Ok(7.0.into())
);

test_interpret!(
    multilocal,
    "
    local a, b, c = 1, 2, 4
    assert(a==1)
    assert(b==2)
    assert(c==4)
    return a + b - c
",
    |_| Ok((-1.0).into())
);

test_interpret!(
    multilocal_more_lhs,
    "
    local a, b, c = 1, 2
    assert(a==1)
    assert(b==2)
    assert(c==nil)
    return c
",
    |_| Ok(RuaVal::nil())
);

test_interpret!(
    multilocal_more_rhs,
    "
    local flag = false
    function asd() flag = true; return 3 end
    local a, b, c = 1, 2, 4, asd()
    assert(a==1)
    assert(b==2)
    assert(c==4)
    assert(flag)
    return a + b - c
",
    |_| Ok((-1.0).into())
);

test_interpret!(
    iterative_fib,
    "
    local n, i = 7, 3
    local fib, last_fib = 1, 1
    while i <= n do
        last_fib, fib = fib, fib + last_fib
        i = i + 1
    end

    return fib",
    |_| Ok(13.0.into())
);

test_interpret!(
    dynamic_prog_fib,
    "
    local fibo_mem = {}
    local function fibo(n)
        if n<3 then return 1 end
        if fibo_mem[n] then return fibo_mem[n] end

        fibo_mem[n] = fibo(n-1) + fibo(n-2)
        return fibo_mem[n]
    end
    return fibo(8)",
    |_| Ok(21.0.into())
);

test_interpret!(
    call_str_literal,
    "
    return tonumber '58'
",
    |_| Ok(58.0.into())
);

test_interpret!(
    call_table_literal,
    "
    local function foo(t) return #t end
    return foo { 5, bar = 8, 'hi' }
",
    |_| Ok(2.0.into())
);

test_interpret!(
    do_block,
    "
    do
        local a = 5
        assert(a==5)
    end
    return a
",
    |_| Ok(RuaVal::nil())
);

test_interpret!(
    forward_for_loop,
    "
    local sum = 0
    for i = 1, 5 do
        sum = sum + i
    end
    return sum
",
    |_| Ok(15.0.into())
);

test_interpret!(
    forward_for_loop_step,
    "
    local sum = 0
    for i = 1, 5, 2 do
        sum = sum + i
    end
    return sum
",
    |_| Ok(9.0.into())
);

test_interpret!(
    for_loop_upvalues,
    "
    local a = {}
    for i = 1, 10 do
        table.insert(a, function() return i end)
    end

    local sum = 0
    for i = 1, 10, 4 do
        sum = sum + a[i]()
    end
    return sum
",
    |_| Ok(15.0.into())
);

test_interpret!(
    for_loop_that_never_runs,
    "
    local a = {}
    for i = 6, 5 do
        table.insert(a, function() return i end)
    end

    return #a
",
    |_| Ok(0.0.into())
);

test_interpret!(
    backward_for_loop,
    "
    local sum = 0
    for i = 5, 1, -1 do
        sum = sum + i
    end
    return sum
",
    |_| Ok(15.0.into())
);

test_interpret!(
    backward_for_loop_that_never_runs,
    "
    local a = {}
    for i = 10, 11, -1 do
        table.insert(a, function() return i end)
    end

    return #a
",
    |_| Ok(0.0.into())
);

test_interpret!(
    val_in_stack_isnt_gced,
    "
    local a = {1, 2}
    collectgarbage()
    return a[1]
",
    |_| Ok(1.0.into())
);

test_interpret!(
    global_val_isnt_gced,
    "
    a = {1, 2}
    collectgarbage()
    return a[1]
",
    |_| Ok(1.0.into())
);

test_interpret!(
    upval_isnt_gced,
    "
    function foo()
        a = {1, 2}
        function bar()
            return a
        end
        return bar
    end
    local bar = foo()
    collectgarbage()
    return bar()[1]
",
    |_| Ok(1.0.into())
);

test_interpret!(
    val_in_call_stack_isnt_gced,
    "
    function baz()
        collectgarbage()
    end
    function foo()
        local a = {1, 2}
        function bar()
            baz()
        end
        bar()
        return a
    end
    local a = foo()
    return a[1]
",
    |_| Ok(1.0.into())
);

test_interpret!(
    single_obj_circular_ref_is_gced,
    "
    a = {}
    a.b = a
",
    |_| Ok(RuaVal::nil())
);

test_interpret!(
    many_obj_circular_ref_is_gced,
    "
    first = {}
    last = first
    for i = 1, 30 do
        local new = {}
        new.prev = last
        last = new
    end
    first.prev = last
",
    |_| Ok(RuaVal::nil())
);

test_interpret!(
    closure_obj_circular_ref_is_gced,
    "
    function get_closure()
        local a = {}
        local function f()
            local function g()
                return a
            end
            return g
        end
        a.func = f()
        return f
    end
    local closure = get_closure()
",
    |_| Ok(RuaVal::nil())
);

test_interpret!(
    long_table_literal,
"local a = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, }",
    |_| Ok(RuaVal::nil()) // Just testing it doesn't panic due to bad register allocation
);
