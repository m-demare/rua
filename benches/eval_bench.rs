use std::cell::RefCell;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rua::{
    eval::{eval, isolate::Isolate},
    lex::Tokenizer,
    parser::parse,
};
use rua_identifiers::Trie;

fn bench(c: &mut Criterion, input: &str, name: &str) {
    c.bench_function(name, |b| {
        b.iter(|| {
            let mut identifiers = Trie::new();
            let tokens = Tokenizer::new(input.chars(), black_box(&mut identifiers));
            let prog = parse(tokens).unwrap();
            black_box(eval(&prog, RefCell::new(Isolate::new(identifiers)).into()))
        })
    });
}

fn eval_fibonacci_rec(c: &mut Criterion) {
    let input = r"
local m = 70000
local function fibo(n)
    local i = 3

    local fib, last_fib = 1, 1
    while i <= n do
        last_fib, fib = fib, fib + last_fib
        i = i + 1
    end
    return fib
end
return fibo(m)";

    bench(c, input, "eval_fibonacci")
}

fn eval_fibonacci(c: &mut Criterion) {
    let input = r"
local m = 2000
local fibo_mem = {}
local function fibo(n)
    if n<3 then return 1 end
    if fibo_mem[n] then return fibo_mem[n] end

    fibo_mem[n] = fibo(n-1) + fibo(n-2)
    return fibo_mem[n]
end

return fibo(m)";

    bench(c, input, "eval_fibonacci_rec")
}

fn eval_fizzbuzz(c: &mut Criterion) {
    let input = r"
local n = 30000
local i = 1

while i <= n do
    local out = ''
    if i % 3 == 0 then
        out = out .. 'Fizz'
    end
    if i % 5 == 0 then
        out = out .. 'Buzz'
    end
    if out == '' then
        out = tostring(i)
    end
    i = i + 1
end
";

    bench(c, input, "eval_fizzbuzz")
}

criterion_group! {
    name=eval_bench;
    config = Criterion::default();
    targets=
        eval_fibonacci,
        eval_fibonacci_rec,
        eval_fizzbuzz,
}
criterion_main!(eval_bench);