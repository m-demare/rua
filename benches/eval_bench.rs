use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rua::{compiler::compile, eval::Vm};

fn bench(c: &mut Criterion, input: &str, name: &str) {
    c.bench_function(name, |b| {
        b.iter(|| {
            let mut vm = Vm::new();
            let prog = compile(input.bytes(), &mut vm).unwrap();
            black_box(vm.interpret(prog.into()))
        })
    });
}

fn bench_file(c: &mut Criterion, input: &str, name: &str) {
    c.bench_function(name, |b| {
        b.iter(|| {
            let input = std::fs::read(input).expect("Error: input file does not exist");
            let mut vm = Vm::new();
            let prog = compile(input.iter().copied(), &mut vm).unwrap();
            black_box(vm.interpret(prog.into()))
        })
    });
}

fn eval_fibonacci(c: &mut Criterion) {
    let input = r"
local m = 70000
local function fibo(n)
    local fib, last_fib = 1, 1
    for i = 3,n do
        last_fib, fib = fib, fib + last_fib
    end
    return fib
end
return fibo(m)";

    bench(c, input, "eval_fibonacci")
}

fn eval_fibonacci_rec(c: &mut Criterion) {
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

for i =1,n do
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
end
";

    bench(c, input, "eval_fizzbuzz")
}

fn stress_rc(c: &mut Criterion) {
    let input = r"
for _ = 1, 500 do
    local first = {}
    local last = first
    for _ = 1, 400 do
        local new = { prev = last }
        last = new
    end
end
";

    bench(c, input, "stress_rc")
}

fn stress_gc(c: &mut Criterion) {
    let input = r"
for i = 1, 500 do
    local first = {}
    local last = first
    for _ = 1, 400 do
        local new = { prev = last }
        last = new
    end
    first.prev = last
    if i%4 == 0 then collectgarbage() end
end
";

    bench(c, input, "stress_gc")
}

fn bintrees(c: &mut Criterion) {
    bench_file(c, "test_scripts/bintrees.lua", "bintrees");
}

fn nbody(c: &mut Criterion) {
    bench_file(c, "test_scripts/nbody.lua", "nbody");
}

fn merge_sort(c: &mut Criterion) {
    bench_file(c, "test_scripts/merge_sort.lua", "merge_sort");
}

fn table_insert(c: &mut Criterion) {
    bench_file(c, "test_scripts/table_insert.lua", "table_insert");
}

criterion_group! {
    name=eval_bench;
    config = Criterion::default().measurement_time(Duration::from_secs(10));
    targets=
        eval_fibonacci,
        eval_fibonacci_rec,
        eval_fizzbuzz,
        stress_rc,
        stress_gc,
        bintrees,
        nbody,
        merge_sort,
        table_insert,
}
criterion_main!(eval_bench);
