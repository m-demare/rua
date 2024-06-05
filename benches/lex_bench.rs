use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rua::{eval::Vm, lex::Tokenizer};

fn bench(c: &mut Criterion, input: &str, name: &str) {
    c.bench_function(name, |b| {
        b.iter(|| {
            let mut vm = Vm::default();
            let tokens = Tokenizer::new(black_box(input.bytes()), black_box(&mut vm));
            for t in tokens {
                black_box(t);
            }
        })
    });
}

fn lex_many_identifiers(c: &mut Criterion) {
    let input = "dajf lkjflsk iojf iowe sadijf woefjoweaifjweaf sfsdl _sdfsdaf _12345 sdf_o489i we894sadf lkjscvv hkfjsdh i j k
        local function and or not break while do if then end\n".repeat(8000);
    bench(c, &input, "lex_many_identifiers");
}

fn dec_ints(c: &mut Criterion) {
    let input = "1234567\n".repeat(600000);
    bench(c, &input, "lex_dec_ints");
}

fn hex_ints(c: &mut Criterion) {
    let input = "0x123ABC\n".repeat(600000);
    bench(c, &input, "lex_hex_ints");
}

fn bin_ints(c: &mut Criterion) {
    let input = "0b100110\n".repeat(600000);
    bench(c, &input, "lex_bin_ints");
}

fn float_numbers(c: &mut Criterion) {
    let input = "0.987654\n.123456\n".repeat(600000 / 2);
    bench(c, &input, "lex_floats");
}

fn single_digits(c: &mut Criterion) {
    let input = "1\n".repeat(1000000);
    bench(c, &input, "lex_digits");
}

fn comments(c: &mut Criterion) {
    let input = "-- aaaa bbbb cccc dddd\n".repeat(600000);
    bench(c, &input, "lex_comments");
}

fn spaces(c: &mut Criterion) {
    let input = "              +     \t\t    +   \n".repeat(400000);
    bench(c, &input, "lex_spaces");
}

fn lex_fibonacci(c: &mut Criterion) {
    let input = r"local function fibo(n)
    if n < 2 then
        return n
    else
        return fibo(n-1) + fibo(n-2)
    end
end
"
    .repeat(30000);
    bench(c, &input, "lex_fibonacci");
}

criterion_group! {
    name=lex_bench;
    config = Criterion::default().measurement_time(Duration::from_secs(3));
    targets=
        lex_many_identifiers,
        dec_ints,
        hex_ints,
        bin_ints,
        float_numbers,
        single_digits,
        comments,
        spaces,
        lex_fibonacci,
}
criterion_main!(lex_bench);
