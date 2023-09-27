use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rua::{lex::tokenize, parser::parse};

fn parse_assignments(c: &mut Criterion) {
    let input = "local aaaaaa = 123".to_owned() + &"\naaaaaa = aaaaaa+123".repeat(500000);
    let tokens = tokenize(&input);

    c.bench_function("parse_assignments", |b| b.iter(|| parse(black_box(&tokens))));
}

fn parse_fibonacci(c: &mut Criterion) {
    let input = r"local function fibo(n)
    if n < 2 then
        return n
    else
        return fibo(n-1) + fibo(n-2)
    end
end
".repeat(50000);
    let tokens = tokenize(&input);

    c.bench_function("parse_fibonacci", |b| b.iter(|| parse(black_box(&tokens))));
}

fn parse_huge_expr(c: &mut Criterion) {
    let input = "return ".to_owned() + &"1 + 5 * 3 - (2^4 * -8) * ".repeat(30000) + "1";
    let tokens = tokenize(&input);

    c.bench_function("parse_huge_expr", |b| b.iter(|| parse(black_box(&tokens))));
}

criterion_group! {
    name=parser_bench;
    config = Criterion::default();
    targets=
        parse_assignments,
        parse_fibonacci,
        parse_huge_expr,
}
criterion_main!(parser_bench);
