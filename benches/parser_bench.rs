use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rua::{compiler::compile, eval::Vm};

fn bench(c: &mut Criterion, input: &str, name: &str) {
    c.bench_function(name, |b| {
        b.iter(|| {
            let mut vm = Vm::default();
            compile(black_box(input.bytes()), black_box(&mut vm))
        })
    });
}

fn parse_assignments(c: &mut Criterion) {
    let input = "local aaaaaa = 123".to_owned() + &"\naaaaaa = aaaaaa+123".repeat(70000);

    bench(c, &input, "parse_assignments")
}

fn parse_fibonacci(c: &mut Criterion) {
    let input = r"function fibo(n)
    if n < 2 then
        return n
    else
        return fibo(n-1) + fibo(n-2)
    end
end
"
    .repeat(10000);

    bench(c, &input, "parse_fibonacci")
}

fn parse_huge_expr(c: &mut Criterion) {
    let input = "return ".to_owned() + &"1 + 5 * 3 - (2^4 * -8) * ".repeat(30000) + "1";

    bench(c, &input, "parse_huge_expr")
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
