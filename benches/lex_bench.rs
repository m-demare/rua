use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rua::{identifiers::Trie, lex::tokenize};

fn lex_one_identifier(c: &mut Criterion) {
    let input = "local aaaaaa = 123".to_owned() + &"\naaaaaa = aaaaaa+123".repeat(500000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_one_identifier", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn lex_many_identifiers(c: &mut Criterion) {
    let input = "dajf lkjflsk iojf iowe sadijf woefjoweaifjweaf sfsdl _sdfsdaf _12345 sdf_o489i we894sadf lkjscvv hkfjsdh i j k\n".repeat(100000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_many_identifiers", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn dec_ints(c: &mut Criterion) {
    let input = "1234567\n".repeat(1000000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_dec_ints", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn hex_ints(c: &mut Criterion) {
    let input = "0x123ABC\n".repeat(1000000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_hex_ints", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn bin_ints(c: &mut Criterion) {
    let input = "0b100110\n".repeat(1000000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_bin_ints", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn float_numbers(c: &mut Criterion) {
    let input = "0.987654\n.123456\n".repeat(1000000/2);
    let mut identifiers = Trie::new();
    c.bench_function("lex_floats", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn single_digits(c: &mut Criterion) {
    let input = "1\n".repeat(1000000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_digits", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn short_floats(c: &mut Criterion) {
    let input = "0.9\n.1\n".repeat(1000000/2);
    let mut identifiers = Trie::new();
    c.bench_function("lex_short_floats", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn comments(c: &mut Criterion) {
    let input = "-- aaaa bbbb cccc dddd\n".repeat(1000000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_comments", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn spaces(c: &mut Criterion) {
    let input = "              +     \t\t    +   \n".repeat(1000000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_spaces", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn bunch_of_keywords(c: &mut Criterion) {
    let input = "local function and or not break while do if then end\n".repeat(500000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_keywords", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

fn lex_fibonacci(c: &mut Criterion) {
    let input = r"local function fibo(n)
    if n < 2 then
        return n
    else
        return fibo(n-1) + fibo(n-2)
    end
end
".repeat(50000);
    let mut identifiers = Trie::new();
    c.bench_function("lex_fibonacci", |b| b.iter(|| tokenize(black_box(&input), black_box(&mut identifiers))));
}

criterion_group! {
    name=lex_bench;
    config = Criterion::default();
    targets=
        lex_one_identifier,
        lex_many_identifiers,
        dec_ints,
        hex_ints,
        bin_ints,
        float_numbers,
        single_digits,
        short_floats,
        comments,
        spaces,
        bunch_of_keywords,
        lex_fibonacci,
}
criterion_main!(lex_bench);
