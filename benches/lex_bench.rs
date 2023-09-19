use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rua::lex::tokenize;

fn lex_one_identifier_bench(c: &mut Criterion) {
    let input = "local aaaaaa = 123".to_owned() + &"\naaaaaa = aaaaaa+123".repeat(1000000);
    c.bench_function("lex_one_identifier", |b| b.iter(|| tokenize(black_box(&input))));
}

fn lex_many_identifiers_bench(c: &mut Criterion) {
    let input = "dajf lkjflsk iojf iowe sadijf woefjoweaifjweaf sfsdl _sdfsdaf _12345 sdf_o489i we894sadf lkjscvv hkfjsdh i j k\n".repeat(100000);
    c.bench_function("lex_many_identifiers", |b| b.iter(|| tokenize(black_box(&input))));
}

fn dec_ints_bench(c: &mut Criterion) {
    let input = "1234567\n".repeat(1000000);
    c.bench_function("lex_dec_ints", |b| b.iter(|| tokenize(black_box(&input))));
}

fn hex_ints_bench(c: &mut Criterion) {
    let input = "0x123ABC\n".repeat(1000000);
    c.bench_function("lex_hex_ints", |b| b.iter(|| tokenize(black_box(&input))));
}

fn bin_ints_bench(c: &mut Criterion) {
    let input = "0b100110\n".repeat(1000000);
    c.bench_function("lex_bin_ints", |b| b.iter(|| tokenize(black_box(&input))));
}

fn float_numbers_bench(c: &mut Criterion) {
    let input = "0.987654\n.123456\n".repeat(1000000/2);
    c.bench_function("lex_floats", |b| b.iter(|| tokenize(black_box(&input))));
}

fn single_digits_bench(c: &mut Criterion) {
    let input = "1\n".repeat(1000000);
    c.bench_function("lex_digits", |b| b.iter(|| tokenize(black_box(&input))));
}

fn short_floats_bench(c: &mut Criterion) {
    let input = "0.9\n.1\n".repeat(1000000/2);
    c.bench_function("lex_short_floats", |b| b.iter(|| tokenize(black_box(&input))));
}

fn comments_bench(c: &mut Criterion) {
    let input = "-- aaaa bbbb cccc dddd\n".repeat(1000000);
    c.bench_function("lex_comments", |b| b.iter(|| tokenize(black_box(&input))));
}

fn spaces_bench(c: &mut Criterion) {
    let input = "              +     \t\t    +   \n".repeat(1000000);
    c.bench_function("lex_spaces", |b| b.iter(|| tokenize(black_box(&input))));
}

fn bunch_of_keywords_bench(c: &mut Criterion) {
    let input = "local function and or not break while do if then end\n".repeat(500000);
    c.bench_function("lex_keywords", |b| b.iter(|| tokenize(black_box(&input))));
}

criterion_group! {
    name=benches;
    config = Criterion::default().measurement_time(Duration::from_secs(30));
    targets=
        lex_one_identifier_bench,
        lex_many_identifiers_bench,
        dec_ints_bench,
        hex_ints_bench,
        bin_ints_bench,
        float_numbers_bench,
        single_digits_bench,
        short_floats_bench,
        comments_bench,
        spaces_bench,
        bunch_of_keywords_bench,
}
criterion_main!(benches);
