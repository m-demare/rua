
# usage: ./scripts/bench_flame.sh bench_group function_path bench_name
# example: ./scripts/bench_flame.sh eval_bench eval_bench::stress_gc stress_gc
cargo flamegraph --bench $1 --skip-after $2 -F 10000 --min-width 0.000002 --image-width 1800 -- $3
firefox --private-window flamegraph.svg
