cargo flamegraph -F 10000 --min-width 0.000002 --image-width 1800 --profile profiling --deterministic --skip-after rua::main -- $1
firefox --private-window flamegraph.svg
