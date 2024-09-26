# Benchmarks

Benchmarks were ran on a laptop with an Intel i5-5200U, 16GB of DDR3 RAM (1333MHz),
running Arch Linux

Benchmark files can be found in `test_scripts/*.lua`

| Benchmark     |   Rua   | Rua (with PGO) | Lua 5.1 | Lua 5.4 |  LuaJIT  |   Luau   |
| ------------- | ------- | -------------- | ------- | ------- | -------- | -------- |
| bintrees (10) | 57.9ms  |     50.9ms     | 42.3ms  | 30.2ms  |  17.5ms  |  22.7ms  |
| nbody (10000) | 125.7ms |    100.4ms     | 56.9ms  | 32.4ms  |  5.0ms   |  23.6ms  |
| stress_rc     | 1.217s  |    1.088s      | 1.497s  | 1.467s  | 732.4ms  | 520.9ms  |
| stress_gc     | 1.595s  |    1.377s      | 1.483s  | 1.512s  | 724.6ms  | 509.9ms  |

Note: PGO is currently [not working well with
LTO](https://github.com/llvm/llvm-project/issues/57501), so all results with PGO
should improve quite a bit when this is fixed

