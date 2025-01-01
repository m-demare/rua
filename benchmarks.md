# Benchmarks

Benchmarks were ran on a laptop with an Intel i5-5200U, 16GB of DDR3 RAM (1333MHz),
running Arch Linux

Benchmark files can be found in `test_scripts/*.lua`

| Benchmark     |   Rua   | Rua (with PGO) | Lua 5.1 | Lua 5.4 |  LuaJIT  |   Luau   |
| ------------- | ------- | -------------- | ------- | ------- | -------- | -------- |
| bintrees (10) | 51.2ms  |     50.3ms     | 41.1ms  | 29.2ms  |  16.8ms  |  22.3ms  |
| nbody (10000) | 105.1ms |    105.2ms     | 55.9ms  | 32.1ms  |  5.0ms   |  22.7ms  |
| stress_rc     | 1.044s  |    967.0ms     | 1.509s  | 1.480s  | 722.6ms  | 513.1ms  |
| stress_gc     | 1.471s  |    1.272s      | 1.486s  | 1.536s  | 722.3ms  | 509.9ms  |

