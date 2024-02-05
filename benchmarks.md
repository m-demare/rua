# Benchmarks

Benchmarks were ran on a laptop with an Intel i5-5200U, 16GB of DDR3 RAM (1333MHz),
running Arch Linux

Benchmark files can be found in `test_scripts/`

| Benchmark     |   Rua   | Rua (with PGO) | Lua 5.1 | Lua 5.4 |  LuaJIT  |   Luau   |
| ------------- | ------- | -------------- | ------- | ------- | -------- | -------- |
| bintrees (10) | 61.7ms  |     53.9ms     | 39.3ms  | 27.9ms  |  14.7ms  |  20.9ms  |
| nbody (10000) | 118.9ms |    101.3ms     | 54.1ms  | 31.4ms  |  4.7ms   |  22.0ms  |
| stress_rc     | 1.368s  |    1.216s      | 1.488s  | 1.463s  | 698.3ms  | 510.5ms  |
| stress_gc     | 1.818s  |    1.575s      | 1.449s  | 1.501s  | 699.7ms  | 503.5ms  |

