## Rua: A blazingly fast, safe, Rust-based implementation of Lua

Warning: this is still both experimental and unstable.
Any API provided is subject to change

### Goals
- Be mostly Lua 5.1 compatible
- Be 100% safe Rust. If unsafety is added at any point, it should be behind an
optional feature flag
- Achieve performance on par with the official PUC-Rio Lua implementation
- Offer a nice API for interacting between Rust and Lua

### Non goals
- Be a 100% compatible, drop-in replacement for your favourite Lua implementation.
This includes things like:
    - Having the same error messages
    - Bytecode compatibility
    - Having the same behaviour for thing like table iteration order
- Use JIT compilation, since that's impossible to implement within safe Rust

### Currently working
- A rather complete single-pass compiler. Most of the Lua syntax is already supported
  (main exception being iterator-based for-loops)
- Some compile-time optimizations, like constant folding and specialized instructions
- A fast, register-based VM, with performance on par with Lua 5.1 (see [benchmarks](./benchmarks.md))
- A memory safe garbage collector, that achieves a decent performance and memory
usage by combining reference counting and a real tracing GC
- A table implementation similar to Lua's, that automatically optimizes itself
depending on its usage (whether it's used as an array or as a dictionary)
- Proper tail calls
- Some proc macros to make the implementation of native functions incredibly simple
- Closures and lexical scoping
- Exceptions and stacktraces
- A somewhat decent test suite

### TODOs
- Sandboxing
- Coroutines
- Varargs and multiple return values
- Switch to tail call based dispatch if/when explicit_tail_calls is stabilized
- Measure if incremental garbage collection is worth it, given that reference
counting should make GC pauses quite infrequent

### Credits
Some invaluable resources in the creation of this interpreter were:
- [Writing an interpreter in Go](https://interpreterbook.com/), on which I based
the original implementation of the interpreter
- [Crafting interpreters](https://craftinginterpreters.com/), which helped me
transform it into a bytecode interpreter, with a single-pass compiler
- The official [PUC-Rio Lua](https://github.com/lua/lua) implementation, on which I
based my register-based compiler, the tables' implementation, and many other things
- [The Rust performance book](https://nnethercote.github.io/perf-book/), which was
extremely useful for optimizing the interpreter

