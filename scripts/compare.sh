cargo build --release
hyperfine -N --warmup 3 --min-runs 5 \
    -n rua "./target/release/rua $1.lua" \
    -n lua5.1 "lua5.1 $1.lua" \
    -n lua5.2 "lua5.2 $1.lua" \
    -n lua5.3 "lua5.3 $1.lua" \
    -n lua5.4 "lua5.4 $1.lua" \
    -n luajit "luajit $1.lua" \
    -n "luajit (no jit)" "luajit -joff $1.lua" \
    -n luau "luau $1.lua"
