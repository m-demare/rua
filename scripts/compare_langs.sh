cargo build --release
hyperfine -N --warmup 3 --min-runs 5 \
    -n rua "./target/release/rua $1.lua" \
    -n python "python $1.py" \
    -n js "node $1.js"\
    -n "js (no jit)" "node --jitless $1.js"
