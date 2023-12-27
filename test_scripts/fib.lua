local k = 700000

local function fibo(n)
    local i = 3

    local fib, last_fib = 1, 1
    while i <= n do
        last_fib, fib = fib, fib + last_fib
        i = i + 1
    end
    return fib
end

print(tostring(k) .. "th fib is", fibo(k))
