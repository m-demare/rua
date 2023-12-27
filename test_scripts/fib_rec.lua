local k = 7000
local fibo_mem = {}

local function fibo(n)
    if n<3 then return 1 end
    if fibo_mem[n] then return fibo_mem[n] end

    fibo_mem[n] = fibo(n-1) + fibo(n-2)
    return fibo_mem[n]
end

print(tostring(k) .. "th fib is", fibo(k))
