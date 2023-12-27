local DEFAULT = 1
local function fact(n)
    if n < 2 then return DEFAULT end
    return n * fact(n-1)
end
return fact(10000)
