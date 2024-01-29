local DEFAULT = 1
local function fact(n, acc)
    if n < 2 then return acc end
    return fact(n-1, n*acc)
end
return fact(10000, DEFAULT)

