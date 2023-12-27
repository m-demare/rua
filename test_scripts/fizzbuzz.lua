local n = 10000
local i = 1

while i <= n do
    local out = ''
    if i % 3 == 0 then
        out = out .. 'Fizz'
    end
    if i % 5 == 0 then
        out = out .. 'Buzz'
    end
    if out == '' then
        out = tostring(i)
    end
    print(out)
    i = i + 1
end

