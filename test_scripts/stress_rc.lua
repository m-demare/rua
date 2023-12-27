for _ = 1, 8000 do
    local first = {}
    local last = first
    for _ = 1, 1000 do
        local new = { prev = last }
        last = new
    end
end
