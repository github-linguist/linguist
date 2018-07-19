local zigzag = {}

function zigzag.new(n)
    local a = {}
    local i -- cols
    local j -- rows

    a.n = n
    a.val = {}

    for j = 1, n do
        a.val[j] = {}
        for i = 1, n do
            a.val[j][i] = 0
        end
    end

    i = 1
    j = 1

    local di
    local dj
    local k = 0

    while k < n * n do
        a.val[j][i] = k
        k = k + 1
        if i == n then
            j = j + 1
            a.val[j][i] = k
            k = k + 1
            di = -1
            dj = 1
        end
        if j == 1 then
            i = i + 1
            a.val[j][i] = k
            k = k + 1
            di = -1
            dj = 1
        end
        if j == n then
            i = i + 1
            a.val[j][i] = k
            k = k + 1
            di = 1
            dj = -1
        end
        if i == 1 then
            j = j + 1
            a.val[j][i] = k
            k = k + 1
            di = 1
            dj = -1
        end
        i = i + di
        j = j + dj
    end

    setmetatable(a, {__index = zigzag, __tostring = zigzag.__tostring})
    return a
end

function zigzag:__tostring()
    local s = {}
    for j = 1, self.n do
        local row = {}
        for i = 1, self.n do
            row[i] = string.format('%d', self.val[j][i])
        end
        s[j] = table.concat(row, ' ')
    end
    return table.concat(s, '\n')
end

print(zigzag.new(5))
