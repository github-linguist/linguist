local test = require("test")

local Num = int

local fib =
terra(a : Num) : Num
    var i,c,p = 0,1,1
    while i < a do
        c,p = c + p,c
        i = i + 1
    end
    return c
end

function fib2(a) 
    local i,c,p = 0,1,1
    while i < a do
        c,p = c + p,c
        i = i + 1
    end
    return c
end
for i = 0,10 do
    print(fib(i))
    test.eq(fib(i),fib2(i))
end