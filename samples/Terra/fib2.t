local test = require("test")

local Num = int

terra fib(a : Num) : Num
    if a == 0 then
        return 1
    elseif a == 1 then
        return 1
    else
        return fib(a - 1) + fib(a - 2)
    end
end

function fib2(a) 
    if a == 0 then
        return 1
    elseif a == 1 then
        return 1
    else
        return fib2(a - 1) + fib2(a - 2)
    end
end
for i = 0,10 do
    print(fib(i))
    test.eq(fib(i),fib2(i))
end
