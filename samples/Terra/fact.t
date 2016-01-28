local test = require("test")

local Num = int
local terra fact(a : Num) : Num
    var c,i = 1,1
    while i <= a do
        c = c * i
        i = i + 1
    end
    return c
end


test.eq(fact(1),1)
test.eq(fact(2),2)
test.eq(fact(3),6)