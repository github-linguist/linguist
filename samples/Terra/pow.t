local N = 4 -- N is a Lua variable
terra powN(a : double)
    var r = 1
    for i = 0, N do
        r = r * a
    end
    return r
end




local math = {}
for N = 1,10 do
    math["pow"..tostring(N)] = terra(a : double)
        var r = 1
        for i = 0, N do
            r = r * a
        end
        return r
    end
end

local test = require("test")
test.eq(powN(3),81)
test.eq(math.pow1(2),2)
test.eq(math.pow2(2),4)
test.eq(math.pow3(2),8)