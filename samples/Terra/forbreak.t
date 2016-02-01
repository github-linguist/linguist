
terra foo(a : int, s : int)
    var r = 0
    for i = 0,a,s do
        if i == 8 then
            break
        end
        r = r + i
        
    end
    return r
end

local test = require("test")
test.eq(foo(10,1),28)
test.eq(foo(10,2),12)
test.eq(foo(0,1),0)