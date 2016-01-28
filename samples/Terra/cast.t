terra bar(a : int) : int
    return a + 1
end
terra foo(a : int) : int
    
    return a
end

local test = require("test")
test.eq(foo(2),2)
test.eq(bar(2),3)
