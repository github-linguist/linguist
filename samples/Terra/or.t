local test = require("test")

terra foo(a : double, b : double, c : double) : bool
    return a < b or b < c
end

test.eq(foo(1,2,1),true)
test.eq(foo(2,1,2),true)
test.eq(foo(3,2,1),false)