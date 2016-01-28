local test = require("test")

terra foo(a : double, b : double, c : double) : bool
    return a < b and b < c
end

test.eq(foo(1,2,3),true)
test.eq(foo(1,2,1),false)
test.eq(foo(2,1,2),false)