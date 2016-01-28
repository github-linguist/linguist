
terra bar(a : &int)
    @a = @a + 1
end
terra foo(a : int) : int
    var b : int
    b = a
    bar(&b)
    return b
end

local test = require("test")
test.eq(foo(4),5)