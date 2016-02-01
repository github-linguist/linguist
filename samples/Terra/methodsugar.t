--the zero line
M = {}
struct M.B {a : int, b : int}

terra M.B:foo(a : int)
    return self.a + a
end

terra bar()
    var b = M.B { 1,2 }
    return b:foo(3)
end

test = require("test")
test.eq(bar(),4)
