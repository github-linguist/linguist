--the zero line
M = {}
struct M.B {a : int, b : int}

terra M.B:foo(a : int)
    return self.a + a
end

terra rify()
    var a = M.B { 1,2}
    return a
end
terra bar()
    return (rify()):foo(3)
end

test = require("test")
test.eq(bar(),4)
