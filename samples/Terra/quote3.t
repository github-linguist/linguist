c = global(0)
terra count()
    c = c + 1
    return c
end
foo = macro(function(a,b) return `a + a + a + b + count() end)
terra doit()
    var a = -100
    return foo(count(),4)
end

local test = require("test")
test.eq(doit(),14)