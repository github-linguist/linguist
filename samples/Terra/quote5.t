
c = global(0)
terra foo(a : int)
    c = c + a
end

local stmts = quote
    foo(5)
    foo(6)
    foo(7)
end

local stmts2 = {stmts,stmts}
terra doit()
    stmts
    stmts2
    return c
end
local test = require("test")
test.eq(doit(),54)