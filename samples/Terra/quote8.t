a = {}

local c = terralib.includec("stdio.h")


a.c = {`1,`2,`3}

a.b = quote
    return a.c,a.c
end


terra foo()
    a.b
end


local test = require("test")
test.meq({1,2,3,1,2,3},foo())
