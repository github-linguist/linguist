a = {}

local c = terralib.includec("stdio.h")


a.c = {`1,`2,`3}

a.b = quote
    return a.c._0,(a.c)._0
end


terra foo()
    a.b
end


local test = require("test")
test.meq({1,1}, foo())