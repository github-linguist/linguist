a = {}

local c = terralib.includec("stdio.h")


a.c = quote
    c.printf("hello\n")
end

a.b = quote
    var d = 4
    a.c
    return d
end


terra foo()
    a.c
    a.b
end

local test = require("test")
test.eq(foo(),4)