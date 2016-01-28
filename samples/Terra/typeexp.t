atype = int
terra foo(a : atype)
   var b : &atype = &a
   return @b
end

local test = require("test")

test.eq(foo(3),3)