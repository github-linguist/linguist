
terra simple(a : int)
    return a + a
end
local test = require("test")
test.eq(simple(2),4)