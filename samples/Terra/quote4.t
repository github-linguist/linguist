
local str2 = `{a = 4}
local str = `str2
terra doit()
    return str.a
end

local test = require("test")
test.eq(doit(),4)