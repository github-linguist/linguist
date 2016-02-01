
local c = `10
local a = `4 + c
terra doit()
    var c = 3
    return a + a
end

local test = require("test")
test.eq(doit(),28)