
local c = `10
local a = `4 + c
terra doit()
    return a
end

local test = require("test")
test.eq(doit(),14)