local a = symbol()
local b = {}

local c = symbol(int)
local d = {}


terra foo()
	var [a],[b] = 1.25
	var [c],[d] = 3.25
	return [a] + [c]
end

local test = require("test")

test.eq(foo(),4.25)