local a = symbol()
local b = symbol(int)

local c = symbol(int)
local d = symbol()


terra foo()
	var [a],[b] = 1.25,1.25
	var [c],[d] = 3.25,3.25
	return [a] + [b] + [c] + [d]
end

local test = require("test")

test.eq(foo(),8.50)