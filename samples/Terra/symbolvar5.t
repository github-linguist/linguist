local a = symbol()
local b = { symbol(int), symbol(int) }

local c = symbol(int)
local d = { symbol(), symbol() }


terra foo()
	var [a],[b] = 1.25,1.25,1.25
	var [c],[d] = 3.25,3.25,3.25
	return [a] + [b[1]] + [b[2]] + [c] + [d[1]] + [d[2]]
end

local test = require("test")

test.eq(foo(),12.75)