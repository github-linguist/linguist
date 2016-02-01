
local a = symbol(int)
local c = {}
terra foo([a], b : int, [c])
	return [a] + b
end
local d = {symbol(int),symbol(int)}
terra foo2([a], b : int, [d])
	return [a] + b + [d[1]] + [d[2]]
end

local test = require("test")

test.eq(foo2(1,2,3,4),10)
test.eq(foo(1,2),3)