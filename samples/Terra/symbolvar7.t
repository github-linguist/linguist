local a = symbol()
local b = { symbol(int), symbol(int) }

local c = symbol(int)
local d = symbol(double)

terra foo()
	var [a] : double,[b] = 1.25,1.25,1.25
	var [c] : double,[d] : int = 3.25,3.25
	return [a] + [b[1]] + [b[2]] + [c] + [d] 
end

local test = require("test")

test.eq(foo(),9.50)