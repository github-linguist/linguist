

local a = symbol()

local q = quote 
	[a] = [a] + 1
end

terra foo()
	var [a] = 2
	q
	return [a]
end

local test = require("test")
test.eq(3,foo())