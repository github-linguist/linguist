

local a = symbol()

local q = quote 
	var [a] = 2
	[a] = [a] + 1
end

terra foo()
	q
	return [a]
end

local test = require("test")
test.eq(3,foo())