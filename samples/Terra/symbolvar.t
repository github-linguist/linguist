

local a = symbol()

terra foo()
	var [a] = 3
	return [a]
end

local test = require("test")
test.eq(3,foo())