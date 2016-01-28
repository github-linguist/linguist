

terra foo()
	var a = 4
	return [a]
end

foo:printpretty()

local test = require("test")
test.eq(foo(),4)