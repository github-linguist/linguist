

terra foo()
	return (1),(2)
end

terra bar()
	var c = [quote var a = 3 in a end]
	var d = 5
	c = c + 10
	return [quote var a = 3 in [quote var b = 4 in a + b + c + d end] end]
end
local test = require("test")
test.meq({1,2},foo())
assert(25 == bar())
