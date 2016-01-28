

terra foo(a : int)
	return a
end

terra foo(a : int, b : int)
	return a + b
end

local test = require("test")
test.eq(foo(1) + foo(3,4), 8)