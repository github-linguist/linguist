
terra foo()
	var a : &int = nil
	var b = nil
	return a == b
end

local test = require("test")
test.eq(foo(),true)