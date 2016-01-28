
terra foo(a : int)
	return 1
end

terra foo(a : double)
	return 2
end

terra doit()
	return foo(2.5)
end

local test = require("test")
test.eq(doit(),2)