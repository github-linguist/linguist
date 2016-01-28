struct A { a : int, b : int}

terra foobar()
	var a = A { 3, 4}
	return a.b
end

local test = require("test")
test.eq(foobar(),4)