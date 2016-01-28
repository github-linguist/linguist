--the zero line
struct A { b : B } and
struct B {a : int, b : int}


terra foo()
	var b = B {1,2}
	return b
end

terra bar()
	return foo().b
end

local test = require("test")
test.eq(bar(),2)