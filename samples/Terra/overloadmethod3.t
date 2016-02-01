
struct A { a : int }
A.methods.foo = terra(self : A)
	return 2
end

terra A:foo() 
	return 1
end


terra doit()
	var a = A { 3 }
	var pa = &a
	return a:foo() + pa:foo()
end
local test = require("test")
test.eq(doit(),4)