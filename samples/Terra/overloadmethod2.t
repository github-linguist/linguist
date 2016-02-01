
struct A { a : int }
A.methods.foo = terra(self : A, a : int)
	return self.a + a
end

terra A:foo() 
	return self.a
end


terra doit()
	var a = A { 3 }
	return a:foo() + a:foo(1)
end
local test = require("test")
test.eq(doit(),7)