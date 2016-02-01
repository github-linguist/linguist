
struct A { a : int }
terra A:foo(a : int)
	return self.a + a
end

terra A:foo(a : &int8) 
	return self.a
end


terra doit()
	var a = A { 3 }
	return a:foo(1) + a:foo("what")
end
terra doit2()
	var a = A { 3 }
	var pa = &a
	return pa:foo(1) + pa:foo("what")
end

local test = require("test")
test.eq(doit(),7)
test.eq(doit2(),7)