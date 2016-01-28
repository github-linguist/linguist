struct A { a : int }

A.methods.up = function(self,b)
	self.a = self.a + 1 + (b or 0)
end

terra foo()
	var a = A { 1 }
	a:up()
	var b = &a
	b:up(4)
	return a.a
end

local test = require("test")
test.eq(foo(),7)