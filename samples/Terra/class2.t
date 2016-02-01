local Interface = require("lib/golike")

local I = Interface.create {
	get = {} -> int;
	set = int -> {};
}

struct A {
	data : int
}

terra A:get()
	return self.data
end

terra A:set(a : int)
	self.data = a
end

struct B {
	data : int
}

terra B:get()
	return self.data + 1
end

terra B:set(a : int)
	self.data = self.data + a
end


terra anInterface(a : I)
	a:set(3)
	return a:get()
end

terra foo()
	var a = A { 0 }
	var b = B { 2 }
	return anInterface(&a) + anInterface(&b) 
end

local test = require("test")
test.eq(foo(),9)