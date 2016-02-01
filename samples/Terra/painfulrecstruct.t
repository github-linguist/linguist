

struct A {
	a : A -> int;
	b : int
}

terra foo(a : A)
	return a.b
end

terra callit(a : A)
	return a.a(a)
end

terra bar()
	var a = A { foo, 3 }
	return callit(a)
end

local test = require("test")
test.eq(bar(),3)