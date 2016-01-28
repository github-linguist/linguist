

local foo = terralib.constant(terralib.new(int[4],{1,2,3,4}))

struct A {
	a : int;
	b : float
}

local mystr = terralib.new(A,{3,4.5})
local const = constant(mystr)


terra bar()
	return foo[3] + mystr.a
end

terra bar2()
	return foo[1] + mystr.b
end

function wrapper(a)
	return a + 1
end

local p1 = terralib.constant(int -> int, wrapper)

terra doit()
	return p1(3)
end

local test = require("test")
test.eq(bar(),7)
test.eq(bar2(),6.5)
test.eq(doit(),4)
