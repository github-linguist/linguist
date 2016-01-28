struct A { a : int, b : double }

terra foo(a : A)
	return a.a + a.b
end


terra foo2(a : A)
	return a.a + a.b,a.a
end

local test = require("test")

test.eq( foo( {a = 1,b = 2.3} ), 3.3 )
test.eq( foo( {1,2.3} ), 3.3 )
test.eq( foo( {b = 1, a = 2.3} ),3 )

test.meq({3.3,1},foo2( { a = 1, b = 2.3} ))