
terra foo(a : int)
	return terralib.select(a > 0, 1,-1)
end


terra foo2(a : int)
	var c = vector(a > 0, a > 1, a > 2)
	var d = terralib.select(c,vector(1,2,3),vector(4,5,6))
	return d[0], d[1], d[2]
end
local test = require("test")

test.eq(foo(1),1)
test.eq(foo(-1),-1)

test.meq({1,5,6},foo2(1))