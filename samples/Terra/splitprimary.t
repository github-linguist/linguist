r = 1
local function foo(a)
	r = a
	return r
end
local b = foo(1)
(foo)(3)
assert(b + r == 4)

terra testescapes()
	var a  = [4]
	[ quote 
		a = a + 1
	  end
	]
	return [ `&a ][0]
end

assert(testescapes() == 5)