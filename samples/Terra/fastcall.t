
terra foo()
	return 1
end


assert(1 == foo())
assert(rawget(foo,"fastcall") == foo:getdefinitions()[1].ffiwrapper)
assert(1 == foo())


terra foo2()
	return 1,2
end
local a,b = terralib.unpackstruct(foo2())
assert(a == 1 and b == 2)
assert(rawget(foo2,"fastcall") == foo2:getpointer())
local a,b = unpackstruct(foo2())
assert(a == 1 and b == 2)

terra foo(a : int)
end

assert(rawget(foo,"fastcall") == nil)