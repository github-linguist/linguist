

C = terralib.includecstring [[
	_Bool And(_Bool a, _Bool b) { return a && b; }
]]


terra foobar(a : bool, b : bool)
	return C.And(a,b)
end

assert(foobar(false,true) == false)
assert(foobar(true,true) == true)