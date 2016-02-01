
local a = terralib.global(int)
local b = terralib.global(5)
local c = terralib.global(int,3)
terra foo()
	a = 4
end


terra bar()
	return c + a + b
end

foo()
assert(bar() == 12)

b:set(4)

assert(bar() == 11)