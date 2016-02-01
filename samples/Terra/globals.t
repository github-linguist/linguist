
struct A { a : int }
local a = terralib.global(A)
a:set({3})
local b = terralib.global(true)
local c = terralib.global(int[3])
local d = terralib.global(3.5)

terra foobar()
	c[0] = 3
	return a.a + d
end

e = global(int)
terra doihaveaname()
	e = 10
	return e
end

assert(doihaveaname() == 10)
assert(6.5 == foobar())
print("HERE")
assert(true == b:get())
assert(3 == c:get()[0])

print("TODO - fix handling of initializers for array-like things and literal strings")