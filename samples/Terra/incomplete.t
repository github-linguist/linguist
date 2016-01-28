

struct A

terra foo(a : &A)
	return a
end

assert(nil == foo(nil))
assert(false == A:iscomplete())


struct A {
	b : int
}

terra foo2(a : &A)
	a.b = 6
	return @a
end

foo2:compile()
assert(true == A:iscomplete())

local ptr = terralib.new(A)

ptr.b = 4

local r = foo2(ptr)

assert(r.b == 6)
assert(ptr.b == 6)