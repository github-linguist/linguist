

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
	return a.b
end

foo2:compile()
assert(true == A:iscomplete())
