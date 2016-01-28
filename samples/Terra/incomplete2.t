
struct A {
	b : &B
} and
struct B


terra foo()
	var a : A
	a.b = nil
	return a
end

local a = foo()


struct B {
	a : int
}

terra foo2(a : &A)
	a.b = a.b + 1
end

foo2(a)
