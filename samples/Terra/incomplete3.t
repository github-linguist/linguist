
struct A {
	b : B -> C
} and
struct B and struct C


terra foo()
	var a : A
	a.b = nil
	return a
end

local a = foo()


struct B {
	a : int
}
struct C {
	b : int
}

terra foo2(a : &A)
	var ptrb : &B
	var c = a.b(@ptrb)
end

foo2:compile()