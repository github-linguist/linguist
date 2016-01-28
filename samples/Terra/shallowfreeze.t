

struct B {
	d : int
}
struct A {
	b : &B;
	c : int;
	d : int;
	e : int;
}


terra foo() : A
	return A {nil}
end

foo:disas()

print(foo())



terralib.dumpmodule()