
struct A {
	a : int
}


print(A:cstring())

terra foo()
	return A { 1 }
end

struct B {
	a : int
}

terra foo2()
	return B { 1 }
end

assert(foo().a == foo2().a)