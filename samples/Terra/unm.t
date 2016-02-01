

struct A {
	a : int
}

A.metamethods.__unm = terra(self : &A)
	return A { -self.a }
end

A.metamethods.__sub = terra(self : &A, rhs  : &A)
	return A { self.a - rhs.a }
end

terra doit()
	var a,b = A { 1 } ,  A { 2 }
	return (-(a - b)).a
end

assert(doit() == 1)