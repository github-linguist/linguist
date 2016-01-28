

local struct A { b : &B }
  and struct B { a : &A }

struct C { a : &A, b : &B, c : &C  }

local struct D {}

terra foo()
	var a : A, b : B, c : C
	a.b = &b
	b.a = &a
	c.c = &c
end

foo()