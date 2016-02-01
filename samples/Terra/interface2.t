
IO = terralib.includec("stdio.h")
local Class = require("lib/javalike")

HasFoo = Class.interface({ foo = int -> int })

struct A {
	a : int
}

terra A:foo(a : int) : int
	return 1
end
Class.implements(A,HasFoo)

struct B {}
Class.extends(B,A)


struct C {}
Class.extends(C,B)

terra C:foo(a : int) : int
	return 3
end

terra hasfoo(a : &HasFoo)
	return a:foo(3)
end

terra testit()
	var a = A.alloc()
	var b = B.alloc()
	var c = C.alloc()
	return hasfoo(a) + hasfoo(b) + hasfoo(c)
end

assert(testit() == 5)