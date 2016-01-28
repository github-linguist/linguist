
IO = terralib.includec("stdio.h")
local Class = require("lib/javalike")

struct A {
	a : int
}

terra A:foo(a : int) : int
	return self.a + a
end

local m = A.methods.foo:getdefinitions()[1]

HasFoo = Class.interface({ foo = int -> int })

Class.implements(A,HasFoo)

terra hasfoo(a : &HasFoo)
	return a:foo(3)
end

terra testit()
	var a = A.alloc()
	a.a = 4
	return hasfoo(a) + a:foo(5)
end

print(m:getpointer())
testit:compile()
print("DONE")

assert(testit() == 16)