
IO = terralib.includec("stdio.h")
local Class = require("lib/javalikesimple")

struct A {
  a : int
}
terra A:times2() : int
    return self.a*2
end
   
struct B {
  b : int
} 
Class.extends(B,A)
    
terra B:combine(a : int) : int
    return self.b + self.a + a
end
    
struct C {
  c : double
}
Class.extends(C,B)

terra C:combine(a : int) : int
    return self.c + self.a + self.b + a
end
terra C:times2() : double
    return self.a * 4
end

terra doubleAnA(a : &A)
    return a:times2()
end

terra combineAB(b : &B)
    return b:combine(3)
end

terra foobar1()
  var c : C
  c:init()
  c.a,c.b,c.c = 1,2,3.5
  return c:times2()
end

assert(foobar1() == 4)

terra foobar()

    var a : A, b : B, c : C
    a:init(); b:init(); c:init()

    a.a = 1
    b.a,b.b = 1,2
	c.a,c.b,c.c = 1,2,3.5

    var r = b:times2() + doubleAnA(&a) + doubleAnA(&b) + doubleAnA(&c) + combineAB(&b) + combineAB(&c)

    return r
end


assert(25 == foobar())

