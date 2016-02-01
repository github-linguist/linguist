
IO = terralib.includec("stdio.h")
local Class = require("lib/javalike")

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

terra returnA(a : A)
    return a
end
terra foobar1()
  var c = C.alloc()
  c.a,c.b,c.c = 1,2,3.5
  return c:times2()
end

assert(foobar1() == 4)

terra foobar()

    var a = A.alloc()
    a.a = 1
    var b = B.alloc()
    b.a,b.b = 1,2

    var c = C.alloc()
    c.a,c.b,c.c = 1,2,3.5

    var r = doubleAnA(a) + doubleAnA(b) + doubleAnA(c) + combineAB(b) + combineAB(c)

    a:free()
    b:free()
    c:free()

    return r
end

assert(23 == foobar())

Doubles = Class.interface { times2 = {} -> int } 

Adds = Class.interface { add = int -> int }

struct D {
  data : int
}
Class.implements(D,Doubles)
Class.implements(D,Adds)


terra D:times2() : int
    return self.data * 2
end

terra D:add(a : int) : int
    return self.data + a
end


terra aDoubles(a : &Doubles)
    return a:times2()
end

terra aAdds(a : &Adds)
    return a:add(3)
end

terra foobar2()
    var a : D
    a:init()
    a.data = 3
    return aDoubles(&a) + aAdds(&a)
end

assert(12 == foobar2())


local IO = terralib.includec("stdio.h")
struct Animal {
  data : int
}
terra Animal:speak() : {}
    IO.printf("... %d\n",self.data)
end

struct Dog {
}
Class.extends(Dog,Animal)
terra Dog:speak() : {}
    IO.printf("woof! %d\n",self.data)
end

struct Cat {
}

Class.extends(Cat,Animal)

terra Cat:speak() : {}
    IO.printf("meow! %d\n",self.data)
end

terra dospeak(a : &Animal)
    a:speak()
end

terra barnyard()
    var c : Cat
    var d : Dog
    c:init()
    d:init()
    c.data,d.data = 0,1

    dospeak(&c)
    dospeak(&d)
end
barnyard()


local Add = Class.interface { add = int -> int }

local Sub = Class.interface { sub = int -> int }

local struct P {
   data : int
}
Class.implements(P,Add)

local struct C {
  data2 : int
}
Class.extends(C,P)
Class.implements(C,Sub)

terra P:add(b : int) : int
   self.data = self.data + b
   return self.data
end

terra C:sub(b : int) : int
    return self.data2 - b
end

terra doadd(a : &Add)
    return a:add(1)
end

terra dopstuff(p : &P)
    return p:add(2) + doadd(p) 
end

terra dosubstuff(s : &Sub)
    return s:sub(1)
end

terra dotests()
    var p : P
    p:init()
    var c : C
    c:init()
    p.data = 1
    c.data = 1
    c.data2 = 2
    return dopstuff(&p) + dopstuff(&c) + dosubstuff(&c)
end

assert(dotests() == 15)

terra timeadd(a :&P, N : int)
  IO.printf("%p\n",a)
  for i = 0, N,10 do
    a:add(1)
    a:add(1)
    a:add(1)
    a:add(1)
    a:add(1)
    a:add(1)
    a:add(1)
    a:add(1)
    a:add(1)
    a:add(1)
  end
  return a
end

local a = global(C)

terra doinit() : &P
  a:init()
  a.data = 0
  return &a
end

local v = doinit()
timeadd:compile()

local b = terralib.currenttimeinseconds()
--timeadd(v,100000000)
local e = terralib.currenttimeinseconds()
print(e - b)
print(v.data)

