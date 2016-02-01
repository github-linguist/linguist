C = terralib.includec("stdio.h")
--the zero line
struct A { a : int }

terra A:foo1()
    self.a = self.a + 1
    return self.a
end

terra A.methods.foo2(self : &A)
    self.a = self.a + 1
    return self.a
end

terra A.methods.foo3(self : A)
    self.a = self.a + 1
    return self.a
end

terra bar()
    var a  = A { 0 }
    var ptra = &a
    
    var v0 = a.a
    var v1 = a:foo1()
    var v2 = a.a
    var v3 = a:foo2()
    var v4 = a.a
    var v5 = a:foo3()
    var v6 = a.a
    
    ptra.a = 0
    var p0 = ptra.a
    var p1 = ptra:foo1()
    var p2 = ptra.a
    var p3 = ptra:foo2()
    var p4 = ptra.a
    var p5 = ptra:foo3()
    var p6 = ptra.a
    
    return v0,v1,v2,v3,v4,v5,v6,p0,p1,p2,p3,p4,p5,p6
end

test = require("test")
test.meq({0,1,1,2,2,3,2,0,1,1,2,2,3,2},bar())