struct A { a : int }

terra A:foo() 
    self.a = self.a + 1
    return self.a
end

terra A.methods.foo2(self : A) 
    self.a = self.a + 1
    return self.a
end


mya = global(A)
mya:set({0})

terra geta()
    return mya
end

terra bar()
    var v0 = mya.a
    var v1 = geta():foo()
    var v2 = mya.a
    var v3 = geta():foo2()
    var v4 = mya.a
    return v0,v1,v2,v3,v4
end
test = require("test")
test.meq({0,1,0,1,0},bar())