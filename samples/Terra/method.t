--the zero line
struct A { b : B } and
struct B {a : int, b : int}

B.methods.foo = terra(b : B)
    return b.a
end
terra bar()
    var b = B { 1,2 }
    return b:foo()
end


B.methods.foo2 = terra(b : &B)
    b.a = 6
end

terra bar2()
    var b = B { 1,2 }
    b:foo2()
    return b.a
end

B.methods.foo3 = terra(b : B)
    return b.a
end

terra bar3()
    var b = B { 1,2 }
    return (&b):foo3()
end
terra bar4()
    var b = B { 1,2 }
    (&b):foo2()
    return b.a
end

test = require("test")
test.eq(bar(),1)
test.eq(bar2(),6)
test.eq(bar3(),1)
test.eq(bar4(),6)