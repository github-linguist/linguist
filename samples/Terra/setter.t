
struct A {
a : int;
b : int;
}

terra A.metamethods.__update(self : &A, a : int, b : int)
    self.a = a
    self.b = b
end
terra A.metamethods.__apply(self : &A, a : int)
    return a + self.b
end

struct B {
    a : int
}


B.metamethods.__update = macro(function(me,arg,arg2,rhs)
    return quote me.a = arg + arg2 + rhs end
end)

B.metamethods.__setentry = macro(function(field,self,rhs)
    field = field:sub(2)
    return quote self.[field] = self.[field] + rhs end
end)

terra foo()
    var a : A
    a(4) = 5
    var b : B
    b(3,4) = 5
    b._a = 1
    return a.a + a.b + a(3) + b.a
end

--foo:printpretty()
assert(foo() == 9+8+3+4+5+1)
