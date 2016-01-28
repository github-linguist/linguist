--the zero line
struct A { b : B } and
struct B {a : int, b : int}

terra bar(a : B)
    a.a = a.a + 1
    return a,3
end

terra foo()
    var a : B
    a.a = 4;
    --(bar(a)).a = 3, TODO: why is the offset == 0 for this value?
    var c,d = bar(a)
    return c.a + a.a + d
end


terra baz(a : &B)
    a.a = 1
    a.b = 2
    return a.a
end


terra foo2()
    var a : B
    var d = baz(&a)
    return a.a + a.b + d
end
local test = require("test")

test.eq(foo(),12)
test.eq(foo2(),4)


local C = tuple(int, int)

local D = tuple(int, int)

terra anon()
    var c : C
    c._0 = 3
    c._1 = 4
    var d : D = c
    --var b : B = d
    return d._0 + d._1
end

test.eq(anon(),7)