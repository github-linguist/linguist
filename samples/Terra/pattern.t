
terra foobar()
    return 1,2
end

terra what()
    var _,a,b = 1,foobar()
    a,b = foobar()
    return a + b
end
terra what2()
    var a = foobar()
    var b,c = unpackstruct(a)
    return b+c
end

assert(what() == 3)
assert(what2() == 3)


struct A {
    a : int
    b : int
}

terra what3() : A
    var a = A {1,2}
    return unpacktuple(a)
end
assert(what3().b == 2)

terra what4()
    var a = A {1,2}
    var c,d = unpackstruct(a)
    return c+d
end
assert(what4() == 3)

local terra ra() return A {1,2} end
local a,b = unpackstruct(foobar())
assert(a == 1 and b == 2)
assert(unpacktuple(ra()).a == 1)
assert(unpackstruct(ra()) == 1)