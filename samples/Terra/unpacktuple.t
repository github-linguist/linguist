

terra foo()
 return 1,2
end

terra foo2()
 return {a = 1, b = 2}
end

assert(unpacktuple(1) == 1)
assert(unpacktuple(foo()) == 1)

assert(unpacktuple(foo(),2) == 2)

assert(unpacktuple(foo(),nil,1) == 1)
assert(unpacktuple(foo2()).a == 1)

assert(2 == #{unpacktuple(foo())})

assert(1 == #{unpacktuple(foo(),2)})

terra usefoo()
    var a = foo()
    var c,d = unpacktuple(a)
    var e = unpacktuple(a,2)
    return e
end

assert(usefoo() == 2)