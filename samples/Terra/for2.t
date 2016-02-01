
terra foo(a : uint64, s : uint64)
    var r = 0
    for i : uint64 = 0,a,s do
        r = r + i
    end
    return r
end

terra foo2(a : int)
    var r = 0
    for i : int = 0.f,a do
        r = r + i
    end
    return r
end
terra foo3(a : int)
    var r = 0
    for i = a,0,-1 do
        r = r + i
    end
    return r
end


local test = require("test")
test.eq(foo(10,1),45)
test.eq(foo(10,2),20)
test.eq(foo(0,1),0)
test.eq(foo2(10),45)
test.eq(foo3(10),55)
