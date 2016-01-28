

a = {}
a.b, a.c = global(3),global(double,4)
d = global(5)
--print(a.b)

--a.b:gettype(nil)

terra bar()
    a.b = a.b + 1
    a.c = a.c + 1
    d = d + 1
end
terra foo()
    return a.b,a.c,d
end


local test = require("test")

bar()
test.meq({4,5,6},foo())