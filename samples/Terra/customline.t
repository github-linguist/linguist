terra foo(c : int, b : int)
    terralib.debuginfo("../src/terralib.lua",300)
    var a = b + c
    terralib.debuginfo("../src/terralib.lua",301)
    return a + a
end

foo(3,4)

--terralib.dumpmodule()