
terra foo()
    var a : int64 = 256+3
    var c = intptr(&a)
    var d = [&int64](c)
    var fi = int(true)
    var what = @[&int8](&a)
    return @d,fi,what
end
local test = require("test")
test.meq({256+3,1,3},foo())