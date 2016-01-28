terra foobar()
    var a = { a=3,b=4}
    var b = {a=5,b=6.0}
    b = a
	return a.a
end

local test = require("test")
test.eq(foobar(),3)