


function addone(a)
	return a + 1
end


local a1 = terralib.cast(int -> int, addone)

terra dofn(a : int -> int)
	return a(3)
end
local test = require("test")
test.eq(dofn(a1),4)