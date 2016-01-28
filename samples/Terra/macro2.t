
local bar2 = macro(function(typ)
	return typ
	
end)

terra foo() : int
	var a = 3
	bar2(a) = bar2(a) + 5
	return a
end

local test = require("test")
test.eq(8,foo())
