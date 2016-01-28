

function omgfunc()
	return 2
end

a = global(0)

terra foo()
	[quote a = a + 1 end];
	[{quote a = a + 1 end,quote a = a + 1 end}]
	return a
end

local test = require("test")
test.eq(foo(),3)