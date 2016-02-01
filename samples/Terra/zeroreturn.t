

local b = global(0)

terra foo(a : int)
	if a > 3 then
		return
	end
	b = a
end

terra getb() return b end

local test = require("test")
foo(4)
test.eq(getb(),0)
foo(2)
test.eq(getb(),2)