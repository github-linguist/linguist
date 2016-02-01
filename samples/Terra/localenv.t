
local a = 4
b = 3
local terra foo()
	return a + b
end
local terra bar()
	return foo()
end
local test = require("test")

test.eq(foo(),7)
test.eq(bar(),7)

terra nested()
	return [ (function(a) return a + b end)(8) ]
end
terra nested2()
	return [ a + b ]
end
local c = {}
local d = {}
local vv = {}
local b = 3
local e = 8
g = 1
local dd = 8
 aa = global(10)
terra nested3()
	var luav = 8
	return [ (function()
		local b = 5
		function c.b() return 7 end
		local f = 7
		vv.bb = global(aa:get())
		terra d.c() return e + f + g + vv.bb + [ `dd ] end
		return b + a
	end)() ]
end

test.eq(nested(),11)

test.eq(nested2(),7)
test.eq(nested3(),9)
test.eq(c.b(),7)
test.eq(d.c(),34)