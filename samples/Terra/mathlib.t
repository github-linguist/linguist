local c = terralib.includec("math.h")

terra mysqrt(a : float)
	return c.sqrtf(a)
end

local test = require("test")
test.eq(mysqrt(4),2)
test.eq(mysqrt(9),3)