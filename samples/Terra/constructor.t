
local b = 1
local dd = "d"
local c = symbol()

terra foo()
	var a = { _0 = [b], [c] = 2, [dd] = 3, r = 4}
	return a._0 + a.[c] + a.d + a.r
end

local test = require("test")
test.eq(foo(),10)