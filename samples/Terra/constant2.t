

local a = terralib.new(int,3)
local b = terralib.new(int8,4)
local c = terralib.new(int64,5)
local d = terralib.new(float,3.25)
local e = terralib.new(double,4.25)

f = global(4)
terra foo()
	return &f
end

local pf = foo()

terra bar()
	return a + b + c + d + e + @pf
end

local test = require("test")
test.eq(bar(),23.5)