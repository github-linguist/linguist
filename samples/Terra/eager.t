

local fn = terra() return 0 end

for i = 1,10 do
	fn = terra() return fn() + 1 end
end

local test = require("test")
test.eq(fn(),10)