
local dd = 8
terra nested3()
	return [ `dd ]
end
local test = require("test")
test.eq(nested3(),8)