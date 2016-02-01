
terra foo()
	return 1,2
end

local q = `foo()

terra bar()
	return q
end

local test = require("test")
test.meq({1,2},bar())