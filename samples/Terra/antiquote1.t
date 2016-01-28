

function omgfunc()
	return 2
end

terra foo()
	return 1 + [{omgfunc()}] + [omgfunc()]
end

local test = require("test")
test.eq(foo(),5)