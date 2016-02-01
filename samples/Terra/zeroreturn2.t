
a = global(0)

terra doit()
	a = a + 1
end
terra geta()
	return a
end

terra bar()
	return doit()
end

bar()
local test = require("test")
test.eq(geta(),1)