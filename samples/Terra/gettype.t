


function makestuff(a)
	local T = a:gettype()
	local struct ST {
		a : T
	}
	return `ST { a }
end

makestuff = macro(makestuff)

terra useit()
	return makestuff(true).a,makestuff(3.0).a
end
local test = require("test")
test.meq({true,3},useit())