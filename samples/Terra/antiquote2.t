

function omgfunc()
	return 4
end

local what = `[ {2,3} ]
terra foo()
	return [{3,`[ {2,3} ]}]
end

local test = require("test")
test.meq({3,2,3},foo())
