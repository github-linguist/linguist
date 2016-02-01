
terra foo(a : {int} )
	return 1
end

terra foo(a : {int,int} )
	return 2
end

terra doit()
	return foo({1,2}) + foo({1,2})
end

local test = require("test")
test.eq(doit(),4)