
terra foo()
	var a = 2* vector(1,2,3)
	return a[0] + a[1] + a[2]
end

terra foo2()
	var a = vector(1,2.5,3)
	return a[1]
end
terra what()
	return 3,4.5
end
expwhat = macro(function(a) return {`a._0, `a._1} end)
terra foo3()
	var a = vector(1,2.5,expwhat(what()))
	return a[3]
end

terra foo4()
	var a = vectorof(int,1,2.5,3)
	return a[1]
end

local test = require("test")
test.eq(foo(),12)
test.eq(foo2(),2.5)
test.eq(foo3(),4.5)
test.eq(foo4(),2)