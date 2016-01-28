
terra foo()
	var a = array(1,2,3)
	return a[0] + a[1] + a[2]
end

terra foo2()
	var a = array(1,2.5,3)
	return a[1]
end
terra what()
	return 3,4.5
end
local expand = macro(function(a) return {`a._0,`a._1} end)
terra foo3()
	var w = what()
	var a = array(1,2.5,expand(w))
	return a[3]
end
terra foo4()
    var a = array("what","magic","is","this")
    return a[1][1]
end

local test = require("test")
test.eq(foo(),6)
test.eq(foo2(),2.5)
test.eq(foo3(),4.5)
test.eq(foo4(),97)