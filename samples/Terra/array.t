
terra bar()
	var a : int[2]
	a[0] = 1
	a[1] = 2
	return a
end

terra foo()
	var a : int[4]
	a[1], a[2] = 4,2
	return a[1] + a[2]
end

terra foo2()
	var b = bar()
	return b[0] + b[1]
end

terra foo3()
	return (bar())[0]
end

terra foo4()
	var a : int[4]
	a[3] = 7
	var b = &a[0]
	b[2] = 8
	return b[2] + b[3]
end

terra bar2(a : &int)
    a[1] = 100
    return a[0]
end

terra foo5()
    var a : int[4]
    bar2(a)
    return a[1]
end

terra foo6()
    return bar2(bar())
end


terra foo7()
    var a = array(1,2,3,4)
    return a[1]+a[2]
end
local test = require("test")

test.eq(6,foo())
test.eq(3,foo2())
test.eq(1,foo3())
test.eq(15,foo4())
test.eq(100,foo5())
test.eq(1,foo6())
test.eq(5,foo7())