

local a = 
quote
	var b = 1
in
	b + 0, b + 1
end

terra f0()
	return (a)
end
terra f1()
	return a
end
terra f2()
	a
end
local test = require("test")
test.meq({1,2},f0())
test.meq({1,2},f1())

local c = symbol()
local b = 
quote
	var [c]  = 3
in
	c,c+1
end

terra f3()
	b
	return ([c])
end
assert(f3() == 3)

a = global(1)
local emptyexp = quote
	a = a + 1
end

local emptystmts = {4,3}

local both = quote
in 4,[quote a = a + 1 in 3 end]
end

terra bar(a : int,  b : int)
	return a + b
end
terra bar2(a : int,  b : int, c : {})
	return a + b
end
terra f4()
	return bar(emptystmts) + bar2(1,2,emptyexp) + a
end

assert(f4() == 12)

terra f5()
	return bar(both._0,both._0) + a
end

assert(f5() == 12)