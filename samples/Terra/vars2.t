
a = global(int) --testing variable with no initializer


terra foo()
	a = 3
end

terra bar()
	return a
end


local test = require("test")

foo()
test.eq(bar(),3)