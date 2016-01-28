

--var a = 4 + 5
a = global(4 + 5)
terra foo()
	return a
end


foo:gettype()
--we currently don't track what initializers need to be run after a nojit compile
--hopefully we can just remove the need to have nojit entirely
--otherwise we need to seperate the calling of variable initializers from the compilation process
--so that they can be called when jit is invoked
a:gettype()

local test = require("test")
test.eq(foo(),9)


