--quotes, unlike functions bind symbols eagerly
--function have :compile() to bind their symbols, but no similar thing exists
--for quotes, making it hard to control
--furthermore there aren't many use cases for late-binding quotes,
--but a bunch exist for needing early-bound quotes, like below:


function times5(x)
	local c = `0
	for i = 1,5 do
		c = `c + x
	end
	return c
end
times5 = macro(times5)

terra foo()
	var a  =  3
	return times5(a)
end

foo:printpretty()
foo:disas()
local test = require("test")
test.eq(foo(),15)
