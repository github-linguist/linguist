
local begin = symbol()
local theend = symbol()

terra foo()
	var a = 0
	::[begin]::
	if a >= 10 then
		goto [theend]
	end
	a = a + 1
	goto [begin]
	::[theend]::
	return a
end

test = require("test")
test.eq(foo(),10)