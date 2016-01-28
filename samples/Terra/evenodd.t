terra iseven(a : uint) : bool
	if a == 0 then
		return true
	else
		return isodd(a - 1)
	end
end and
terra isodd(a : uint) : bool
	if a == 0 then
		return false
	else
		return iseven(a - 1)
	end
end

local test = require("test")
test.eq(iseven(3),false)
test.eq(iseven(2),true)
test.eq(isodd(3),true)
test.eq(isodd(2),false)
