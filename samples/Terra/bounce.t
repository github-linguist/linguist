local io = terralib.includec("stdio.h")

struct Count { value : int }
function luafn(a)
	print("lua:",a.value)
	a.value = a.value + 1
	terrafn(a)
end
terra terrafn(a : &Count)
	io.printf("terra: %d\n",a.value)
	if a.value < 100 then
		luafn(a)
	end
	return a.value
end

terra begin()
	var c = Count {0}
	return terrafn(&c)
end

local test = require("test")
test.eq(begin(),100)