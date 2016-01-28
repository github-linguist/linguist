

A = terralib.new(int[4],{1,2,3,4})

terra foo(a : &int)
	var sum = 0
	for i = 0,4 do
		sum = sum + a[i]
	end
	return sum,a
end

local test = require("test")

local a,b = terralib.unpackstruct(foo(A))

test.eq(a,10)
test.eq(terralib.typeof(A),int[4])
test.eq(terralib.typeof(b),&int)