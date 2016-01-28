local c = terralib.includecstring [[
	#include <stdlib.h>
	#include <stdio.h>
]]

local N = 10
terra foo()
	c.printf("size = %d\n",int(sizeof(int)))
	var list = [&int](c.malloc(sizeof(int)*N))
	for i = 0,N do
		list[i] = i + 1
	end
	var result = 0
	for i = 0,N do
		c.printf("%d = %d\n",int(i),list[i])
		result = result + list[i]
	end
	c.free([&uint8](list))
	return result
end

local test = require("test")
test.eq(foo(),N*(1 + N)/2)
