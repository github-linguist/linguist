local c = terralib.includecstring [[
	#include <stdlib.h>
	#include <stdio.h>
]]


struct Node {
	next : &Node;
	v : int;
}

terra foo()
	var cur : &Node = [&Node](c.malloc(sizeof(Node)))
	cur.v = 3
	return cur.v
end

local test = require("test")
test.eq(foo(),3)
