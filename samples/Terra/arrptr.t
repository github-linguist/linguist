a = &int[4]
a = (&int)[4]
local test = require("test")
test.eq(false,a:ispointer())
test.eq(true,a:isarray())