import "lib/def"

local c = 3
local a = 4
local b = def(a) a + c
local d = b(10)

def e(a) a + c
local def f(a) a + c

print(d, e(10), f(10)) 

local test = require("test")
test.eq(d,13)
test.eq(e(10),13)
test.eq(f(10),13)
