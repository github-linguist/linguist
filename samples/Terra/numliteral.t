
terra thetest()
	var a,b,c,d,e = 3,3.0,3.f,3LL, 3ULL
	return a,b,c,d,e
end

local exp = { "int32", "double", "float", "int64", "uint64" }

local test = require("test")
thetest:compile()
local typ = thetest.definitions[1]:gettype()
for i,e in ipairs(typ.returntype:getentries()) do
	test.eq(tostring(e.type),exp[i]) 
end