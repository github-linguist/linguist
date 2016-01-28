

local a = global(double)

terra getptr()
	return &a
end

local b = getptr()
local c = terralib.pointertolightuserdata(b)
print(b,c,a)

terra foo(a : &&int)
	
end

foo(c)
print("DONE")