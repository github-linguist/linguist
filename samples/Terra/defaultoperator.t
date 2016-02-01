
local a = "+"
local b = "__sub"
local ops =  {4,5}
terra foobar()
	return operator(b,operator(a,3,operator("+",ops)))
end

assert(foobar() == -12)
