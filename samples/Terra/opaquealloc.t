


local r,e = pcall(function()
	local struct A {
		a : opaque
	}
	local v = terralib.new(A)
end)

assert(not r and e:match("Errors reported during"))


local r,e = pcall(function()
	local v = terralib.new(opaque)
end)


assert(not r and e:match("attempting to use an opaque type"))