import "lib/examplecompiler"
foo = {}


local def foo2(b : eg.number) : eg.number return 2 + b end


assert(foo2(3) == 5)

def foo.bar(a : eg.number) : eg.number 
	return 1 + -2 * 3 + 1 + a - foo2(a) 
end

assert(-6 == foo.bar(4))

local c = def(c : eg.number) : eg.number return 4 end

assert(4 == c(1))

def sign(a : eg.number) : eg.number
	if a < 0 then
		return -1
	else
		return 1
	end
end

def usev(a : eg.number) : eg.number
	var b = 1
	return a + b
end

def what(a : eg.number) : eg.number
	var c = usev
	return c(a)
end

assert(usev(4) == 5)
assert(sign(4) == 1)
assert(sign(-3) == -1)
assert(what(4) == 5)