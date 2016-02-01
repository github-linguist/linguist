

terra foobar()
	var a = 4
	@&[quote in a end] = 5
	return a
end

assert(foobar() == 5)