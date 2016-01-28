

terra what()
	return " " / 1
end

dostuff = macro(function()
	pcall(what.compile,what)
	return 4
end)

terra foobar()
	return dostuff()
end
assert(4 == foobar())