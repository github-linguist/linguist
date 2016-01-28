

foo = macro(function()
	local terra bar()
		return 3
	end
	return bar
end)

terra baz()
	return foo()()
end

assert(baz() == 3)