

twice = macro(function(a)
	return quote
		var v = a
	in
		v + v
	end
end)


terra foobar()
	var what = 1
	return twice([quote what = what + 1 in what end]) + what
end

assert(6 == foobar())