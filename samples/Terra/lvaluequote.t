

checkl = macro(function(a)
	assert(a:islvalue())
	return a
end)

checkr = macro(function(a)
	assert(not a:islvalue())
	return a
end)


terra testit()

	var a = checkr(4)
	var b = checkr(checkl(a) + 3)
	var c = checkl([quote var d = 5 in d end])
	return a + b + c
end

assert(4+7+5 == testit())
