

terra foo()
	var a : int[5]
	a[2] = 2
	var b = [int64](2)
	return a[b]
end

assert(foo() == 2)