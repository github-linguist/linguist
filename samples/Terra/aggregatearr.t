

a = terralib.new(int[4])

terra foo()
	a[3] = 4
end

foo()
assert(4 == a[3])


terra bar()
	a = array(5,6,7,8)
end

bar()
assert(a[3] == 8)
