
struct A {
	a : int
}

terra foo(a : &A)
	return a
end

print(foo(terralib.new(A,{3})))
