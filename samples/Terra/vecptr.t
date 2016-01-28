terra foo(a : &float)
	var b = [&vector(float,4)](a)
end
foo:compile()