

struct A {
	a : int
}

A.metamethods.__methodmissing = macro(function(methodname,obj,anarg)
	print(methodname)
	return `anarg + [string.byte(methodname,1,1)]
end)

terra foobar()
	var a : A
	return a:a(3) + a:b(4)
end

assert(foobar() == 202)