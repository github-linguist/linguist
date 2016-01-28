
C = terralib.includecstring [[
	struct Foo {
		int (*bar)(int);
	};
	int runptr(int (*bar)(int), int a) {
		return bar(a);
	}
]]

terra add1(a : int) return a + 1 end

terra what0()
	return C.runptr(add1,1)
end

assert(what0() == 2)

terra what()
	var c : C.Foo
	c.bar =  add1
	return c.bar(1)
end

assert(what() == 2)