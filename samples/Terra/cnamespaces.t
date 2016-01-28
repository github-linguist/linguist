C,T = terralib.includecstring [[
	
	struct Foo {
		int a;
	};
	typedef int Foo;
	typedef struct Foo * Bar;
	typedef Foo * Bar2;
	Bar bar(struct Foo * a, Bar2 b) { return (Bar)0; }
]]

terra what()
	var f  = T.Foo { 3 }
	var a : C.Bar = &f
	return a.a
end

assert(3 == what())

C,T = terralib.includecstring [[
	typedef struct { int a; } Foo;
	typedef Foo * FooPtr;
	int returna(FooPtr a) { return a->a; }
]]

terra what2()
	var a : C.Foo = C.Foo { 3 }
	var ap : C.FooPtr = &a
	return C.returna(ap)
end

assert(3 == what2())
