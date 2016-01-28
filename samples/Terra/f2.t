local C = terralib.includecstring [[
	typedef struct {
		int a;
		int b;
	} Foo;
]]
for i,e in ipairs(C.Foo.entries) do
	print(e.field,e.type)
end

terra useFoo()
	var f : C.Foo
	f.a = 1
	f.b = 2
end

useFoo()