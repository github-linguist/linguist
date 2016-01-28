C = terralib.includecstring [[
	int foobar() {
		return 3;
	}
]]


terra doit()
	return C.foobar()
end

print(doit())