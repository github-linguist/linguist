struct A {
	a : int;
	b : int;
}

function A.metamethods.__getentries(self)
	print("GET ENTRIES")
	for i,e in ipairs(self.entries) do
		e.field = "foo"..e.field
	end
	return self.entries
end

terra foo()
	var a : A
	a.fooa = 3
	a.foob = 4
	return a.fooa + a.foob
end


assert(foo() == 7)