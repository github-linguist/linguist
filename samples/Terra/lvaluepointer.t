

terra foo()
	var a : int = 5
	var pa = &a
	@pa = 4
	@[&float](pa) = 5
end

foo()