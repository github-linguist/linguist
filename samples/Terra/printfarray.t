

C = terralib.includec("stdio.h")

terra char(a : &int8) : int8
	return a[0]
end
terra foobar()
	var a = arrayof(int8,char("a"),0)
	C.printf("%s\n",a)
end
foobar()