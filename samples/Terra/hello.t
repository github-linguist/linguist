local c = terralib.includec("stdio.h")

terra hello()
	c.printf("hello, world\n")
end

terralib.saveobj("hello",{main = hello})
hello()
