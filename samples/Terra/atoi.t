c = terralib.includec("stdlib.h")

terra what()
	return c.atoi("52")
end

print(what())
