local IO = terralib.includec("stdio.h")

terra foobar()
	IO.printf("%f\n",3.3f)
end

foobar()