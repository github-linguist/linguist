
local c = terralib.includec("stdio.h")

terralib.tree.printraw(getmetatable(c).errors)

terra main()
	c.printf("hello, world\n")
end

main()