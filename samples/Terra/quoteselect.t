
local a = {}

local c = terralib.includec("stdio.h")

b = `c.printf("hello\n")

terra foo()
	return b
end

foo()