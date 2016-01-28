
local c = terralib.includec("stdio.h")

terra foo()
    var a = "whatwhat\n"
    return c.puts(a)
end

local test = require("test")
local ffi = require("ffi")
if ffi.os == "Windows" then
	test.eq(foo(),0)
else
	test.eq(foo(),10)
end