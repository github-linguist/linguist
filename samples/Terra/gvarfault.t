if require("ffi").os == "Windows" then
	print("Disabled on windows (uses unistd.h)")
	return
end
local C = terralib.includecstring(
[[
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
]]
)

local gstr = global(&int8, "some text")

terra fun1()
    C.printf("gstr: %s \n", gstr)
    return 0
end

fun1:printpretty()

fun1()