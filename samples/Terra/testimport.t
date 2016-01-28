
local r,e = terralib.loadstring [[
import "lib.fakeimport"
]]

assert(e:match("stack traceback"))