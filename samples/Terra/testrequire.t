local f = assert(io.popen("uname", 'r'))
local s = assert(f:read('*a'))
f:close()

if s~="Darwin\n" then
  print("Warning, not running test b/c this isn't a mac")
else

local A = require("lib.objc")
local B = require("lib.objc")

assert(A == B)

end