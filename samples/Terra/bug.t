local f = assert(io.popen("uname", 'r'))
local s = assert(f:read('*a'))
f:close()

if s~="Darwin\n" then
  print("Warning, not running test b/c this isn't a mac")
else


local OC = require("lib/objc")
local OCR = terralib.includec("objc/runtime.h")

terra main()
	var nsobject = OC.NSObject
	OCR.objc_allocateClassPair([&OCR.objc_class](nsobject.data),nil,0)
end

main:compile()

end