string=terralib.includec("string.h")
buf=terralib.new(int8[1024])
ffi = require "ffi"
if ffi.os == "Windows" then
	string.strerror_s(buf,1024,1)
else
	string.strerror_r(1,buf,1024)
end
print(ffi.string(buf))
