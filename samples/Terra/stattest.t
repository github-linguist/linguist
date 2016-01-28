local ffi = require("ffi")
if ffi.os == "Windows" then
  return
end

C,T = terralib.includec("sys/stat.h")
terra dostat()
	var s : T.stat
	C.stat("stattest.t",&s)
	return s.st_size
end

assert(dostat() == 210)
