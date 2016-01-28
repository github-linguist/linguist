ffi = require("ffi")
cstdio = terralib.includec("stdio.h")

local vec4 = &vector(float,4)

local align = terralib.aligned 
terra lol( w : &float, out : &float)
  var a  = terralib.attrload(vec4(w),{align = 4})
  terralib.attrstore(vec4(out), a, {align = 4})
end

dat = ffi.new("float[?]",32)
for i=0,31 do dat[i]=i end
datO = ffi.new("float[?]",32)

lol:compile()
lol:disas()

lol(dat, datO)

