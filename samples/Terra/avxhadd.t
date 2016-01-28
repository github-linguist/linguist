local haddavx = terralib.intrinsic("llvm.x86.avx.hadd.ps.256", { vector(float,8), vector(float,8) } -> vector(float,8))

terra hadd(v : vector(float,8))
	var v1 = haddavx(v,v)
	var v2 = haddavx(v1,v1)
	return v2[0] + v2[4]
end

ffi = require("ffi")

local stdio = terralib.includec("stdio.h")

terra foobar(a : &float)
	return hadd(@[&vector(float,8)](a))
end

dat = ffi.new("float[?] __attribute__((aligned(32)))",8)

for i = 1,8 do
	dat[i-1] = i
end

if terralib.llvmversion == 31 then
	print("ignoring...")
else
	foobar:compile()
	local test = require("test")
	test.eq(foobar(dat),36)
end

--terralib.saveobj("avxhadd",{main = foobar})
--os.execute("./avxhadd")