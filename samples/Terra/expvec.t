
if require("ffi").os == "Windows" then
	print("Not consistent on windows")
	return
end
local C = terralib.includecstring[[
	#include <immintrin.h>
	void dostuff(__m128 what) {}
	void dostuff2() { __m128 a; dostuff(a); }
]]

local V = 1
local exp = terralib.intrinsic(("llvm.exp.v%df32"):format(V),vector(float,V) -> vector(float,V))

terra foo(N : int)
	
	var d = vector(0.f,1.f,3.f,4.f)

	C.dostuff(d)

	--var v = vector(0.f,1.f,2.f,3.f,4.f,5.f,6.f,7.f)
	var v = vector(0.f)
	for i = 0, N do
		v = exp(v)
	end
	return v[0],v[1],v[2],v[3]
end

C.dostuff:printpretty()

local begin = terralib.currenttimeinseconds()
print(foo(100000000))
local endd = terralib.currenttimeinseconds()
print(endd - begin)