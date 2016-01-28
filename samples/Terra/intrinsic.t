local sqrt = terralib.intrinsic("llvm.sqrt.f32",float -> float)
local sqrt2 = terralib.intrinsic("llvm.sqrt.v4f32",vector(float,4) -> vector(float,4))

local sqrt3 = terralib.intrinsic(function(types)
	if #types == 1 and types[1]:isvector() and types[1].type == float then
		local N = types[1].N
		return ("llvm.sqrt.v%df32"):format(N), vector(float,N) -> vector(float,N)
	elseif #types == 1 and types[1] == float then
		return "llvm.sqrt.f32",float -> float
	end
end)

terra foo()
	var v = vector(1.f,2.f,3.f,4.f)
	var c = sqrt2(vector(1,2,3,4))
	return sqrt(4) + c[3] + sqrt3(v)[3] + sqrt3(4.f)
end

local test = require("test")

test.eq(foo(),8)