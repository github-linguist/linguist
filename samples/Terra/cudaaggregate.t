if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)
local ntid = cudalib.nvvm_read_ptx_sreg_ntid_x -- terralib.intrinsic("llvm.nvvm.read.ptx.sreg.ntid.x",{} -> int)

struct A {
    a : int
    b : int
    c : int[2]
}
terra foo(result : &float,a : A, c : int, d : int[2])
    var t = tid()
	result[t] = t + a.a + a.b + c + a.c[0] + a.c[1] + d[0] + d[1]
end

terralib.includepath = terralib.includepath..";/usr/local/cuda/include"

local C = terralib.includecstring [[
#include "cuda_runtime.h"
#include <stdlib.h>
#include <stdio.h>
]]
local R = terralib.cudacompile({ bar = foo })

terra doit(N : int)
	var data : &float
	C.cudaMalloc([&&opaque](&data),sizeof(float)*N)
	var launch = terralib.CUDAParams { 1,1,1, N,1,1, 0, nil }
	R.bar(&launch,data, A { 1,2, array(3,4) },5,array(6,7))
	var results : &float = [&float](C.malloc(sizeof(float)*N))
	C.cudaMemcpy(results,data,sizeof(float)*N,2)
	var result = 0.f
	for i = 0,N do
		result = result + results[i]
	end
	return result
end

local test = require("test")
local N = 16
local expected = (N - 1)*N/2 + N*(1 + 2 + 3 + 4 + 5 + 6 + 7)
test.eq(doit(N),expected)
