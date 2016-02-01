if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)
local ntid = cudalib.nvvm_read_ptx_sreg_ntid_x -- terralib.intrinsic("llvm.nvvm.read.ptx.sreg.ntid.x",{} -> int)


theone = global(0)

theconst = cudalib.constantmemory(int,1)

terra foo(result : &float)
    result[tid()] = tid() + theone + theconst[0]
end

terralib.includepath = terralib.includepath..";/usr/local/cuda/include"

local C = terralib.includecstring [[
#include "cuda_runtime.h"
#include <stdlib.h>
#include <stdio.h>
]]
local R = terralib.cudacompile({ foo = foo, aone = theone, theconst = theconst })

terra doit(N : int)
	var data : &float
	C.cudaMalloc([&&opaque](&data),sizeof(float)*N)
	var one = 1
	var two = 2
	C.cudaMemcpy(R.aone,&one,sizeof(int),1)
	C.cudaMemcpy(R.theconst,&two,sizeof(int),1)
	var launch = terralib.CUDAParams { 1,1,1, N,1,1, 0, nil }
	R.foo(&launch,data)
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
local expected = (N - 1)*N/2 + 3*N
test.eq(doit(N),expected)
