if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)

N = 1024

somedata = cudalib.sharedmemory(int,N)

terra bar(result : &int)
    var t = tid()
    somedata[t] = t
    cudalib.nvvm_barrier0()
    result[t] = somedata[N - 1 - t]  
end

terralib.includepath = terralib.includepath..";/usr/local/cuda/include"

local C = terralib.includecstring [[
#include "cuda_runtime.h"
#include <stdlib.h>
#include <stdio.h>
]]

local R = terralib.cudacompile({ bar = bar },true)


terra doit(N : int)
	var data : &int
	C.cudaMalloc([&&opaque](&data),sizeof(int)*N)
	var launch = terralib.CUDAParams { 1,1,1, N,1,1, 0, nil }
	R.bar(&launch,data)
	var results : &int = [&int](C.malloc(sizeof(int)*N))
	C.cudaMemcpy(results,data,sizeof(int)*N,2)
	var result = 0
	for i = 0,N do
	    --C.printf("result = %d\n",results[i])
		result = result + results[i]
	end
	return result
end

local test = require("test")
local expected = (N - 1)*N/2
test.eq(doit(N),expected)
