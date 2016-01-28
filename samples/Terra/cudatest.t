if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)
local ntid = cudalib.nvvm_read_ptx_sreg_ntid_x -- terralib.intrinsic("llvm.nvvm.read.ptx.sreg.ntid.x",{} -> int)

fn = terra(result : &float)
    var t = tid()
	result[t] = t
end
fn:setinlined(false)
--our very simple cuda kernel
--more work needs to be done to expose the right CUDA intrinsics
--to do more compilicated things
foo = terra(result : &float)
    fn(result)
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
	R.bar(&launch,data)
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
local expected = (N - 1)*N/2
test.eq(doit(N),expected)
