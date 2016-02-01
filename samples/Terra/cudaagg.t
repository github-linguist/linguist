if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)
local ntid = cudalib.nvvm_read_ptx_sreg_ntid_x -- terralib.intrinsic("llvm.nvvm.read.ptx.sreg.ntid.x",{} -> int)


struct OneFloat {
    a : int;
}

fn = terra(result : &float, bar : int[5])
	result[tid()] = bar[0] + bar[1] + bar[2] + bar[3] + bar[4]
end
fn:setinlined(false)

--our very simple cuda kernel
--more work needs to be done to expose the right CUDA intrinsics
--to do more complicated things
foo = terra(result : &float)
    fn(result, array(tid(),tid()+1,tid()+2,tid()+3,tid()+4) )
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

function s(n) return (n - 1) * n / 2 end
function ex(i) return s(N+i) - s(i) end
local expected = ex(0) + ex(1) + ex(2) + ex(3) + ex(4)
test.eq(doit(N),expected)

