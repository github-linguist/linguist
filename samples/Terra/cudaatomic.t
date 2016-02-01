if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)

foo = terra(result : &int)
    var t = tid()
    terralib.asm(terralib.types.unit,"red.global.max.u32 [$0], $1;","l,r",true,result,t)
end

terralib.includepath = terralib.includepath..";/usr/local/cuda/include"


local C = terralib.includecstring [[
#include "cuda_runtime.h"
#include <stdlib.h>
#include <stdio.h>
]]

sync = terralib.externfunction("cudaThreadSynchronize", {} -> int)

local R = terralib.cudacompile({ bar = foo },true)

terra doit(N : int)
    var data = 0
    var location : &int
    C.cudaMalloc([&&opaque](&location),sizeof(int))
    C.cudaMemcpy(location,&data,sizeof(int),1)
    
	var launch = terralib.CUDAParams { 1,1,1, N,1,1, 0, nil }
	R.bar(&launch,location)
	var data2 = -1
	C.cudaMemcpy(&data2,location,sizeof(int),2)
    return data2
end

assert(doit(32) == 31)
