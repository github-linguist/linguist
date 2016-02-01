if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)

C = terralib.includecstring [[
#include <stdio.h>
#include <cuda.h>
#include <cuda_runtime.h>
#include <string.h>
]]

vprintf = terralib.externfunction("cudart:vprintf", {&int8,&int8} -> int)

foo = terra(result : C.cudaTextureObject_t)
    var t = tid()
    var r = terralib.asm([tuple(float,float,float,float)],
                          "tex.1d.v4.f32.s32 {$0,$1,$2,$3}, [$4, {$5}];", 
                          "=f,=f,=f,=f,l,r",false,result,t)
    var rr : double = r._0
    vprintf("%f\n",[&int8](&rr))    
end

terralib.includepath = terralib.includepath..";/usr/local/cuda/include"

sync = terralib.externfunction("cudaThreadSynchronize", {} -> int)

local R = terralib.cudacompile({ foo = foo })

terra doit(N : int)
	var d_buffer : &double
	C.cudaMalloc([&&opaque](&d_buffer),N*sizeof(float))
	
	var h_buffer = arrayof(float,0,1,2,3,4,5,6,7,8,9,10)
	C.cudaMemcpy(d_buffer,&h_buffer[0],sizeof(float)*N, C.cudaMemcpyHostToDevice)
	
	var resDesc : C.cudaResourceDesc
	C.memset(&resDesc,0,sizeof(C.cudaResourceDesc))
	resDesc.resType = C.cudaResourceTypeLinear;
    resDesc.res.linear.devPtr = d_buffer;
    resDesc.res.linear.desc.f = C.cudaChannelFormatKindFloat;
    resDesc.res.linear.desc.x = 32; -- bits per channel
    resDesc.res.linear.sizeInBytes = N*sizeof(float);
    var texDesc : C.cudaTextureDesc
    C.memset(&texDesc, 0, sizeof(C.cudaTextureDesc));
    texDesc.readMode = C.cudaReadModeElementType;
	
	var tex : C.cudaTextureObject_t
    C.cudaCreateTextureObject(&tex, &resDesc, &texDesc, nil);
    
	var launch = terralib.CUDAParams { 1,1,1, N,1,1, 0, nil }
	R.foo(&launch,tex)
	sync()
	C.printf("and were done\n")
end

doit(10)
