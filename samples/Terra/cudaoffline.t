if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x
local ntid = cudalib.nvvm_read_ptx_sreg_ntid_x

theone = global(0)

theconst = cudalib.constantmemory(int,1)

terra foo(result : &float)
    result[tid()] = tid() + theone + theconst[0]
end

local C = terralib.includecstring [[
#include "cuda_runtime.h"
#include <stdlib.h>
#include <stdio.h>
]]

local R,L = terralib.cudacompile({ foo = foo, aone = theone, theconst = theconst },nil,nil,false)

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


terra main() : int
    if L(nil,nil,nil,0) ~= 0 then
        C.printf("WHAT\n")
    end
    var N = 16
    var expected = (N - 1)*N/2 + 3*N
    return terralib.select(doit(N) == expected,0,1)
end


local ffi = require 'ffi'
local path = ({ OSX = "/lib", Linux = "/lib64", Windows = "\\lib\\x64" })[ffi.os]
path = terralib.cudahome..path


local args = ffi.os == "Windows" and {path.."\\cuda.lib", path.."\\cudart.lib"} 
                                 or {"-L"..path, "-Wl,-rpath,"..path, "-lcuda", "-lcudart"}

local name = ffi.os == "Windows" and ".\\cudaoffline.exe" or "./cudaoffline"
terralib.saveobj(name,{ main = main },args)
assert(os.execute(name) == 0)

