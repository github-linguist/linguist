if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)

C = terralib.includec("stdio.h")
vprintf = terralib.externfunction("cudart:vprintf", {&int8,&int8} -> int)

foo = terra(result : &float)
    var t = tid()
    vprintf("%d\n",[&int8](&t))
end

terralib.includepath = terralib.includepath..";/usr/local/cuda/include"


sync = terralib.externfunction("cudaThreadSynchronize", {} -> int)

local R = terralib.cudacompile({ bar = foo })

terra doit(N : int)
	var launch = terralib.CUDAParams { 1,1,1, N,1,1, 0, nil }
	R.bar(&launch,nil)
	sync()
	C.printf("and were done\n")
end

doit(3)
