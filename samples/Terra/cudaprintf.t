if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local tid = cudalib.nvvm_read_ptx_sreg_tid_x--terralib.intrinsic("llvm.nvvm.read.ptx.sreg.tid.x",{} -> int)

vprintf = terralib.externfunction("cudart:vprintf", {&int8,&int8} -> int)
local function createbuffer(args)
    local Buf = terralib.types.newstruct()
    return quote
        var buf : Buf
        escape
            for i,e in ipairs(args) do
                local typ = e:gettype()
                local field = "_"..tonumber(i)
                typ = typ == float and double or typ
                table.insert(Buf.entries,{field,typ})
                emit quote
                   buf.[field] = e
                end
            end
        end
    in
        [&int8](&buf)
    end
end
printf = macro(function(fmt,...)
    local buf = createbuffer({...})
    return `vprintf(fmt,buf) 
end)

foo = terra(result : &float)
    var t = tid()
    printf("a = %d, b = %f, c = %d\n",t,1.0 + t,t + 2)
end

sync = terralib.externfunction("cuStreamSynchronize", {&opaque} -> int)

annotations = { {"maxntidx",43}, {"minctasm",8}} -- example of annotating cuda kernel with launch bounds
local R = terralib.cudacompile({ bar = { kernel = foo, annotations = annotations }})

terra doit(N : int)
	var launch = terralib.CUDAParams { 1,1,1, N,1,1, 0, nil }
	R.bar(&launch,nil)
	sync(nil)
end

doit(3)

