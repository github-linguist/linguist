if not terralib.cudacompile then
	print("CUDA not enabled, not performing test...")
	return
end

local const = cudalib.constantmemory(float, 1)

local terra kernel(data: &float)
end

local M = terralib.cudacompile({
    kernel = kernel,
    const = const
})
