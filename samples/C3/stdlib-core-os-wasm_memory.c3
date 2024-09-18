// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/os/wasm_memory.c3
// Permalink: https://github.com/c3lang/c3c/blob/499c82b/lib/std/core/os/wasm_memory.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::mem::allocator;


const usz WASM_BLOCK_SIZE = 65536;

WasmMemory wasm_memory;

struct WasmMemory
{
	usz allocation;
	uptr use;
}

fn char[]! WasmMemory.allocate_block(&self, usz bytes)
{
	if (!self.allocation)
	{
		self.allocation = $$wasm_memory_size(0) * WASM_BLOCK_SIZE;
	}
	isz bytes_required = bytes + self.use - self.allocation;
	if (bytes_required <= 0)
	{
		defer self.use += bytes;
		return ((char*)self.use)[:bytes];
	}

	usz blocks_required = (bytes_required + WASM_BLOCK_SIZE + 1) / WASM_BLOCK_SIZE;
	if ($$wasm_memory_grow(0, blocks_required) == -1) return AllocationFailure.OUT_OF_MEMORY?;
	self.allocation = $$wasm_memory_size(0) * WASM_BLOCK_SIZE;
	defer self.use += bytes;
	return ((char*)self.use)[:bytes];
}