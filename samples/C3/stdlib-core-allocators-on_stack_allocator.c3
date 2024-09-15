// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/allocators/on_stack_allocator.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/core/allocators/on_stack_allocator.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::mem::allocator;

struct OnStackAllocator (Allocator)
{
	Allocator backing_allocator;
	char[] data;
	usz used;
	OnStackAllocatorExtraChunk* chunk;
}


struct OnStackAllocatorExtraChunk @local
{
	bool is_aligned;
	OnStackAllocatorExtraChunk* prev;
	void* data;
}

/**
 * @param [&inout] allocator
 * Initialize a memory arena for use using the provided bytes.
 **/
fn void OnStackAllocator.init(&self, char[] data, Allocator allocator)
{
	self.data = data;
	self.backing_allocator = allocator;
	self.used = 0;
}

fn void OnStackAllocator.free(&self)
{
	OnStackAllocatorExtraChunk* chunk = self.chunk;
	while (chunk)
	{
		if (chunk.is_aligned)
		{
			allocator::free_aligned(self.backing_allocator, chunk.data);
		}
		else
		{
			allocator::free(self.backing_allocator, chunk.data);
		}
		void* old = chunk;
		chunk = chunk.prev;
		allocator::free(self.backing_allocator, old);
	}
	self.chunk = null;
	self.used = 0;
}

struct OnStackAllocatorHeader
{
	usz size;
	char[*] data;
}

/**
 * @require old_pointer
 **/
fn void OnStackAllocator.release(&self, void* old_pointer, bool aligned) @dynamic
{
	if (allocation_in_stack_mem(self, old_pointer)) return;
	on_stack_allocator_remove_chunk(self, old_pointer);
	self.release(old_pointer, aligned);
}

fn bool allocation_in_stack_mem(OnStackAllocator* a, void* ptr) @local
{
	return ptr >= a.data.ptr && ptr <= &a.data[^1];
}

fn void on_stack_allocator_remove_chunk(OnStackAllocator* a, void* ptr) @local
{
	OnStackAllocatorExtraChunk* chunk = a.chunk;
	OnStackAllocatorExtraChunk** addr = &a.chunk;
	while (chunk)
	{
		if (chunk.data == ptr)
		{
			*addr = chunk.prev;
			allocator::free(a.backing_allocator, chunk);
			return;
		}
		addr = &chunk.prev;
		chunk = *addr;
	}
	unreachable("Missing chunk");
}

fn OnStackAllocatorExtraChunk* on_stack_allocator_find_chunk(OnStackAllocator* a, void* ptr) @local
{
	OnStackAllocatorExtraChunk* chunk = a.chunk;
	while (chunk)
	{
		if (chunk.data == ptr) return chunk;
		chunk = chunk.prev;
	}
	return null;
}

/**
 * @require size > 0
 * @require old_pointer != null
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 **/
fn void*! OnStackAllocator.resize(&self, void* old_pointer, usz size, usz alignment) @dynamic
{
	if (!allocation_in_stack_mem(self, old_pointer))
	{
		OnStackAllocatorExtraChunk* chunk = on_stack_allocator_find_chunk(self, old_pointer);
		assert(chunk, "Tried to realloc pointer not belonging to the allocator");
		return chunk.data = self.backing_allocator.resize(old_pointer, size, alignment)!;
	}

	OnStackAllocatorHeader* header = old_pointer - OnStackAllocatorHeader.sizeof;
	usz old_size = header.size;
	void* mem = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(mem, old_pointer, old_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	return mem;
}

/**
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require size > 0
 **/
fn void*! OnStackAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	bool aligned = alignment > 0;
	alignment = alignment_for_allocation(alignment);
	usz total_len = self.data.len;
	void* start_mem = self.data.ptr;
	void* unaligned_pointer_to_offset = start_mem + self.used + OnStackAllocatorHeader.sizeof ;
	void* mem = mem::aligned_pointer(unaligned_pointer_to_offset, alignment);
	usz end = (usz)(mem - self.data.ptr) + size;
	Allocator backing_allocator = self.backing_allocator;

	if (end > total_len)
	{
		OnStackAllocatorExtraChunk* chunk = allocator::alloc_try(backing_allocator, OnStackAllocatorExtraChunk)!;
		defer catch allocator::free(backing_allocator, chunk);
		defer try self.chunk = chunk;
		*chunk = { .prev = self.chunk, .is_aligned = aligned };
		return chunk.data = backing_allocator.acquire(size, init_type, aligned ? alignment : 0)!;
	}
	self.used = end;
	OnStackAllocatorHeader* header = mem - OnStackAllocatorHeader.sizeof;
	header.size = size;
	return mem;
}
