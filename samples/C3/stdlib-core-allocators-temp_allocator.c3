// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/allocators/temp_allocator.c3
// Permalink: https://github.com/c3lang/c3c/blob/59ed118/lib/std/core/allocators/temp_allocator.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::mem::allocator;
import std::io, std::math;

struct TempAllocatorChunk @local
{
	usz size;
	char[*] data;
}

struct TempAllocator (Allocator)
{
	Allocator backing_allocator;
	TempAllocatorPage* last_page;
	usz used;
	usz capacity;
	char[*] data;
}

const usz PAGE_IS_ALIGNED @private = (usz)isz.max + 1u;


struct TempAllocatorPage
{
	TempAllocatorPage* prev_page;
	void* start;
	usz mark;
	usz size;
	usz ident;
	char[*] data;
}

macro usz TempAllocatorPage.pagesize(&self) => self.size & ~PAGE_IS_ALIGNED;
macro bool TempAllocatorPage.is_aligned(&self) => self.size & PAGE_IS_ALIGNED == PAGE_IS_ALIGNED;

/**
 * @require size >= 16
 **/
fn TempAllocator*! new_temp_allocator(usz size, Allocator allocator)
{
	TempAllocator* temp = allocator::alloc_with_padding(allocator, TempAllocator, size)!;
	temp.last_page = null;
	temp.backing_allocator = allocator;
	temp.used = 0;
	temp.capacity = size;
	return temp;
}

fn void TempAllocator.destroy(&self)
{
	self.reset(0);
	if (self.last_page) (void)self._free_page(self.last_page);
	allocator::free(self.backing_allocator, self);
}

fn usz TempAllocator.mark(&self) @dynamic => self.used;

fn void TempAllocator.release(&self, void* old_pointer, bool) @dynamic
{
	usz old_size = *(usz*)(old_pointer - DEFAULT_SIZE_PREFIX);
	if (old_pointer + old_size == &self.data[self.used])
	{
		self.used -= old_size;
		asan::poison_memory_region(&self.data[self.used], old_size);
	}
}
fn void TempAllocator.reset(&self, usz mark) @dynamic
{
	TempAllocatorPage *last_page = self.last_page;
	while (last_page && last_page.mark > mark)
	{
		TempAllocatorPage *to_free = last_page;
		last_page = last_page.prev_page;
		self._free_page(to_free)!!;
	}
	self.last_page = last_page;
	$if env::COMPILER_SAFE_MODE || env::ADDRESS_SANITIZER:
		if (!last_page)
		{
			usz cleaned = self.used - mark;
			if (cleaned > 0)
			{
				$if env::COMPILER_SAFE_MODE:
					self.data[mark : cleaned] = 0xAA;
				$endif
				asan::poison_memory_region(&self.data[mark], cleaned);
			}
		}
	$endif
	self.used = mark;
}

fn void! TempAllocator._free_page(&self, TempAllocatorPage* page) @inline @local
{
	void* mem = page.start;
	return self.backing_allocator.release(mem, page.is_aligned());
}

fn void*! TempAllocator._realloc_page(&self, TempAllocatorPage* page, usz size, usz alignment) @inline @local
{
	// Then the actual start pointer:
	void* real_pointer = page.start;

	// Walk backwards to find the pointer to this page.
	TempAllocatorPage **pointer_to_prev = &self.last_page;
	// Remove the page from the list
	while (*pointer_to_prev != page)
	{
		pointer_to_prev = &((*pointer_to_prev).prev_page);
	}
	*pointer_to_prev = page.prev_page;
	usz page_size = page.pagesize();
	// Clear on size > original size.
	void* data = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(data, &page.data[0], page_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	self.backing_allocator.release(real_pointer, page.is_aligned());
	return data;
}

fn void*! TempAllocator.resize(&self, void* pointer, usz size, usz alignment) @dynamic
{
	TempAllocatorChunk *chunk = pointer - TempAllocatorChunk.sizeof;
	if (chunk.size == (usz)-1)
	{
		assert(self.last_page, "Realloc of non temp pointer");
		// First grab the page
		TempAllocatorPage *page = pointer - TempAllocatorPage.sizeof;
		return self._realloc_page(page, size, alignment);
	}

	TempAllocatorChunk* data = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(data, pointer, chunk.size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);

	return data;
}

/**
 * @require size > 0
 * @require !alignment || math::is_power_of_2(alignment)
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 **/
fn void*! TempAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	void* start_mem = &self.data;
	void* starting_ptr = start_mem + self.used;
	void* aligned_header_start = mem::aligned_pointer(starting_ptr, TempAllocatorChunk.alignof);
	void* mem = aligned_header_start + TempAllocatorChunk.sizeof;
	if (alignment > TempAllocatorChunk.alignof)
	{
		mem = mem::aligned_pointer(mem, alignment);
	}
	usz new_usage = (usz)(mem - start_mem) + size;

	// Arena allocation, simple!
	if (new_usage <= self.capacity)
	{
		asan::unpoison_memory_region(starting_ptr, new_usage - self.used);
		TempAllocatorChunk* chunk_start = mem - TempAllocatorChunk.sizeof;
		chunk_start.size = size;
		self.used = new_usage;
		if (init_type == ZERO) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
		return mem;
	}

	// Fallback to backing allocator
	TempAllocatorPage* page;

	// We have something we need to align.
	if (alignment > mem::DEFAULT_MEM_ALIGNMENT)
	{
		// This is actually simpler, since it will create the offset for us.
		usz total_alloc_size = mem::aligned_offset(TempAllocatorPage.sizeof + size, alignment);
		if (init_type == ZERO)
		{
			mem = allocator::calloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		else
		{
			mem = allocator::malloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		page = (TempAllocatorPage*)mem - 1;
		page.start = mem;
		page.size = size | PAGE_IS_ALIGNED;
	}
	else
	{
		// Here we might need to pad
		usz padded_header_size = mem::aligned_offset(TempAllocatorPage.sizeof, mem::DEFAULT_MEM_ALIGNMENT);
		usz total_alloc_size = padded_header_size + size;
		void* alloc = self.backing_allocator.acquire(total_alloc_size, init_type, 0)!;

		// Find the page.
		page = alloc + padded_header_size - TempAllocatorPage.sizeof;
		assert(mem::ptr_is_aligned(page, TempAllocator.alignof));
		assert(mem::ptr_is_aligned(&page.data[0], mem::DEFAULT_MEM_ALIGNMENT));
		page.start = alloc;
		page.size = size;
	}

	// Mark it as a page
	page.ident = ~(usz)0;
	// Store when it was created
	page.mark = ++self.used;
	// Hook up the page.
	page.prev_page = self.last_page;
	self.last_page = page;
	return &page.data[0];
}

fn void! TempAllocator.print_pages(&self, File* f)
{
	TempAllocatorPage *last_page = self.last_page;
	if (!last_page)
	{
		io::fprintf(f, "No pages.\n")!;
		return;
	}
	io::fprintf(f, "---Pages----\n")!;
	uint index = 0;
	while (last_page)
	{
		bool is_not_aligned = !(last_page.size & (1u64 << 63));
		io::fprintf(f, "%d. Alloc: %d %d at %p%s\n", ++index,
				last_page.size & ~(1u64 << 63), last_page.mark, &last_page.data[0], is_not_aligned ? "" : " [aligned]")!;
		last_page = last_page.prev_page;
	}
}
