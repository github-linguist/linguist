// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/allocators/dynamic_arena.c3
// Permalink: https://github.com/c3lang/c3c/blob/d328611/lib/std/core/allocators/dynamic_arena.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::allocator;
import std::math;

struct DynamicArenaAllocator (Allocator)
{
	Allocator backing_allocator;
	DynamicArenaPage* page;
	DynamicArenaPage* unused_page;
	usz page_size;
}

/**
 * @param [&inout] allocator
 * @require page_size >= 128
 **/
fn void DynamicArenaAllocator.init(&self, usz page_size, Allocator allocator)
{
	self.page = null;
	self.unused_page = null;
	self.page_size = page_size;
	self.backing_allocator = allocator;
}

import std::io;
fn void DynamicArenaAllocator.free(&self)
{
	DynamicArenaPage* page = self.page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		allocator::free(self.backing_allocator, page.memory);
		allocator::free(self.backing_allocator, page);
		page = next_page;
	}
	page = self.unused_page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		allocator::free(self.backing_allocator, page.memory);
		allocator::free(self.backing_allocator, page);
		page = next_page;
	}
	self.page = null;
	self.unused_page = null;
}

struct DynamicArenaPage @local
{
	void* memory;
	void* prev_arena;
	usz total;
	usz used;
	void* current_stack_ptr;
}

struct DynamicArenaChunk @local
{
	usz size;
}

/**
 * @require ptr
 * @require self.page `tried to free pointer on invalid allocator`
 */
fn void DynamicArenaAllocator.release(&self, void* ptr, bool) @dynamic
{
	DynamicArenaPage* current_page = self.page;
	if (ptr == current_page.current_stack_ptr)
	{
		current_page.used = (usz)((ptr - DEFAULT_SIZE_PREFIX) - current_page.memory);
	}
	current_page.current_stack_ptr = null;
}

/**
 * @require size > 0 `Resize doesn't support zeroing`
 * @require old_pointer != null `Resize doesn't handle null pointers`
 * @require self.page `tried to realloc pointer on invalid allocator`
 */
fn void*! DynamicArenaAllocator.resize(&self, void* old_pointer, usz size, usz alignment) @dynamic
{
	DynamicArenaPage* current_page = self.page;
	alignment = alignment_for_allocation(alignment);
	usz* old_size_ptr = old_pointer - DEFAULT_SIZE_PREFIX;
	usz old_size = *old_size_ptr;
	// We have the old pointer and it's correctly aligned.
	if (old_size >= size && mem::ptr_is_aligned(old_pointer, alignment))
	{
		*old_size_ptr = size;
		if (current_page.current_stack_ptr == old_pointer)
		{
			current_page.used = (usz)((old_pointer - DEFAULT_SIZE_PREFIX) - current_page.memory);
		}
		return old_pointer;
	}
	if REUSE: (current_page.current_stack_ptr == old_pointer && mem::ptr_is_aligned(old_pointer, alignment))
	{
		assert(size > old_size);
		usz add_size = size - old_size;
		if (add_size + current_page.used > current_page.total) break REUSE;
		*old_size_ptr = size;
		current_page.used += add_size;
		return old_pointer;
	}
	void* new_mem = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(new_mem, old_pointer, old_size, mem::DEFAULT_MEM_ALIGNMENT);
	return new_mem;
}

fn void DynamicArenaAllocator.reset(&self, usz mark = 0) @dynamic
{
	assert(mark == 0, "Unexpectedly reset dynamic arena allocator with mark %d", mark);
	DynamicArenaPage* page = self.page;
	DynamicArenaPage** unused_page_ptr = &self.unused_page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		page.used = 0;
		DynamicArenaPage* prev_unused = *unused_page_ptr;
		*unused_page_ptr = page;
		page.prev_arena = prev_unused;
		page = next_page;
	}
	self.page = page;
}

/**
 * @require math::is_power_of_2(alignment)
 * @require size > 0
 */
fn void*! DynamicArenaAllocator._alloc_new(&self, usz size, usz alignment) @local
{
	// First, make sure that we can align it, extending the page size if needed.
	usz page_size = max(self.page_size, mem::aligned_offset(size + DynamicArenaChunk.sizeof + alignment, alignment));
	assert(page_size > size + DynamicArenaChunk.sizeof);
	// Grab the page without alignment (we do it ourselves)
	void* mem = allocator::malloc_try(self.backing_allocator, page_size)!;
	DynamicArenaPage*! page = allocator::new_try(self.backing_allocator, DynamicArenaPage);
	if (catch err = page)
	{
		allocator::free(self.backing_allocator, mem);
		return err?;
	}
	page.memory = mem;
	void* mem_start = mem::aligned_pointer(mem + DynamicArenaChunk.sizeof, alignment);
	assert(mem_start + size < mem + page_size);
	DynamicArenaChunk* chunk = (DynamicArenaChunk*)mem_start - 1;
	chunk.size = size;
	page.prev_arena = self.page;
	page.total = page_size;
	page.used = mem_start + size - page.memory;
	self.page = page;
	page.current_stack_ptr = mem_start;
	return mem_start;
}

/**
 * @require size > 0 `acquire expects size > 0`
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! DynamicArenaAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	DynamicArenaPage* page = self.page;
	void* ptr = {|
		if (!page && self.unused_page)
		{
			self.page = page = self.unused_page;
			self.unused_page = page.prev_arena;
			page.prev_arena = null;
		}
		if (!page) return self._alloc_new(size, alignment);
		void* start = mem::aligned_pointer(page.memory + page.used + DynamicArenaChunk.sizeof, alignment);
		usz new_used = start - page.memory + size;
		if ALLOCATE_NEW: (new_used > page.total)
		{
			if ((page = self.unused_page))
			{
				start = mem::aligned_pointer(page.memory + page.used + DynamicArenaChunk.sizeof, alignment);
				new_used = start + size - page.memory;
				if (page.total >= new_used)
				{
					self.unused_page = page.prev_arena;
					page.prev_arena = self.page;
					self.page = page;
					break ALLOCATE_NEW;
				}
			}
			return self._alloc_new(size, alignment);
		}
		page.used = new_used;
		assert(start + size == page.memory + page.used);
		void* mem = start;
		DynamicArenaChunk* chunk = (DynamicArenaChunk*)mem - 1;
		chunk.size = size;
		return mem;
	|}!;
	if (init_type == ZERO) mem::clear(ptr, size, mem::DEFAULT_MEM_ALIGNMENT);
	return ptr;
}
