// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/allocators/heap_allocator.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/core/allocators/heap_allocator.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import std::math;

struct SimpleHeapAllocator (Allocator)
{
	MemoryAllocFn alloc_fn;
	Header* free_list;
}

/**
 * @require allocator "An underlying memory provider must be given"
 * @require !self.free_list "The allocator may not be already initialized"
 **/
fn void SimpleHeapAllocator.init(&self, MemoryAllocFn allocator)
{
	self.alloc_fn = allocator;
	self.free_list = null;
}

fn void*! SimpleHeapAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		return alignment > 0 ? @aligned_alloc(self._calloc, size, alignment) : self._calloc(size);
	}
	return alignment > 0 ? @aligned_alloc(self._alloc, size, alignment) : self._alloc(size);
}

fn void*! SimpleHeapAllocator.resize(&self, void* old_pointer, usz size, usz alignment) @dynamic
{
	return alignment > 0
		? @aligned_realloc(self._calloc, self._free, old_pointer, size, alignment)
		:  self._realloc(old_pointer, size);
}

fn void SimpleHeapAllocator.release(&self, void* old_pointer, bool aligned) @dynamic
{
	if (aligned)
	{
		@aligned_free(self._free, old_pointer)!!;
	}
	else
	{
		self._free(old_pointer);
	}
}

/**
 * @require old_pointer && bytes > 0
 **/
fn void*! SimpleHeapAllocator._realloc(&self, void* old_pointer, usz bytes) @local
{
	// Find the block header.
	Header* block = (Header*)old_pointer - 1;
	if (block.size >= bytes) return old_pointer;
	void* new = self._alloc(bytes)!;
	usz max_to_copy = math::min(block.size, bytes);
	mem::copy(new, old_pointer, max_to_copy);
	self._free(old_pointer);
	return new;
}

fn void*! SimpleHeapAllocator._calloc(&self, usz bytes) @local
{
	void* data = self._alloc(bytes)!;
	mem::clear(data, bytes, mem::DEFAULT_MEM_ALIGNMENT);
	return data;
}

fn void*! SimpleHeapAllocator._alloc(&self, usz bytes) @local
{
	usz aligned_bytes = mem::aligned_offset(bytes, mem::DEFAULT_MEM_ALIGNMENT);
	if (!self.free_list)
	{
		self.add_block(aligned_bytes)!;
	}

	Header* current = self.free_list;
	Header* previous = current;
	while (current)
	{
		switch
		{
			case current.size >= aligned_bytes && current.size <= aligned_bytes + Header.sizeof + 64:
				if (current == previous)
				{
					self.free_list = current.next;
				}
				else
				{
					previous.next = current.next;
				}
				current.next = null;
				return current + 1;
			case current.size > aligned_bytes:
				Header* unallocated = (Header*)((char*)current + aligned_bytes + Header.sizeof);
				unallocated.size = current.size - aligned_bytes;
				unallocated.next = current.next;
				if (current == self.free_list)
				{
					self.free_list = unallocated;
				}
				else
				{
					previous.next = unallocated;
				}
				current.size = aligned_bytes;
				current.next = null;
				return current + 1;
			default:
				previous = current;
				current = current.next;
		}
	}
	self.add_block(aligned_bytes)!;
	return self._alloc(aligned_bytes);
}

fn void! SimpleHeapAllocator.add_block(&self, usz aligned_bytes) @local
{
	assert(mem::aligned_offset(aligned_bytes, mem::DEFAULT_MEM_ALIGNMENT) == aligned_bytes);
	char[] result = self.alloc_fn(aligned_bytes + Header.sizeof)!;
	Header* new_block = (Header*)result.ptr;
	new_block.size = result.len - Header.sizeof;
	new_block.next = null;
	self._free(new_block + 1);
}


fn void SimpleHeapAllocator._free(&self, void* ptr) @local
{
	// Empty ptr -> do nothing.
	if (!ptr) return;

	// Find the block header.
	Header* block = (Header*)ptr - 1;

	// No free list? Then just return self.
	if (!self.free_list)
	{
		self.free_list = block;
		return;
	}

	// Find where in the list it should be inserted.
	Header* current = self.free_list;
	Header* prev = current;
	while (current)
	{
		if (block < current)
		{
			// Between prev and current
			if (block > prev) break;
			// Before current
			if (current == prev) break;
		}
		prev = current;
		current = prev.next;
	}
	if (current)
	{
		// Insert after the current block.
		// Are the blocks adjacent?
		if (current == (Header*)((char*)(block + 1) + block.size))
		{
			// Merge
			block.size += current.size + Header.sizeof;
			block.next = current.next;
		}
		else
		{
			// Chain to current
			block.next = current;
		}
	}
	if (prev == current)
	{
		// Swap new start of free list
		self.free_list = block;
	}
	else
	{
		// Prev adjacent?
		if (block == (Header*)((char*)(prev + 1) + prev.size))
		{
			prev.size += block.size + Header.sizeof;
			prev.next = block.next;
		}
		else
		{
			// Link prev to block
			prev.next = block;
		}
	}
}

union Header @local
{
	struct
	{
		Header* next;
		usz size;
	}
	usz align;
}
