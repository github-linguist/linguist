// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/allocators/libc_allocator.c3
// Permalink: https://github.com/c3lang/c3c/blob/2600c31/lib/std/core/allocators/libc_allocator.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator @if(env::LIBC);
import libc;

const LibcAllocator LIBC_ALLOCATOR = {};
distinct LibcAllocator (Allocator) = uptr;

module std::core::mem::allocator @if(env::POSIX);
import std::os;
import libc;

fn void*! LibcAllocator.acquire(&self, usz bytes, AllocInitType init_type, usz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		void* data @noinit;
		if (alignment > mem::DEFAULT_MEM_ALIGNMENT)
		{
			if (posix::posix_memalign(&data, alignment, bytes)) return AllocationFailure.OUT_OF_MEMORY?;
			mem::clear(data, bytes, mem::DEFAULT_MEM_ALIGNMENT);
			return data;
		}
        return libc::calloc(1, bytes) ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	else
	{
		void* data @noinit;
		if (alignment > mem::DEFAULT_MEM_ALIGNMENT)
		{
			if (posix::posix_memalign(&data, alignment, bytes)) return AllocationFailure.OUT_OF_MEMORY?;
		}
		else
		{
			if (!(data = libc::malloc(bytes))) return AllocationFailure.OUT_OF_MEMORY?;
		}
        $if env::TESTING:
		    for (usz i = 0; i < bytes; i++) ((char*)data)[i] = 0xAA;
        $endif
        return data;
	}
}

fn void*! LibcAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment) @dynamic
{
	if (alignment <= mem::DEFAULT_MEM_ALIGNMENT) return libc::realloc(old_ptr, new_bytes) ?: AllocationFailure.OUT_OF_MEMORY?;
    void* new_ptr;
	if (posix::posix_memalign(&new_ptr, alignment, new_bytes)) return AllocationFailure.OUT_OF_MEMORY?;

	$switch
	$case env::DARWIN:
		usz old_usable_size = darwin::malloc_size(old_ptr);
	$case env::LINUX:
		usz old_usable_size = linux::malloc_usable_size(old_ptr);
	$default:
		usz old_usable_size = new_bytes;
	$endswitch

	usz copy_size = new_bytes < old_usable_size ? new_bytes : old_usable_size;
	mem::copy(new_ptr, old_ptr, copy_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	libc::free(old_ptr);
	return new_ptr;
}

fn void LibcAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
	libc::free(old_ptr);
}

module std::core::mem::allocator @if(env::WIN32);
import std::os::win32;
import libc;

fn void*! LibcAllocator.acquire(&self, usz bytes, AllocInitType init_type, usz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		if (alignment > 0)
		{
			return win32::_aligned_recalloc(null, 1, bytes, alignment) ?: AllocationFailure.OUT_OF_MEMORY?;
		}
		return libc::calloc(1, bytes) ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	void* data = alignment > 0 ? win32::_aligned_malloc(bytes, alignment) : libc::malloc(bytes);
	if (!data) return AllocationFailure.OUT_OF_MEMORY?;
    $if env::TESTING:
		for (usz i = 0; i < bytes; i++) ((char*)data)[i] = 0xAA;
	$endif
    return data;
}

fn void*! LibcAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment) @dynamic
{
	if (alignment)
	{
		return win32::_aligned_realloc(old_ptr, new_bytes, alignment) ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	return libc::realloc(old_ptr, new_bytes) ?: AllocationFailure.OUT_OF_MEMORY?;
}

fn void LibcAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
	if (aligned)
	{
    	win32::_aligned_free(old_ptr);
    	return;
	}
	libc::free(old_ptr);
}

module std::core::mem::allocator @if(!env::WIN32 && !env::POSIX && env::LIBC);
import libc;

fn void*! LibcAllocator.acquire(&self, usz bytes, AllocInitType init_type, usz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		void* data = alignment ? @aligned_alloc(fn void*(usz bytes) => libc::calloc(bytes, 1), bytes, alignment)!! : libc::calloc(bytes, 1);
        return data ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	else
	{
		void* data = alignment ? @aligned_alloc(libc::malloc, bytes, alignment)!! : libc::malloc(bytes);
        if (!data) return AllocationFailure.OUT_OF_MEMORY?;
        $if env::TESTING:
		    for (usz i = 0; i < bytes; i++) ((char*)data)[i] = 0xAA;
        $endif
        return data;
	}
}


fn void*! LibcAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment) @dynamic
{
	if (alignment)
	{
		void* data = @aligned_realloc(fn void*(usz bytes) => libc::malloc(bytes), libc::free, old_ptr, new_bytes, alignment)!!;
		return data ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	return libc::realloc(old_ptr, new_bytes) ?: AllocationFailure.OUT_OF_MEMORY?;
}


fn void LibcAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
	if (aligned)
	{
		@aligned_free(libc::free, old_ptr)!!;
	}
	else
	{
		libc::free(old_ptr);
	}
}
