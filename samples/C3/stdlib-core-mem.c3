// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/mem.c3
// Permalink: https://github.com/c3lang/c3c/blob/26acce2/lib/std/core/mem.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem;
import std::core::mem::allocator @public;
import std::math;

const MAX_MEMORY_ALIGNMENT = 0x1000_0000;
const DEFAULT_MEM_ALIGNMENT = (void*.alignof) * 2;

macro bool @constant_is_power_of_2($x) @const @private
{
	return $x != 0 && ($x & ($x - 1)) == 0;
}

/**
 * Load a vector from memory according to a mask assuming default alignment.
 *
 * @param ptr "The pointer address to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 * @require $assignable(&&passthru, $typeof(ptr)) : "Pointer and passthru must match"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro masked_load(ptr, bool[<*>] mask, passthru)
{
	return $$masked_load(ptr, mask, passthru, 0);
}

/**
 * Load a vector from memory according to a mask.
 *
 * @param ptr "The pointer address to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 * @param $alignment "The alignment to assume for the pointer"
 *
 * @require $assignable(&&passthru, $typeof(ptr)) : "Pointer and passthru must match"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 * @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro @masked_load_aligned(ptr, bool[<*>] mask, passthru, usz $alignment)
{
	return $$masked_load(ptr, mask, passthru, $alignment);
}

/**
 * Load values from a pointer vector, assuming default alignment.
 *
 * @param ptrvec "The vector of pointers to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 *
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require $assignable(&&passthru[0], $typeof(ptrvec[0])) : "Pointer and passthru must match"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro gather(ptrvec, bool[<*>] mask, passthru)
{
	return $$gather(ptrvec, mask, passthru, 0);
}


/**
 * Load values from a pointer vector.
 *
 * @param ptrvec "The vector of pointers to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 * @param $alignment "The alignment to assume for the pointers"
 *
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require $assignable(&&passthru[0], $typeof(ptrvec[0])) : "Pointer and passthru must match"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 * @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro @gather_aligned(ptrvec, bool[<*>] mask, passthru, usz $alignment)
{
	return $$gather(ptrvec, mask, passthru, $alignment);
}


/**
 * Store parts of a vector according to the mask, assuming default alignment.
 *
 * @param ptr "The pointer address to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 *
 * @require $assignable(&&value, $typeof(ptr)) : "Pointer and value must match"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require value.len == mask.len : "Mask and value must have the same length"
 **/
macro masked_store(ptr, value, bool[<*>] mask)
{
	return $$masked_store(ptr, value, mask, 0);
}

/**
 * @param ptr "The pointer address to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 * @param $alignment "The alignment of the pointer"
 *
 * @require $assignable(&&value, $typeof(ptr)) : "Pointer and value must match"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require value.len == mask.len : "Mask and value must have the same length"
 * @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
 *
 **/
macro @masked_store_aligned(ptr, value, bool[<*>] mask, usz $alignment)
{
	return $$masked_store(ptr, value, mask, $alignment);
}

/**
 * @param ptrvec "The vector pointer containing the addresses to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require $assignable(&&value[0], $typeof(ptrvec[0])) : "Pointer and value must match"
 * @require value.len == mask.len : "Mask and value must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 *
 **/
macro scatter(ptrvec, value, bool[<*>] mask)
{
	return $$scatter(ptrvec, value, mask, 0);
}

/**
 * @param ptrvec "The vector pointer containing the addresses to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 * @param $alignment "The alignment of the load"
 *
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require $assignable(&&value[0], $typeof(ptrvec[0])) : "Pointer and value must match"
 * @require value.len == mask.len : "Mask and value must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 * @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
 **/
macro @scatter_aligned(ptrvec, value, bool[<*>] mask, usz $alignment)
{
	return $$scatter(ptrvec, value, mask, $alignment);
}

/**
 * @param [in] x "The variable or dereferenced pointer to load."
 * @param $alignment "The alignment to assume for the load"
 * @return "The value of x"
 *
 * @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
 **/
macro @unaligned_load(&x, usz $alignment) @builtin
{
	return $$unaligned_load(x, $alignment);
}

/**
 * @param [out] x "The variable or dereferenced pointer to store to."
 * @param value "The value to store."
 * @param $alignment "The alignment to assume for the store"
 * @return "The value of x"
 *
 * @require $assignable(value, $typeof(*x)) : "The value doesn't match the variable"
 * @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
 **/
macro @unaligned_store(&x, value, usz $alignment) @builtin
{
	return $$unaligned_store(x, ($typeof(*x))value, $alignment);
}

macro @volatile_load(&x) @builtin
{
	return $$volatile_load(x);
}

/**
 * @require $assignable(y, $typeof(*x)) : "The value doesn't match the variable"
 **/
macro @volatile_store(&x, y) @builtin
{
	return $$volatile_store(x, ($typeof(*x))y);
}

enum AtomicOrdering : int
{
	NOT_ATOMIC,         // Not atomic
	UNORDERED,          // No lock
	RELAXED,            // Consistent ordering
	ACQUIRE,            // Barrier locking load/store
	RELEASE,            // Barrier releasing load/store
	ACQUIRE_RELEASE,    // Barrier fence to load/store
	SEQ_CONSISTENT,     // Acquire semantics, ordered with other seq_consistent
}

/**
 * @param [in] x "the variable or dereferenced pointer to load."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @param $volatile "whether the load should be volatile, defaults to 'false'"
 * @return "returns the value of x"
 *
 * @require $ordering != AtomicOrdering.RELEASE "Release ordering is not valid for load."
 * @require $ordering != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid for load."
 * @require types::may_load_atomic($typeof(x)) "Only integer, float and pointers may be used."
 * @require @typekind(x) == POINTER "You can only load from a pointer"
 **/
macro @atomic_load(&x, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	return $$atomic_load(x, $volatile, (int)$ordering);
}

/**
 * @param [out] x "the variable or dereferenced pointer to store to."
 * @param value "the value to store."
 * @param $ordering "the atomic ordering of the store, defaults to SEQ_CONSISTENT"
 * @param $volatile "whether the store should be volatile, defaults to 'false'"
 *
 * @require $ordering != AtomicOrdering.ACQUIRE "Acquire ordering is not valid for store."
 * @require $ordering != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid for store."
 * @require types::may_load_atomic($typeof(x)) "Only integer, float and pointers may be used."
 **/
macro void @atomic_store(&x, value, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	$$atomic_store(x, value, $volatile, (int)$ordering);
}

/**
 * @require $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 * @require $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid."
 **/
macro compare_exchange(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT, bool $volatile = true, bool $weak = false, usz $alignment = 0)
{
	return $$compare_exchange(ptr, compare, value, $volatile, $weak, $success.ordinal, $failure.ordinal, $alignment);
}

/**
 * @require $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 * @require $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid."
 **/
macro compare_exchange_volatile(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT)
{
	return compare_exchange(ptr, compare, value, $success, $failure, true);
}

/**
 * @require math::is_power_of_2(alignment)
 **/
fn usz aligned_offset(usz offset, usz alignment)
{
	return alignment * ((offset + alignment - 1) / alignment);
}

macro void* aligned_pointer(void* ptr, usz alignment)
{
	return (void*)(uptr)aligned_offset((uptr)ptr, alignment);
}

/**
 * @require math::is_power_of_2(alignment)
 **/
fn bool ptr_is_aligned(void* ptr, usz alignment) @inline
{
	return (uptr)ptr & ((uptr)alignment - 1) == 0;
}

macro void clear(void* dst, usz len, usz $dst_align = 0, bool $is_volatile = false, bool $inlined = false)
{
	$$memset(dst, (char)0, len, $is_volatile, $dst_align);
}

macro void clear_inline(void* dst, usz $len, usz $dst_align = 0, bool $is_volatile = false)
{
	$$memset_inline(dst, (char)0, $len, $is_volatile, $dst_align);
}

/**
 * Copy memory from src to dst efficiently, assuming the memory ranges do not overlap.
 *
 * @param [&out] dst "The destination to copy to"
 * @param [&in] src "The source to copy from"
 * @param len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $src_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 *
 * @require len == 0 || dst + len <= src || src + len <= dst : "Ranges may not overlap"
 **/
macro void copy(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false, bool $inlined = false)
{
	$$memcpy(dst, src, len, $is_volatile, $dst_align, $src_align);
}

/**
 * Copy memory from src to dst efficiently, assuming the memory ranges do not overlap, it
 * will always be inlined and never call memcopy
 *
 * @param [&out] dst "The destination to copy to"
 * @param [&in] src "The source to copy from"
 * @param $len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $src_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 *
 * @require $len == 0 || dst + $len <= src || src + $len <= dst : "Ranges may not overlap"
 **/
macro void copy_inline(void* dst, void* src, usz $len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false)
{
	$$memcpy_inline(dst, src, $len, $is_volatile, $dst_align, $src_align);
}

/**
 * Copy memory from src to dst but correctly handle the possibility of overlapping ranges.
 *
 * @param [&out] dst "The destination to copy to"
 * @param [&in] src "The source to copy from"
 * @param len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $src_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 **/
macro void move(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false)
{
	$$memmove(dst, src, len, $is_volatile, $dst_align, $src_align);
}

/**
 * Sets all memory in a region to that of the provided byte.
 *
 * @param [&out] dst "The destination to copy to"
 * @param val "The value to copy into memory"
 * @param len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 *
 * @ensure !len || (dst[0] == val && dst[len - 1] == val)
 **/
macro void set(void* dst, char val, usz len, usz $dst_align = 0, bool $is_volatile = false)
{
	$$memset(dst, val, len, $is_volatile, $dst_align);
}

/**
 * Sets all memory in a region to that of the provided byte. Never calls OS memset.
 *
 * @param [&out] dst "The destination to copy to"
 * @param val "The value to copy into memory"
 * @param $len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 *
 * @ensure !$len || (dst[0] == val && dst[$len - 1] == val)
 **/
macro void set_inline(void* dst, char val, usz $len, usz $dst_align = 0, bool $is_volatile = false)
{
	$$memset_inline(dst, val, $len, $is_volatile, $dst_align);
}
/**
 * @require values::@inner_kind(a) == TypeKind.SLICE || values::@inner_kind(a) == TypeKind.POINTER
 * @require values::@inner_kind(b) == TypeKind.SLICE || values::@inner_kind(b) == TypeKind.POINTER
 * @require values::@inner_kind(a) != TypeKind.SLICE || len == -1
 * @require values::@inner_kind(a) != TypeKind.POINTER || len > -1
 * @require values::@assign_to(a, b) && values::@assign_to(b, a)
 **/
macro bool equals(a, b, isz len = -1, usz $align = 0)
{
	$if !$align:
		$align = $typeof(a[0]).alignof;
	$endif
	void* x @noinit;
	void* y @noinit;
	$if values::@inner_kind(a) == TypeKind.SLICE:
		len = a.len;
		if (len != b.len) return false;
		x = a.ptr;
		y = b.ptr;
	$else
		x = a;
		y = b;
		assert(len >= 0, "A zero or positive length must be given when comparing pointers.");
	$endif

	if (!len) return true;
	var $Type;
	$switch ($align)
		$case 1:
			$Type = char;
		$case 2:
			$Type = ushort;
		$case 4:
			$Type = uint;
		$case 8:
		$default:
			$Type = ulong;
	$endswitch
	var $step = $Type.sizeof;
	usz end = len / $step;
	for (usz i = 0; i < end; i++)
	{
		if ((($Type*)x)[i] != (($Type*)y)[i]) return false;
	}
	usz last = len % $align;
	for (usz i = len - last; i < len; i++)
	{
		if (((char*)x)[i] != ((char*)y)[i]) return false;
	}
	return true;
}


macro type_alloc_must_be_aligned($Type)
{
	return $Type.alignof > DEFAULT_MEM_ALIGNMENT;
}

/**
 * Run with a specific allocator inside of the macro body.
 **/
macro void @scoped(Allocator allocator; @body())
{
	Allocator old_allocator = allocator::thread_allocator;
	allocator::thread_allocator = allocator;
	defer allocator::thread_allocator = old_allocator;
	@body();
}

macro void @report_heap_allocs_in_scope(;@body())
{
	TrackingAllocator tracker;
	tracker.init(allocator::thread_allocator);
	Allocator old_allocator = allocator::thread_allocator;
	allocator::thread_allocator = &tracker;
	defer
	{
		allocator::thread_allocator = old_allocator;
		tracker.print_report();
		tracker.free();
	}
	@body();
}

macro void @stack_mem(usz $size; @body(Allocator mem)) @builtin
{
	char[$size] buffer;
	OnStackAllocator allocator;
	allocator.init(&buffer, allocator::heap());
	defer allocator.free();
	@body(&allocator);
}

macro void @stack_pool(usz $size; @body) @builtin
{
	char[$size] buffer;
	OnStackAllocator allocator;
	allocator.init(&buffer, allocator::heap());
	defer allocator.free();
	mem::@scoped(&allocator)
	{
		@body();
	};
}

struct TempState
{
	TempAllocator* old;
	TempAllocator* current;
	usz mark;
}

/**
 * Push the current temp allocator. A push must always be balanced with a pop using the current state.
 **/
fn TempState temp_push(TempAllocator* other = null)
{
	TempAllocator* current = allocator::temp();
	TempAllocator* old = current;
	if (other == current)
	{
		current = allocator::temp_allocator_next();
	}
	return { old, current, current.used };
}

/**
 * Pop the current temp allocator. A pop must always be balanced with a push.
 **/
fn void temp_pop(TempState old_state)
{
	assert(allocator::thread_temp_allocator == old_state.current, "Tried to pop temp allocators out of order.");
	assert(old_state.current.used >= old_state.mark, "Tried to pop temp allocators out of order.");
	old_state.current.reset(old_state.mark);
	allocator::thread_temp_allocator = old_state.old;
}

macro void @pool(TempAllocator* #other_temp = null; @body) @builtin
{
	TempAllocator* current = allocator::temp();
	var $has_arg = !$is_const(#other_temp);
	$if $has_arg:
		TempAllocator* original = current;
		if (current == (void*)#other_temp) current = allocator::temp_allocator_next();
	$endif
	usz mark = current.used;
	defer
	{
		current.reset(mark);
		$if $has_arg:
			allocator::thread_temp_allocator = original;
		$endif;
	}
	@body();
}

import libc;


module std::core::mem @if(WASM_NOLIBC);
import std::core::mem::allocator @public;
SimpleHeapAllocator wasm_allocator @private;
extern int __heap_base;

fn void initialize_wasm_mem() @init(1024) @private
{
	allocator::wasm_memory.allocate_block(mem::DEFAULT_MEM_ALIGNMENT)!!; // Give us a valid null.
	// Check if we need to move the heap.
	uptr start = (uptr)&__heap_base;
	if (start > mem::DEFAULT_MEM_ALIGNMENT) allocator::wasm_memory.use = start;
	wasm_allocator.init(fn (x) => allocator::wasm_memory.allocate_block(x));
	allocator::thread_allocator = &wasm_allocator;
	allocator::temp_base_allocator = &wasm_allocator;
	allocator::init_default_temp_allocators();
}

module std::core::mem;


macro TrackingEnv* get_tracking_env()
{
	$if env::TRACK_MEMORY:
		return &&TrackingEnv { $$FILE, $$FUNC, $$LINE };
	$else
		return null;
	$endif
}

macro @clone(value) @builtin @nodiscard
{
	return allocator::clone(allocator::heap(), value);
}

macro @tclone(value) @builtin @nodiscard
{
	return temp_new($typeof(value), value);
}

fn void* malloc(usz size) @builtin @inline @nodiscard
{
	return allocator::malloc(allocator::heap(), size);
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
fn void* malloc_aligned(usz size, usz alignment) @builtin @inline @nodiscard
{
	return allocator::malloc_aligned(allocator::heap(), size, alignment)!!;
}

fn void* tmalloc(usz size, usz alignment = 0) @builtin @inline @nodiscard
{
	if (!size) return null;
	return allocator::temp().acquire(size, NO_ZERO, alignment)!!;
}

/**
 * @require $vacount < 2 : "Too many arguments."
 * @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
 * @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 **/
macro new($Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc($Type.sizeof);
	$else
		$Type* val = malloc($Type.sizeof);
		*val = $vaexpr[0];
		return val;
	$endif
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 * @require $vacount < 2 : "Too many arguments."
 * @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
 **/
macro new_aligned($Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc_aligned($Type.sizeof, $Type.alignof);
	$else
		$Type* val = malloc_aligned($Type.sizeof, $Type.alignof);
		*val = $vaexpr[0];
		return val;
	$endif
}

/**
 * @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 **/
macro alloc($Type) @nodiscard
{
	return ($Type*)malloc($Type.sizeof);
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
macro alloc_aligned($Type) @nodiscard
{
	return ($Type*)malloc_aligned($Type.sizeof, $Type.alignof);
}

/**
 * @require $vacount < 2 : "Too many arguments."
 * @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
 **/
macro temp_new($Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)tcalloc($Type.sizeof) @inline;
	$else
		$Type* val = tmalloc($Type.sizeof) @inline;
		*val = $vaexpr[0];
		return val;
	$endif
}

macro temp_alloc($Type) @nodiscard
{
	return tmalloc($Type.sizeof);
}


/**
 * @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 **/
macro new_array($Type, usz elements) @nodiscard
{
	return allocator::new_array(allocator::heap(), $Type, elements);
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
macro new_array_aligned($Type, usz elements) @nodiscard
{
	return allocator::new_array_aligned(allocator::heap(), $Type, elements);
}

/**
 * @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 **/
macro alloc_array($Type, usz elements) @nodiscard
{
	return allocator::alloc_array(allocator::heap(), $Type, elements);
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
macro alloc_array_aligned($Type, usz elements) @nodiscard
{
	return allocator::alloc_array_aligned(allocator::heap(), $Type, elements);
}

macro temp_alloc_array($Type, usz elements) @nodiscard
{
	return (($Type*)tmalloc($Type.sizeof * elements, $Type.alignof))[:elements];
}

macro temp_new_array($Type, usz elements) @nodiscard
{
	return (($Type*)tcalloc($Type.sizeof * elements, $Type.alignof))[:elements];
}

fn void* calloc(usz size) @builtin @inline @nodiscard
{
	return allocator::calloc(allocator::heap(), size);
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
fn void* calloc_aligned(usz size, usz alignment) @builtin @inline @nodiscard
{
	return allocator::calloc_aligned(allocator::heap(), size, alignment)!!;
}

fn void* tcalloc(usz size, usz alignment = 0) @builtin @inline @nodiscard
{
	if (!size) return null;
	return allocator::temp().acquire(size, ZERO, alignment)!!;
}

fn void* realloc(void *ptr, usz new_size) @builtin @inline @nodiscard
{
	return allocator::realloc(allocator::heap(), ptr, new_size);
}

fn void* realloc_aligned(void *ptr, usz new_size, usz alignment) @builtin @inline @nodiscard
{
	return allocator::realloc_aligned(allocator::heap(), ptr, new_size, alignment)!!;
}

fn void free(void* ptr) @builtin @inline
{
	return allocator::free(allocator::heap(), ptr);
}

fn void free_aligned(void* ptr) @builtin @inline
{
	return allocator::free_aligned(allocator::heap(), ptr);
}

fn void* trealloc(void* ptr, usz size, usz alignment = mem::DEFAULT_MEM_ALIGNMENT) @builtin @inline @nodiscard
{
	if (!size) return null;
    if (!ptr) return tmalloc(size, alignment);
	return allocator::temp().resize(ptr, size, alignment)!!;
}

