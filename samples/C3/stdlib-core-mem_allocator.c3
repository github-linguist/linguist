// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/mem_allocator.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/core/mem_allocator.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::mem::allocator;

const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

struct TrackingEnv
{
	String file;
	String function;
	uint line;
}

enum AllocInitType
{
	NO_ZERO,
	ZERO
}

interface Allocator
{
	fn void reset(usz mark) @optional;
	fn usz mark() @optional;
	/**
     * @require !alignment || math::is_power_of_2(alignment)
     * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
     * @require size > 0
     **/
	fn void*! acquire(usz size, AllocInitType init_type, usz alignment = 0);
	/**
     * @require !alignment || math::is_power_of_2(alignment)
     * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
     * @require ptr != null
     * @require new_size > 0
     **/
	fn void*! resize(void* ptr, usz new_size, usz alignment = 0);
	/**
     * @require ptr != null
     **/
	fn void release(void* ptr, bool aligned);
}

def MemoryAllocFn = fn char[]!(usz);

fault AllocationFailure
{
	OUT_OF_MEMORY,
	CHUNK_TOO_LARGE,
}

fn usz alignment_for_allocation(usz alignment) @inline @private
{
	return alignment < mem::DEFAULT_MEM_ALIGNMENT ? mem::DEFAULT_MEM_ALIGNMENT : alignment;
}

macro void* malloc(Allocator allocator, usz size) @nodiscard
{
	return malloc_try(allocator, size)!!;
}

macro void*! malloc_try(Allocator allocator, usz size) @nodiscard
{
	if (!size) return null;
	$if env::TESTING:
		char* data = allocator.acquire(size, NO_ZERO)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return allocator.acquire(size, NO_ZERO);
	$endif
}

macro void* calloc(Allocator allocator, usz size) @nodiscard
{
	return calloc_try(allocator, size)!!;
}

macro void*! calloc_try(Allocator allocator, usz size) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO);
}

macro void* realloc(Allocator allocator, void* ptr, usz new_size) @nodiscard
{
	return realloc_try(allocator, ptr, new_size)!!;
}

macro void*! realloc_try(Allocator allocator, void* ptr, usz new_size) @nodiscard
{
	if (!new_size)
	{
		free(allocator, ptr);
		return null;
	}
	if (!ptr) return allocator.acquire(new_size, NO_ZERO);
	return allocator.resize(ptr, new_size);
}

macro void free(Allocator allocator, void* ptr)
{
	if (!ptr) return;
	$if env::TESTING:
		((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, false);
}

macro void*! malloc_aligned(Allocator allocator, usz size, usz alignment) @nodiscard
{
	if (!size) return null;
	$if env::TESTING:
		char* data = allocator.acquire(size, NO_ZERO, alignment)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return allocator.acquire(size, NO_ZERO, alignment);
	$endif
}

macro void*! calloc_aligned(Allocator allocator, usz size, usz alignment) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO, alignment);
}

macro void*! realloc_aligned(Allocator allocator, void* ptr, usz new_size, usz alignment) @nodiscard
{
	if (!new_size)
	{
		free_aligned(allocator, ptr);
		return null;
	}
	if (!ptr)
	{
		return malloc_aligned(allocator, new_size, alignment);
	}
	return allocator.resize(ptr, new_size, alignment);
}

macro void free_aligned(Allocator allocator, void* ptr)
{
	if (!ptr) return;
	$if env::TESTING:
	((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, aligned: true);
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 * @require $vacount < 2 : "Too many arguments."
 * @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
 **/
macro new(Allocator allocator, $Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc(allocator, $Type.sizeof);
	$else
		$Type* val = malloc(allocator, $Type.sizeof);
		*val = $vaexpr[0];
		return val;
	$endif
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 * @require $vacount < 2 : "Too many arguments."
 * @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
 **/
macro new_try(Allocator allocator, $Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc_try(allocator, $Type.sizeof);
	$else
		$Type* val = malloc_try(allocator, $Type.sizeof)!;
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
		return ($Type*)calloc_aligned(allocator, $Type.sizeof, $Type.alignof);
	$else
		$Type* val = malloc_aligned(allocator, $Type.sizeof, $Type.alignof);
		*val = $vaexpr[0];
		return val;
	$endif
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT
 **/
macro new_with_padding(Allocator allocator, $Type, usz padding) @nodiscard
{
	return ($Type*)calloc_try(allocator, $Type.sizeof + padding);
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 **/
macro alloc(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc(allocator, $Type.sizeof);
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 **/
macro alloc_try(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type.sizeof);
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
macro alloc_aligned(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_aligned(allocator, $Type.sizeof, $Type.alignof);
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT
 **/
macro alloc_with_padding(Allocator allocator, $Type, usz padding) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type.sizeof + padding);
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 **/
macro new_array(Allocator allocator, $Type, usz elements) @nodiscard
{
	return new_array_try(allocator, $Type, elements)!!;
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 **/
macro new_array_try(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)calloc_try(allocator, $Type.sizeof * elements))[:elements];
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
macro new_array_aligned(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)calloc_aligned(allocator, $Type.sizeof * elements, $Type.alignof))[:elements]!!;
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 **/
macro alloc_array(Allocator allocator, $Type, usz elements) @nodiscard
{
	return alloc_array_try(allocator, $Type, elements)!!;
}

/**
 * Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 * exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 **/
macro alloc_array_aligned(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)malloc_aligned(allocator, $Type.sizeof * elements, $Type.alignof))[:elements]!!;
}

/**
 * @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 **/
macro alloc_array_try(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)malloc_try(allocator, $Type.sizeof * elements))[:elements];
}

macro clone(Allocator allocator, value) @nodiscard
{
	return new(allocator, $typeof(value), value);
}

fn any clone_any(Allocator allocator, any value) @nodiscard
{
	usz size = value.type.sizeof;
	void* data = malloc(allocator, size);
	mem::copy(data, value.ptr, size);
	return any_make(data, value.type);
}


/**
 * @require bytes > 0
 * @require alignment > 0
 * @require bytes <= isz.max
 **/
macro void*! @aligned_alloc(#alloc_fn, usz bytes, usz alignment)
{
	if (alignment < void*.alignof) alignment = void*.alignof;
	usz header = AlignedBlock.sizeof + alignment;
	usz alignsize = bytes + header;
	$if @typekind(#alloc_fn(bytes)) == OPTIONAL:
		void* data = #alloc_fn(alignsize)!;
	$else
		void* data = #alloc_fn(alignsize);
	$endif
	void* mem = mem::aligned_pointer(data + AlignedBlock.sizeof, alignment);
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	assert(mem > data);
	*desc = { bytes, data };
	return mem;
}

struct AlignedBlock
{
	usz len;
	void* start;
}

macro void! @aligned_free(#free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if @typekind(#free_fn(desc.start)) == OPTIONAL:
	#free_fn(desc.start)!;
	$else
	#free_fn(desc.start);
	$endif
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
macro void*! @aligned_realloc(#calloc_fn, #free_fn, void* old_pointer, usz bytes, usz alignment)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	void* data_start = desc.start;
	void* new_data = @aligned_alloc(#calloc_fn, bytes, alignment)!;
	mem::copy(new_data, old_pointer, desc.len < bytes ? desc.len : bytes, 1, 1);
	$if @typekind(#free_fn(data_start)) == OPTIONAL:
	#free_fn(data_start)!;
	$else
	#free_fn(data_start);
	$endif
	return new_data;
}


// All allocators


tlocal Allocator thread_allocator @private = base_allocator();
Allocator temp_base_allocator @private = base_allocator();

tlocal TempAllocator* thread_temp_allocator @private = null;
tlocal TempAllocator*[2] temp_allocator_pair @private;

macro Allocator base_allocator() @private
{
	$if env::LIBC:
		return &allocator::LIBC_ALLOCATOR;
	$else
		return &allocator::NULL_ALLOCATOR;
	$endif
}

macro TempAllocator* create_default_sized_temp_allocator(Allocator allocator) @local
{
	$switch (env::MEMORY_ENV)
	$case NORMAL:
		return new_temp_allocator(1024 * 256, allocator)!!;
	$case SMALL:
		return new_temp_allocator(1024 * 16, allocator)!!;
	$case TINY:
		return new_temp_allocator(1024 * 2, allocator)!!;
	$case NONE:
		unreachable("Temp allocator must explicitly created when memory-env is set to 'none'.");
	$endswitch
}

macro Allocator heap() => thread_allocator;

macro TempAllocator* temp()
{
	if (!thread_temp_allocator)
	{
		init_default_temp_allocators();
	}
	return thread_temp_allocator;
}

fn void init_default_temp_allocators() @private
{
	temp_allocator_pair[0] = create_default_sized_temp_allocator(temp_base_allocator);
	temp_allocator_pair[1] = create_default_sized_temp_allocator(temp_base_allocator);
	thread_temp_allocator = temp_allocator_pair[0];
}

fn void destroy_temp_allocators_after_exit() @finalizer(65535) @local @if(env::LIBC)
{
	destroy_temp_allocators();
}

/**
 * Call this to destroy any memory used by the temp allocators. This will invalidate all temp memory.
 **/
fn void destroy_temp_allocators()
{
	if (!thread_temp_allocator) return;
	temp_allocator_pair[0].destroy();
	temp_allocator_pair[1].destroy();
	temp_allocator_pair[..] = null;
	thread_temp_allocator = null;
}

fn TempAllocator* temp_allocator_next() @private
{
	if (!thread_temp_allocator)
	{
		init_default_temp_allocators();
		return thread_temp_allocator;
	}
	usz index = thread_temp_allocator == temp_allocator_pair[0] ? 1 : 0;
	return thread_temp_allocator = temp_allocator_pair[index];
}

const NullAllocator NULL_ALLOCATOR = {};
distinct NullAllocator (Allocator) = uptr;

fn void*! NullAllocator.acquire(&self, usz bytes, AllocInitType init_type, usz alignment) @dynamic
{
	return AllocationFailure.OUT_OF_MEMORY?;
}

fn void*! NullAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment) @dynamic
{
	return AllocationFailure.OUT_OF_MEMORY?;
}

fn void NullAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
}

