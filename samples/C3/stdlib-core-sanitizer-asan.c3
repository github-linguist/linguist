// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/sanitizer/asan.c3
// Permalink: https://github.com/c3lang/c3c/blob/59ed118/lib/std/core/sanitizer/asan.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Add this to your code to suppress leak detection or set other default options
// fn ZString __asan_default_options() @export("__asan_default_options") @if(env::ADDRESS_SANITIZER)
// {
//   return "detect_leaks=0";
// }

// Add this to break on error
// asan::set_error_report_callback(fn (ZString err)
// {
//   breakpoint();
// });

module std::core::sanitizer::asan;

def ErrorCallback = fn void (ZString);

/**
 * Marks a memory region ([addr, addr+size)) as unaddressable.
 *
 * This memory must be previously allocated by your program. Instrumented
 * code is forbidden from accessing addresses in this region until it is
 * unpoisoned. This function is not guaranteed to poison the entire region -
 * it could poison only a subregion of [addr, addr+size) due to ASan
 * alignment restrictions.
 *
 * NOTE This function is not thread-safe because no two threads can poison or
 * unpoison memory in the same memory region simultaneously.
 *
 * @param addr "Start of memory region."
 * @param size "Size of memory region."
 **/
macro poison_memory_region(void* addr, usz size)
{
	$if env::ADDRESS_SANITIZER: 
		__asan_poison_memory_region(addr, size);
	$endif 
}

/**
 * Marks a memory region ([addr, addr+size)) as addressable.
 *
 * This memory must be previously allocated by your program. Accessing
 * addresses in this region is allowed until this region is poisoned again.
 * This function could unpoison a super-region of [addr, addr+size) due
 * to ASan alignment restrictions.
 *
 * NOTE This function is not thread-safe because no two threads can
 * poison or unpoison memory in the same memory region simultaneously.
 *
 * @param addr "Start of memory region."
 * @param size "Size of memory region."
 **/
macro unpoison_memory_region(void* addr, usz size)
{
	$if env::ADDRESS_SANITIZER: 
		__asan_unpoison_memory_region(addr, size);
	$endif 
}

/**
 * Checks if an address is poisoned.
 * @return "True if 'addr' is poisoned (that is, 1-byte read/write access to this address would result in an error report from ASan). Otherwise returns false."
 * @param addr "Address to check."
 **/
macro bool address_is_poisoned(void* addr)
{
	$if env::ADDRESS_SANITIZER:
		return (bool)__asan_address_is_poisoned(addr);
	$else
		return false;
	$endif
}

/**
 * Checks if a region is poisoned.
 *
 * If at least one byte in [beg, beg+size) is poisoned, returns the
 * address of the first such byte. Otherwise returns 0.
 *
 * @param beg "Start of memory region."
 * @param size "Start of memory region."
 * @return "Address of first poisoned byte."
 **/
macro void* region_is_poisoned(void* beg, usz size)
{
	$if env::ADDRESS_SANITIZER:
		return __asan_region_is_poisoned(addr);
	$else
		return null;
	$endif
}

/**
 * Sets the callback function to be called during ASan error reporting.
 **/
fn void set_error_report_callback(ErrorCallback callback)
{
	$if env::ADDRESS_SANITIZER:
		__asan_set_error_report_callback(callback);
	$endif
}

module std::core::sanitizer::asan @if(env::ADDRESS_SANITIZER);

extern fn void __asan_poison_memory_region(void* addr, usz size);
extern fn void __asan_unpoison_memory_region(void* addr, usz size);
extern fn CInt __asan_address_is_poisoned(void* addr);
extern fn void* __asan_region_is_poisoned(void* beg, usz size);
extern fn void __asan_describe_address(void* addr);
extern fn CInt __asan_report_present();
extern fn void* __asan_get_report_pc();
extern fn void* __asan_get_report_bp();
extern fn void* __asan_get_report_sp();
extern fn void* __asan_get_report_address();
extern fn CInt __asan_get_report_access_type();
extern fn usz __asan_get_report_access_size();
extern fn ZString __asan_get_report_description();
extern fn ZString __asan_locate_address(void* addr, char* name, usz name_size, void** region_address, usz* region_size);
extern fn usz __asan_get_alloc_stack(void* addr, void** trace, usz size, CInt* thread_id);
extern fn usz __asan_get_free_stack(void* addr, void** trace, usz size, CInt* thread_id);
extern fn void __asan_get_shadow_mapping(usz* shadow_scale, usz* shadow_offset);
extern fn void __asan_set_error_report_callback(ErrorCallback callback);
extern fn void __asan_print_accumulated_stats();
extern fn void* __asan_get_current_fake_stack();
extern fn void* __asan_addr_is_in_fake_stack(void* fake_stack, void* addr, void** beg, void** end);
extern fn void __asan_handle_no_return();
extern fn CInt __asan_update_allocation_context(void* addr);
