// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/sanitizer/sanitizer.c3
// Permalink: https://github.com/c3lang/c3c/blob/59ed118/lib/std/core/sanitizer/sanitizer.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::sanitizer;

macro void annotate_contiguous_container(void* beg, void* end, void* old_mid, void* new_mid)
{
	$if env::ADDRESS_SANITIZER:
		__sanitizer_annotate_contiguous_container(beg, end, old_mid, new_mid);
	$endif
}

macro void annotate_double_ended_contiguous_container(void* storage_beg, void* storage_end, void* old_container_beg, void* old_container_end, void* new_container_beg, void* new_container_end)
{
	$if env::ADDRESS_SANITIZER:
		__sanitizer_annotate_double_ended_contiguous_container(storage_beg, storage_end, old_container_beg, old_container_end, new_container_beg, new_container_end);
	$endif
}

macro void print_stack_trace()
{
	$if env::ADDRESS_SANITIZER:
		__sanitizer_print_stack_trace();
	$endif
}

fn void set_death_callback(VoidFn callback)
{
	$if env::ANY_SANITIZER:
		__sanitizer_set_death_callback(callback);
	$endif
}

module std::core::sanitizer @if (env::ANY_SANITIZER);

struct __Sanitizer_sandbox_arguments
{
	CInt coverage_sandboxed;
	iptr coverage_fd;
	CUInt coverage_max_block_size;
}

extern fn void __sanitizer_set_report_path(ZString path);
extern fn void __sanitizer_set_report_fd(void* fd);
extern fn ZString __sanitizer_get_report_path();
extern fn void __sanitizer_sandbox_on_notify(__Sanitizer_sandbox_arguments* args);
extern fn void __sanitizer_report_error_summary(ZString error_summary);
extern fn ushort __sanitizer_unaligned_load16(void* p);
extern fn uint __sanitizer_unaligned_load32(void* p);
extern fn ulong __sanitizer_unaligned_load64(void* p);
extern fn void __sanitizer_unaligned_store16(void* p, ushort x);
extern fn void __sanitizer_unaligned_store32(void* p, uint x);
extern fn void __sanitizer_unaligned_store64(void* p, ulong x);
extern fn CInt __sanitizer_acquire_crash_state();
extern fn void __sanitizer_annotate_contiguous_container(void* beg, void* end, void* old_mid, void* new_mid);
extern fn void __sanitizer_annotate_double_ended_contiguous_container(void* storage_beg, void* storage_end,
	void* old_container_beg, void* old_container_end,
	void* new_container_beg, void* new_container_end);
extern fn CInt __sanitizer_verify_contiguous_container(void* beg, void* mid, void* end);
extern fn CInt __sanitizer_verify_double_ended_contiguous_container(
	void* storage_beg, void* container_beg,
	void* container_end, void* storage_end);
extern fn void* __sanitizer_contiguous_container_find_bad_address(void* beg, void* mid, void* end);
extern fn void* __sanitizer_double_ended_contiguous_container_find_bad_address(
	void* storage_beg, void* container_beg,
	void* container_end, void* storage_end);

extern fn void __sanitizer_print_stack_trace();
extern fn void __sanitizer_symbolize_pc(void* pc, ZString fmt, char* out_buf, usz out_buf_size);
extern fn void __sanitizer_symbolize_global(void* data_ptr, ZString fmt, char* out_buf, usz out_buf_size);
extern fn void __sanitizer_set_death_callback(VoidFn callback);
extern fn void __sanitizer_weak_hook_memcmp(void* called_pc, void* s1, void* s2, usz n, CInt result);
extern fn void __sanitizer_weak_hook_strncmp(void* called_pc, ZString s1, ZString s2, usz n, CInt result);
extern fn void __sanitizer_weak_hook_strncasecmp(void* called_pc, ZString s1, ZString s2, usz n, CInt result);
extern fn void __sanitizer_weak_hook_strcmp(void* called_pc, ZString s1, ZString s2, CInt result);
extern fn void __sanitizer_weak_hook_strcasecmp(void* called_pc, ZString s1, ZString s2, CInt result);
extern fn void __sanitizer_weak_hook_strstr(void* called_pc, ZString s1, ZString s2, char* result);
extern fn void __sanitizer_weak_hook_strcasestr(void* called_pc, ZString s1, ZString s2, char* result);
extern fn void __sanitizer_weak_hook_memmem(void* called_pc, void* s1, usz len1, void* s2, usz len2, void* result);
extern fn void __sanitizer_print_memory_profile(usz top_percent, usz max_number_of_contexts);
extern fn void __sanitizer_start_switch_fiber(void** fake_stack_save, void* bottom, usz size);
extern fn void __sanitizer_finish_switch_fiber(void* fake_stack_save, void** bottom_old, usz* size_old);
extern fn CInt __sanitizer_get_module_and_offset_for_pc(void* pc, char* module_path, usz module_path_len, void** pc_offset);
