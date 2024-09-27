// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/macos/cf_array.c3
// Permalink: https://github.com/c3lang/c3c/blob/a030985/lib/std/os/macos/cf_array.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::macos::cf @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

distinct CFArrayRef = void*;
distinct CFArrayCallBacksRef = void*;
distinct CFMutableArrayRef = void*;
extern fn CFArrayRef macos_CFArrayCreate(CFAllocatorRef allocator, void** values, CFIndex num_values, CFArrayCallBacksRef callBacks) @extern("CFArrayCreate") @builtin;
extern fn CFArrayRef macos_CFArrayCopy(CFAllocatorRef allocator, CFArrayRef array) @extern("CFArrayCopy") @builtin;
extern fn CFIndex macos_CFArrayGetCount(CFArrayRef array) @extern("CFArrayGetCount") @builtin;
extern fn void macos_CFArrayAppendArray(CFMutableArrayRef theArray, CFArrayRef otherArray, CFRange otherRange) @extern("CFArrayAppendArray") @builtin;
extern fn CFMutableArrayRef macos_CFArrayCreateMutable(CFAllocatorRef allocator, CFIndex capacity, CFArrayCallBacksRef callBacks) @extern("CFArrayCreateMutable") @builtin;
extern fn void macos_CFArrayAppendValue(CFMutableArrayRef theArray, void *value) @extern("CFArrayAppendValue") @builtin;
