// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/macos/core_foundation.c3
// Permalink: https://github.com/c3lang/c3c/blob/a030985/lib/std/os/macos/core_foundation.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::macos::cf @if(env::DARWIN) @link(env::DARWIN, "CoreFoundation.framework");

distinct CFTypeRef = void*;
def CFIndex = isz;
struct CFRange
{
	CFIndex location;
	CFIndex length;
}

extern fn CFTypeRef macos_CFRetain(CFTypeRef cf) @extern("CFRetain") @builtin;
extern fn void macos_CFRelease(CFTypeRef cf) @extern("CFRelease") @builtin;