// Source: https://github.com/c3lang/c3c/blob/master/lib/std/libc/os/errno.c3
// Permalink: https://github.com/c3lang/c3c/blob/68f6cb1/lib/std/libc/os/errno.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module libc::os;

// Linux
extern fn int* __errno_location() @if(env::LINUX);
macro int errno() @if(env::LINUX) => *__errno_location();
macro void errno_set(int err) @if(env::LINUX) => *(__errno_location()) = err;

// Darwin
extern fn int* __error() @if(env::DARWIN);
macro int errno() @if(env::DARWIN) => *__error();
macro void errno_set(int err) @if(env::DARWIN) => *(__error()) = err;

// Win32
macro int errno() @if(env::WIN32)
{
	int holder;
	_get_errno(&holder);
	return holder;
}
macro void errno_set(int err) @if(env::WIN32) => _set_errno(err);
extern fn void _get_errno(int* result) @if(env::WIN32);
extern fn void _set_errno(int err) @if(env::WIN32);

// Default
const ERRNO_DEFAULT @local = !env::LINUX && !env::DARWIN && !env::WIN32;
tlocal int _errno_c3 @if(ERRNO_DEFAULT) = 0;
fn void errno_set(int err) @if(ERRNO_DEFAULT) => _errno_c3 = err;
fn int errno() @if(ERRNO_DEFAULT) => _errno_c3;