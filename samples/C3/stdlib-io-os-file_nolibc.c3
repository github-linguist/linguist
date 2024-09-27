// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/file_nolibc.c3
// Permalink: https://github.com/c3lang/c3c/blob/59b0772/lib/std/io/os/file_nolibc.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os @if(env::NO_LIBC);
import libc;

def FopenFn = fn void*!(String, String);
def FreopenFn = fn void*!(void*, String, String);
def FcloseFn = fn void!(void*);
def FseekFn = fn void!(void*, isz, Seek);
def FtellFn = fn usz!(void*);
def FwriteFn = fn usz!(void*, char[] buffer);
def FreadFn = fn usz!(void*, char[] buffer);
def RemoveFn = fn void!(String);

FopenFn native_fopen_fn @weak @if(!$defined(native_fopen_fn));
FcloseFn native_fclose_fn @weak @if(!$defined(native_fclose_fn));
FreopenFn native_freopen_fn @weak @if(!$defined(native_freopen_fn));
FseekFn native_fseek_fn @weak @if(!$defined(native_fseek_fn));
FtellFn native_ftell_fn @weak @if(!$defined(native_ftell_fn));
FwriteFn native_fwrite_fn @weak @if(!$defined(native_fwrite_fn));
FreadFn native_fread_fn @weak @if(!$defined(native_fread_fn));
RemoveFn native_remove_fn @weak @if(!$defined(native_remove_fn));

/**
 * @require mode.len > 0
 * @require filename.len > 0
 **/
fn void*! native_fopen(String filename, String mode) @inline
{
	if (native_fopen_fn) return native_fopen_fn(filename, mode);
	return IoError.UNSUPPORTED_OPERATION?;
}

/**
 * Delete a file.
 *
 * @require filename.len > 0
 **/
fn void! native_remove(String filename) @inline
{
	if (native_remove_fn) return native_remove_fn(filename);
	return IoError.UNSUPPORTED_OPERATION?;
}

/**
 * @require mode.len > 0
 * @require filename.len > 0
 **/
fn void*! native_freopen(void* file, String filename, String mode) @inline
{
	if (native_freopen_fn) return native_freopen_fn(file, filename, mode);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn void! native_fseek(void* file, isz offset, Seek seek_mode) @inline
{
	if (native_fseek_fn) return native_fseek_fn(file, offset, seek_mode);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn usz! native_ftell(CFile file) @inline
{
	if (native_ftell_fn) return native_ftell_fn(file);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn usz! native_fwrite(CFile file, char[] buffer) @inline
{
	if (native_fwrite_fn) return native_fwrite_fn(file, buffer);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn usz! native_fread(CFile file, char[] buffer) @inline
{
	if (native_fread_fn) return native_fread_fn(file, buffer);
	return IoError.UNSUPPORTED_OPERATION?;
}
