// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/temp_directory.c3
// Permalink: https://github.com/c3lang/c3c/blob/b25c573/lib/std/io/os/temp_directory.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os @if(env::LIBC);
import std::io::path, std::os;

fn Path! native_temp_directory(Allocator allocator = allocator::heap()) @if(!env::WIN32)
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::get_var(env) ?? "";
		if (tmpdir) return path::new(tmpdir, allocator);
	}
	return path::new("/tmp", allocator);
}

fn Path! native_temp_directory(Allocator allocator = allocator::heap()) @if(env::WIN32)
{
	@pool(allocator)
	{
		Win32_DWORD len = win32::getTempPathW(0, null);
		if (!len) return IoError.GENERAL_ERROR?;
		Char16[] buff = mem::temp_alloc_array(Char16, len + (usz)1);
		if (!win32::getTempPathW(len, buff)) return IoError.GENERAL_ERROR?;
		return path::new(string::temp_from_utf16(buff[:len]), allocator);
	};
}

module std::io::os @if(env::NO_LIBC);
import std::io::path;

macro Path! native_temp_directory(Allocator allocator = allocator::heap())
{
	return IoError.UNSUPPORTED_OPERATION?;
}
