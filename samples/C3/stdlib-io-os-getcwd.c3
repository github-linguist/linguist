// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/getcwd.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/io/os/getcwd.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os;
import libc, std::os;

macro String! getcwd(Allocator allocator = allocator::heap())
{
	$switch
		$case env::WIN32:
			const DEFAULT_BUFFER = 256;
			Char16[DEFAULT_BUFFER] buffer;
			WString res = win32::_wgetcwd(&buffer, DEFAULT_BUFFER);
			bool free = false;
			defer if (free) libc::free(res);
			if (!res)
			{
				if (libc::errno() != errno::ERANGE) return IoError.GENERAL_ERROR?;
				res = win32::_wgetcwd(null, 0);
				free = true;
			}
			Char16[] str16 = res[:win32::wcslen(res)];
			return string::new_from_utf16(str16, allocator);

		$case env::POSIX:
			const usz DEFAULT_BUFFER = 256;
			char[DEFAULT_BUFFER] buffer;
			ZString res = posix::getcwd(&buffer, DEFAULT_BUFFER);
			bool free = false;
			if (!res)
			{
				// Improve error
				if (libc::errno() != errno::ERANGE) return IoError.GENERAL_ERROR?;
				res = posix::getcwd(null, 0);
				free = true;
			}
			defer if (free) libc::free((void*)res);
			return res.copy(allocator);

		$default:
			return IoError.UNSUPPORTED_OPERATION?;
	$endswitch
}

