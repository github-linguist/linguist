// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/chdir.c3
// Permalink: https://github.com/c3lang/c3c/blob/748c737/lib/std/io/os/chdir.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os;
import std::io::path, libc, std::os;

macro void! native_chdir(Path path)
{
	$switch
		$case env::POSIX:
			if (posix::chdir(path.as_zstr()))
			{
				switch (libc::errno())
				{
					case errno::EACCES: return IoError.NO_PERMISSION?;
					case errno::ENAMETOOLONG: return IoError.NAME_TOO_LONG?;
					case errno::ENOTDIR: return IoError.FILE_NOT_DIR?;
					case errno::ENOENT: return IoError.FILE_NOT_FOUND?;
					case errno::ELOOP: return IoError.SYMLINK_FAILED?;
					default: return IoError.GENERAL_ERROR?;
				}
			}
		$case env::WIN32:
			@pool()
			{
				// TODO improve with better error handling.
				if (win32::setCurrentDirectoryW(path.str_view().to_temp_utf16()!!)) return;
			};
			return IoError.GENERAL_ERROR?;
		$default:
			return IoError.UNSUPPORTED_OPERATION?;
	$endswitch
}
