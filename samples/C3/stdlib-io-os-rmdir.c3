// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/rmdir.c3
// Permalink: https://github.com/c3lang/c3c/blob/a1ecf22/lib/std/io/os/rmdir.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os;
import libc;
import std::io::path;
import std::os::win32;
import std::os::posix;

macro bool! native_rmdir(Path path)
{
	$switch
		$case env::POSIX:
			if (!posix::rmdir(path.as_zstr())) return true;
			switch (libc::errno())
			{
				case errno::EBUSY: return IoError.BUSY?;
				case errno::EACCES:
				case errno::EPERM:
				case errno::EROFS:
				case errno::EFAULT: return IoError.NO_PERMISSION?;
				case errno::ENAMETOOLONG: return IoError.NAME_TOO_LONG?;
				case errno::ENOTDIR:
				case errno::ENOENT: return false;
				case errno::ENOTEMPTY: return IoError.DIR_NOT_EMPTY?;
				case errno::ELOOP: return IoError.SYMLINK_FAILED?;
				default: return IoError.GENERAL_ERROR?;
			}
		$case env::WIN32:
			@pool()
			{
				if (win32::removeDirectoryW(path.str_view().to_temp_utf16()!!)) return true;
				switch (win32::getLastError())
				{
					case win32::ERROR_ACCESS_DENIED:
						return IoError.NO_PERMISSION?;
					case win32::ERROR_CURRENT_DIRECTORY:
						return IoError.BUSY?;
					case win32::ERROR_DIR_NOT_EMPTY:
						return IoError.DIR_NOT_EMPTY?;
					case win32::ERROR_DIRECTORY:
					case win32::ERROR_PATH_NOT_FOUND:
						return false;
					default:
						return IoError.GENERAL_ERROR?;
				}
			};
		$default:
			return IoError.UNSUPPORTED_OPERATION?;
	$endswitch
}
