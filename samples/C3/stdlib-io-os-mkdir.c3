// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/mkdir.c3
// Permalink: https://github.com/c3lang/c3c/blob/a1ecf22/lib/std/io/os/mkdir.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os;
import libc;
import std::io::path;
import std::os::win32;
import std::os::posix;


macro bool! native_mkdir(Path path, MkdirPermissions permissions)
{
	$switch
		$case env::POSIX:
			if (!posix::mkdir(path.as_zstr(), permissions == NORMAL ? 0o777 : 0o700)) return true;
			switch (libc::errno())
			{
				case errno::EACCES:
				case errno::EPERM:
				case errno::EROFS:
				case errno::EFAULT: return IoError.NO_PERMISSION?;
				case errno::ENAMETOOLONG: return IoError.NAME_TOO_LONG?;
				case errno::EDQUOT:
				case errno::ENOSPC: return IoError.OUT_OF_SPACE?;
				case errno::EISDIR:
				case errno::EEXIST: return false;
				case errno::ELOOP: return IoError.SYMLINK_FAILED?;
				case errno::ENOTDIR: return IoError.FILE_NOT_FOUND?;
				default: return IoError.GENERAL_ERROR?;
			}
		$case env::WIN32:
			@pool()
			{
				// TODO security attributes
				if (win32::createDirectoryW(path.str_view().to_temp_utf16()!!, null)) return true;
				switch (win32::getLastError())
				{
					case win32::ERROR_ACCESS_DENIED:
						return IoError.NO_PERMISSION?;
					case win32::ERROR_DISK_FULL:
						return IoError.OUT_OF_SPACE?;
					case win32::ERROR_ALREADY_EXISTS:
						return false;
					case win32::ERROR_PATH_NOT_FOUND:
						return IoError.FILE_NOT_FOUND?;
					default:
						return IoError.GENERAL_ERROR?;
				}
			};
		$default:
			return IoError.UNSUPPORTED_OPERATION?;
	$endswitch
}