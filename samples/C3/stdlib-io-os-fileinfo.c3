// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/fileinfo.c3
// Permalink: https://github.com/c3lang/c3c/blob/1b54a99/lib/std/io/os/fileinfo.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os;
import libc, std::os, std::io;

fn void! native_stat(Stat* stat, String path) @if(env::DARWIN || env::LINUX || env::BSD_FAMILY)
{
	@pool()
	{
		$if env::DARWIN || env::LINUX || env::BSD_FAMILY:
			int res = libc::stat(path.zstr_tcopy(), stat);
		$else
			unreachable("Stat unimplemented");
			int res = 0;
		$endif
		if (res != 0)
		{
			switch (libc::errno())
			{
				case errno::EBADF:
					return IoError.FILE_NOT_VALID?;
				case errno::EFAULT:
					unreachable("Invalid stat");
				case errno::EIO:
					return IoError.GENERAL_ERROR?;
				case errno::EACCES:
					return IoError.NO_PERMISSION?;
				case errno::ELOOP:
					return IoError.NO_PERMISSION?;
				case errno::ENAMETOOLONG:
					return IoError.NAME_TOO_LONG?;
				case errno::ENOENT:
					return IoError.FILE_NOT_FOUND?;
				case errno::ENOTDIR:
					return IoError.FILE_NOT_DIR?;
				case errno::EOVERFLOW:
					return IoError.GENERAL_ERROR?;
				default:
					return IoError.UNKNOWN_ERROR?;
			}
		}
	};
}

fn usz! native_file_size(String path) @if(env::WIN32)
{
	@pool()
	{
		Win32_FILE_ATTRIBUTE_DATA data;
		win32::getFileAttributesExW(path.to_temp_wstring()!, Win32_GET_FILEEX_INFO_LEVELS.STANDARD, &data);
		Win32_LARGE_INTEGER size;
		size.lowPart = data.nFileSizeLow;
		size.highPart = data.nFileSizeHigh;
		return (usz)size.quadPart;
	};
}

fn usz! native_file_size(String path) @if(!env::WIN32 && !env::DARWIN)
{
	File f = file::open(path, "r")!;
	defer (void)f.close();
	return f.seek(0, Seek.END)!;
}

fn usz! native_file_size(String path) @if(env::DARWIN)
{
	Stat stat;
	native_stat(&stat, path)!;
	return stat.st_size;
}

fn bool native_file_or_dir_exists(String path)
{
	$switch
		$case env::DARWIN:
		$case env::FREEBSD:
		$case env::NETBSD:
		$case env::OPENBSD:
		$case env::LINUX:
			Stat stat;
			return @ok(native_stat(&stat, path));
		$case env::WIN32:
			@pool()
			{
				return (bool)win32::pathFileExistsW(path.to_temp_utf16()) ?? false;
			};
		$case env::POSIX:
			@pool()
			{
				return posix::access(path.zstr_tcopy(), 0 /* F_OK */) != -1;
			};
		$default:
			unreachable("Not supported");
	$endswitch
}

fn bool native_is_file(String path)
{
	$switch
		$case env::DARWIN:
		$case env::FREEBSD:
		$case env::NETBSD:
		$case env::OPENBSD:
		$case env::LINUX:
			Stat stat;
			return @ok(native_stat(&stat, path)) && libc_S_ISTYPE(stat.st_mode, libc::S_IFREG);
		$default:
			File! f = file::open(path, "r");
			defer (void)f.close();
			return @ok(f);
	$endswitch
}

fn bool native_is_dir(String path)
{
	$if env::DARWIN || env::LINUX || env::BSD_FAMILY:
		Stat stat;
		return @ok(native_stat(&stat, path)) && libc_S_ISTYPE(stat.st_mode, libc::S_IFDIR);
	$else
		return native_file_or_dir_exists(path) && !native_is_file(path);
	$endif
}


