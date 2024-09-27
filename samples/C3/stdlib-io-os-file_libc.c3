// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/file_libc.c3
// Permalink: https://github.com/c3lang/c3c/blob/499c82b/lib/std/io/os/file_libc.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os @if(env::LIBC);
import libc;

/**
 * @require mode.len > 0
 * @require filename.len > 0
 **/
fn void*! native_fopen(String filename, String mode) @inline
{
	@pool()
	{
		$if env::WIN32:
			void* file = libc::_wfopen(filename.to_temp_wstring(), mode.to_temp_wstring())!;
		$else
			void* file = libc::fopen(filename.zstr_tcopy(), mode.zstr_tcopy());
		$endif
			return file ?: file_open_errno()?;
	};
}

fn void! native_remove(String filename)
{
	@pool()
	{
		$if env::WIN32:
			CInt result = libc::_wremove(filename.to_temp_wstring())!;
		$else
			CInt result = libc::remove(filename.zstr_tcopy());
		$endif
		if (result)
		{
			switch (libc::errno())
			{
				case errno::ENOENT:
					return IoError.FILE_NOT_FOUND?;
				case errno::EACCES:
				default:
					return IoError.FILE_CANNOT_DELETE?;
			}
		}
	};
}

/**
 * @require mode.len > 0
 * @require filename.len > 0
 **/
fn void*! native_freopen(void* file, String filename, String mode) @inline
{
	@pool()
	{
		$if env::WIN32:
			file = libc::_wfreopen(filename.to_temp_wstring(), mode.to_temp_wstring(), file)!;
		$else
			file = libc::freopen(filename.zstr_tcopy(), mode.zstr_tcopy(), file);
		$endif
			return file ?: file_open_errno()?;
	};
}

fn void! native_fseek(void* file, isz offset, Seek seek_mode) @inline
{
	if (libc::fseek(file, (SeekIndex)offset, (CInt)seek_mode)) return file_seek_errno()?;
}


fn usz! native_ftell(CFile file) @inline
{
	long index = libc::ftell(file);
	return index >= 0 ? (usz)index : file_seek_errno()?;
}

fn usz! native_fwrite(CFile file, char[] buffer) @inline
{
	return libc::fwrite(buffer.ptr, 1, buffer.len, file);
}

fn usz! native_fread(CFile file, char[] buffer) @inline
{
	return libc::fread(buffer.ptr, 1, buffer.len, file);
}

macro anyfault file_open_errno() @local
{
	switch (libc::errno())
	{
		case errno::EACCES: return IoError.NO_PERMISSION;
		case errno::EDQUOT: return IoError.OUT_OF_SPACE;
		case errno::EBADF: return IoError.FILE_NOT_VALID;
		case errno::EEXIST: return IoError.ALREADY_EXISTS;
		case errno::EINTR: return IoError.INTERRUPTED;
		case errno::EFAULT: return IoError.GENERAL_ERROR;
		case errno::EISDIR: return IoError.FILE_IS_DIR;
		case errno::ELOOP: return IoError.SYMLINK_FAILED;
		case errno::EMFILE: return IoError.TOO_MANY_DESCRIPTORS;
		case errno::ENAMETOOLONG: return IoError.NAME_TOO_LONG;
		case errno::ENFILE: return IoError.OUT_OF_SPACE;
		case errno::ENOTDIR: return IoError.FILE_NOT_DIR;
		case errno::ENOENT: return IoError.FILE_NOT_FOUND;
		case errno::ENOSPC: return IoError.OUT_OF_SPACE;
		case errno::ENXIO: return IoError.FILE_NOT_FOUND;
		case errno::EOVERFLOW: return IoError.OVERFLOW;
		case errno::EROFS: return IoError.READ_ONLY;
		case errno::EOPNOTSUPP: return IoError.UNSUPPORTED_OPERATION;
		case errno::EIO: return IoError.INCOMPLETE_WRITE;
		case errno::EWOULDBLOCK: return IoError.WOULD_BLOCK;
		default: return IoError.UNKNOWN_ERROR;
	}
}

macro anyfault file_seek_errno() @local
{
	switch (libc::errno())
	{
		case errno::ESPIPE: return IoError.FILE_IS_PIPE;
		case errno::EPIPE: return IoError.FILE_IS_PIPE;
		case errno::EOVERFLOW: return IoError.OVERFLOW;
		case errno::ENXIO: return IoError.FILE_NOT_FOUND;
		case errno::ENOSPC: return IoError.OUT_OF_SPACE;
		case errno::EIO: return IoError.INCOMPLETE_WRITE;
		case errno::EINVAL: return IoError.INVALID_POSITION;
		case errno::EINTR: return IoError.INTERRUPTED;
		case errno::EFBIG: return IoError.OUT_OF_SPACE;
		case errno::EBADF: return IoError.FILE_NOT_VALID;
		case errno::EAGAIN: return IoError.WOULD_BLOCK;
		default: return IoError.UNKNOWN_ERROR;
	}
}

