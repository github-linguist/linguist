// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/file.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/io/file.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;
import libc;

struct File (InStream, OutStream)
{
	CFile file;
}

module std::io::file;
import libc, std::io::path, std::io::os;

fn File! open(String filename, String mode)
{
	return from_handle(os::native_fopen(filename, mode));
}

fn File! open_path(Path path, String mode)
{
	return from_handle(os::native_fopen(path.str_view(), mode));
}

fn File from_handle(CFile file)
{
	return { .file = file };
}

fn bool is_file(String path)
{
	return os::native_is_file(path);
}

fn usz! get_size(String path)
{
	return os::native_file_size(path);
}

fn void! delete(String filename) => os::native_remove(filename) @inline;


/**
 * @require self.file != null
 **/
fn void! File.reopen(&self, String filename, String mode)
{
	self.file = os::native_freopen(self.file, filename, mode)!;
}

/**
 * @require self.file != null
 **/
fn usz! File.seek(&self, isz offset, Seek seek_mode = Seek.SET) @dynamic
{
	os::native_fseek(self.file, offset, seek_mode)!;
	return os::native_ftell(self.file);
}


/*
Implement later
/**
 * @require self.file == null
 **/
fn void! File.memopen(File* file, char[] data, String mode)
{
	@pool()
	{
		file.file = libc::memopen(data.ptr, data.len, mode.zstr_tcopy(), file.file);
		// TODO errors
	};
}
*/


/**
 * @require self.file != null
 */
fn void! File.write_byte(&self, char c) @dynamic
{
	if (!libc::fputc(c, self.file)) return IoError.EOF?;
}

/**
 * @param [&inout] self
 */
fn void! File.close(&self) @inline @dynamic
{
	if (self.file && libc::fclose(self.file))
	{
		switch (libc::errno())
		{
			case errno::ECONNRESET:
			case errno::EBADF: return IoError.FILE_NOT_VALID?;
			case errno::EINTR: return IoError.INTERRUPTED?;
			case errno::EDQUOT:
			case errno::EFAULT:
			case errno::EAGAIN:
			case errno::EFBIG:
			case errno::ENETDOWN:
			case errno::ENETUNREACH:
			case errno::ENOSPC:
			case errno::EIO: return IoError.INCOMPLETE_WRITE?;
			default: return IoError.UNKNOWN_ERROR?;
		}
	}
	self.file = null;
}

/**
 * @require self.file
 */
fn bool File.eof(&self) @inline
{
	return libc::feof(self.file) != 0;
}

/**
 * @param [in] buffer
 */
fn usz! File.read(&self, char[] buffer) @dynamic
{
	return os::native_fread(self.file, buffer);
}

/**
 * @param [out] buffer
 * @require self.file `File must be initialized`
 */
fn usz! File.write(&self, char[] buffer) @dynamic
{
	return os::native_fwrite(self.file, buffer);
}


fn char! File.read_byte(&self) @dynamic
{
	int c = libc::fgetc(self.file);
	if (c == -1) return IoError.EOF?;
	return (char)c;
}

/**
 * Load up to buffer.len characters. Returns IoError.OVERFLOW if the file is longer
 * than the buffer.
 *
 * @param filename "The path to the file to read"
 * @param [in] buffer "The buffer to read to"
 **/
fn char[]! load_buffer(String filename, char[] buffer)
{
	File file = open(filename, "rb")!;
	defer (void)file.close();
	usz len = file.seek(0, END)!;
	if (len > buffer.len) return IoError.OVERFLOW?;
	file.seek(0, SET)!;
	usz read = 0;
	while (read < len)
	{
		read += file.read(buffer[read:len - read])!;
	}
	return buffer[:len];

}

fn char[]! load_new(String filename, Allocator allocator = allocator::heap())
{
	File file = open(filename, "rb")!;
	defer (void)file.close();
	usz len = file.seek(0, END)!;
	file.seek(0, SET)!;
	char* data = allocator::malloc_try(allocator, len)!;
	defer catch allocator::free(allocator, data);
	usz read = 0;
	while (read < len)
	{
		read += file.read(data[read:len - read])!;
	}
	return data[:len];
}

fn char[]! load_temp(String filename)
{
	return load_new(filename, allocator::temp());
}

/**
 * @require self.file `File must be initialized`
 */
fn void! File.flush(&self) @dynamic
{
	libc::fflush(self.file);
}
