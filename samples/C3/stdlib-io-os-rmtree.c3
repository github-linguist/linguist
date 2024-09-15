// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/rmtree.c3
// Permalink: https://github.com/c3lang/c3c/blob/ad49501/lib/std/io/os/rmtree.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os @if(env::POSIX);
import std::io, std::os, libc;

/**
 * @require dir.str_view()
 **/
fn void! native_rmtree(Path dir)
{
	DIRPtr directory = posix::opendir(dir.as_zstr());
	defer if (directory) posix::closedir(directory);
	if (!directory) return path::is_dir(dir) ? IoError.CANNOT_READ_DIR? : IoError.FILE_NOT_DIR?;
	Posix_dirent* entry;
	while ((entry = posix::readdir(directory)))
	{
		@pool()
		{
			String name = ((ZString)&entry.name).str_view();
			if (!name || name == "." || name == "..") continue;
			Path new_path = dir.temp_append(name)!;
			if (entry.d_type == posix::DT_DIR)
			{
				native_rmtree(new_path)!;
				continue;
			}
			if (libc::remove(new_path.as_zstr()))
			{
				// TODO improve
				return IoError.GENERAL_ERROR?;
			}
		};
	}
	os::native_rmdir(dir)!;
}

module std::io::os @if(env::WIN32);
import std::io, std::time, std::os;

fn void! native_rmtree(Path path)
{
	Win32_WIN32_FIND_DATAW find_data;
	String s = path.str_view().tconcat("\\*");
	Win32_HANDLE find = win32::findFirstFileW(s.to_temp_utf16(), &find_data)!;

	if (find == win32::INVALID_HANDLE_VALUE) return IoError.CANNOT_READ_DIR?;
	defer win32::findClose(find);
	do
	{
		@pool()
		{
			String filename = string::new_from_wstring((WString)&find_data.cFileName, allocator::temp())!;
			if (filename == "." || filename == "..") continue;
			Path file_path = path.temp_append(filename)!;
			if (find_data.dwFileAttributes & win32::FILE_ATTRIBUTE_DIRECTORY)
			{
				native_rmtree(file_path)!;
			}
			else
			{
				win32::deleteFileW(file_path.str_view().to_temp_wstring()!!);
			}
		};
	} while (win32::findNextFileW(find, &find_data) != 0);
	os::native_rmdir(path)!;
}
