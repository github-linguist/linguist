// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/os/ls.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/io/os/ls.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::os @if(env::POSIX);
import std::io, std::os;

fn PathList! native_ls(Path dir, bool no_dirs, bool no_symlinks, String mask, Allocator allocator)
{
	PathList list;
	list.new_init(allocator: allocator);
	DIRPtr directory = posix::opendir(dir.str_view() ? dir.as_zstr() : (ZString)".");
	defer if (directory) posix::closedir(directory);
	if (!directory) return (path::is_dir(dir) ? IoError.CANNOT_READ_DIR : IoError.FILE_NOT_DIR)?;
	Posix_dirent* entry;
	while ((entry = posix::readdir(directory)))
	{
		String name = ((ZString)&entry.name).str_view();
		if (!name || name == "." || name == "..") continue;
		if (entry.d_type == posix::DT_LNK && no_symlinks) continue;
		if (entry.d_type == posix::DT_DIR && no_dirs) continue;
		Path path = path::new(name, allocator)!!;
		list.push(path);
	}
	return list;
}

module std::io::os @if(env::WIN32);
import std::time, std::os, std::io;

fn PathList! native_ls(Path dir, bool no_dirs, bool no_symlinks, String mask, Allocator allocator)
{
	PathList list;
	list.new_init(allocator: allocator);

	@pool(allocator)
	{
		WString result = dir.str_view().tconcat(`\*`).to_temp_wstring()!!;
		Win32_WIN32_FIND_DATAW find_data;
		Win32_HANDLE find = win32::findFirstFileW(result, &find_data);
		if (find == win32::INVALID_HANDLE_VALUE) return IoError.CANNOT_READ_DIR?;
		defer win32::findClose(find);
		do
		{
			if (no_dirs && (find_data.dwFileAttributes & win32::FILE_ATTRIBUTE_DIRECTORY)) continue;
			@pool(allocator)
			{
				String filename = string::temp_from_wstring((WString)&find_data.cFileName)!;
				if (filename == ".." || filename == ".") continue;
				list.push(path::new(filename, allocator)!);
			};
		} while (win32::findNextFileW(find, &find_data));
		return list;
	};
}
