// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/path.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/io/path.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io::path;
import std::collections::list, std::io::os;
import std::os::win32;

const PathEnv DEFAULT_PATH_ENV = env::WIN32 ? PathEnv.WIN32 : PathEnv.POSIX;
const char PREFERRED_SEPARATOR_WIN32 = '\\';
const char PREFERRED_SEPARATOR_POSIX = '/';
const char PREFERRED_SEPARATOR = env::WIN32 ? PREFERRED_SEPARATOR_WIN32 : PREFERRED_SEPARATOR_POSIX;

def PathList = List(<Path>);

fault PathResult
{
	INVALID_PATH,
	NO_PARENT,
}

def Path = PathImp;

struct PathImp (Printable)
{
	String path_string;
	PathEnv env;
}

enum PathEnv
{
	WIN32,
	POSIX
}

fn Path! new_cwd(Allocator allocator = allocator::heap())
{
	@pool(allocator)
	{
		return new(os::getcwd(allocator::temp()), allocator);
	};
}

fn Path! getcwd(Allocator allocator = allocator::heap()) @deprecated("Use new_cwd()")
{
	@pool(allocator)
	{
		return new(os::getcwd(allocator::temp()), allocator);
	};
}

fn bool is_dir(Path path) => os::native_is_dir(path.str_view());
fn bool is_file(Path path) => os::native_is_file(path.str_view());
fn usz! file_size(Path path) => os::native_file_size(path.str_view());
fn bool exists(Path path) => os::native_file_or_dir_exists(path.str_view());
fn Path! temp_cwd() => new_cwd(allocator::temp()) @inline;
fn Path! tgetcwd() @deprecated("Use temp_cwd()") => new_cwd(allocator::temp()) @inline;
fn void! chdir(Path path) => os::native_chdir(path) @inline;
fn Path! temp_directory(Allocator allocator = allocator::heap()) => os::native_temp_directory(allocator);
fn void! delete(Path path) => os::native_remove(path.str_view()) @inline;

macro bool is_separator(char c, PathEnv path_env = DEFAULT_PATH_ENV)
{
	return c == '/' || (c == '\\' && path_env == PathEnv.WIN32);
}

macro bool is_posix_separator(char c)
{
	return c == '/' || c == '\\';
}

macro bool is_win32_separator(char c)
{
	return c == '/' || c == '\\';
}

fn PathList! ls(Path dir, bool no_dirs = false, bool no_symlinks = false, String mask = "", Allocator allocator = allocator::heap()) @deprecated("use new_ls")
{
	return new_ls(dir, no_dirs, no_symlinks, mask, allocator);

}
fn PathList! temp_ls(Path dir, bool no_dirs = false, bool no_symlinks = false, String mask = "")
{
	return new_ls(dir, no_dirs, no_symlinks, mask, allocator::temp()) @inline;
}

fn PathList! new_ls(Path dir, bool no_dirs = false, bool no_symlinks = false, String mask = "", Allocator allocator = allocator::heap())
{
	$if $defined(os::native_ls):
		return os::native_ls(dir, no_dirs, no_symlinks, mask, allocator);
	$else
		return IoError.UNSUPPORTED_OPERATION?;
	$endif
}

enum MkdirPermissions
{
	NORMAL,
	USER_ONLY,
	USER_AND_ADMIN
}

fn bool! mkdir(Path path, bool recursive = false, MkdirPermissions permissions = NORMAL)
{
	if (!path.path_string.len) return PathResult.INVALID_PATH?;
	if (is_dir(path)) return false;
	if (exists(path)) return IoError.FILE_NOT_DIR?;

	if (recursive)
	{
		if (try parent = path.parent()) mkdir(parent, true, permissions)!;
	}
	if (!is_dir(path.parent()) ?? false) return IoError.CANNOT_READ_DIR?;

	return os::native_mkdir(path, permissions);
}

fn bool! rmdir(Path path)
{
	if (!path.path_string.len) return PathResult.INVALID_PATH?;
	return os::native_rmdir(path);
}

fn void! rmtree(Path path)
{
	if (!path.path_string.len) return PathResult.INVALID_PATH?;
	$if $defined(os::native_rmtree):
		return os::native_rmtree(path);
	$else
		return IoError.UNSUPPORTED_OPERATION?;
	$endif
}

fn Path! new(String path, Allocator allocator = allocator::heap(), PathEnv path_env = DEFAULT_PATH_ENV)
{
	return { normalize(path.copy(allocator), path_env), path_env };
}

fn Path! temp_new(String path, PathEnv path_env = DEFAULT_PATH_ENV)
{
	return new(path, allocator::temp(), path_env);
}

fn Path! new_win32_wstring(WString path, Allocator allocator = allocator::heap())
{
	@pool(allocator)
	{
		return path::new(string::temp_from_wstring(path)!, allocator: allocator);
	};
}

fn Path! new_windows(String path, Allocator allocator = allocator::heap())
{
	return new(path, allocator, WIN32);
}

fn Path! new_posix(String path, Allocator allocator = allocator::heap())
{
	return new(path, allocator, POSIX);
}

fn bool Path.equals(self, Path p2)
{
	return self.env == p2.env && self.path_string == p2.path_string;
}

fn Path! Path.append(self, String filename, Allocator allocator = allocator::heap()) @deprecated("Use path.new_append(...)")
{
	return self.new_append(filename, allocator) @inline;
}

/**
 * Append the string to the current path.
 *
 * @param [in] filename
 **/
fn Path! Path.new_append(self, String filename, Allocator allocator = allocator::heap())
{
	if (!self.path_string.len) return new(filename, allocator, self.env)!;
	assert(!is_separator(self.path_string[^1], self.env));

	@pool(allocator)
	{
		DString dstr = dstring::temp_with_capacity(self.path_string.len + 1 + filename.len);
		dstr.append(self.path_string);
		dstr.append(PREFERRED_SEPARATOR);
		dstr.append(filename);
		return { normalize(dstr.copy_str(allocator), self.env), self.env };
	};
}

fn Path! Path.temp_append(self, String filename) => self.new_append(filename, allocator::temp());

fn Path! Path.tappend(self, String filename) @deprecated("Use path.temp_append(...)") => self.new_append(filename, allocator::temp());

fn usz Path.start_of_base_name(self) @local
{
	String path_str = self.path_string;
	if (!path_str.len) return 0;
	if (self.env == PathEnv.WIN32)
	{
		if (try index = path_str.rindex_of_char('\\'))
		{
			// c:\ style path, we're done!
			if (path_str[0] != '\\') return index + 1;
			// Handle \\server\foo
			// Find the \ before "foo"
			usz last_index = 2 + path_str[2..].index_of_char('\\')!!;
			// If they don't match, we're done
			assert(last_index <= index, "Invalid normalized, path %d vs %s in %s", last_index, index, path_str);
			if (last_index != index) return index + 1;
			// Otherwise just default to the volume length.
		}
		return volume_name_len(path_str, self.env)!!;
	}
	return path_str.rindex_of_char('/') + 1 ?? 0;
}

fn bool! Path.is_absolute(self)
{
	String path_str = self.str_view();
	if (!path_str.len) return false;
	usz path_start = volume_name_len(path_str, self.env)!;
	if (path_start > 0 && path_str[0] == '\\') return true;
	return path_start < path_str.len && is_separator(path_str[path_start], self.env);
}

fn Path! Path.absolute(self, Allocator allocator = allocator::heap()) @deprecated("Use path.new_absolute()")
{
	return self.new_absolute(allocator) @inline;
}

/**
 * @require self.env == DEFAULT_PATH_ENV : "This method is only available on native paths"
 **/
fn Path! Path.new_absolute(self, Allocator allocator = allocator::heap())
{
	String path_str = self.str_view();
	if (!path_str.len) return PathResult.INVALID_PATH?;
	if (self.is_absolute()!) return new(path_str, allocator, self.env);
	if (path_str == ".")
	{
		@pool(allocator)
		{
			String cwd = os::getcwd(allocator::temp())!;
			return new(cwd, allocator, self.env);
		};
	}
	$if DEFAULT_PATH_ENV == WIN32:
		@pool(allocator)
		{
			const usz BUFFER_LEN = 4096;
			WString buffer = (WString)mem::temp_alloc_array(Char16, BUFFER_LEN);
			buffer = win32::_wfullpath(buffer, path_str.to_temp_wstring()!, BUFFER_LEN);
			if (!buffer) return PathResult.INVALID_PATH?;
			return { string::new_from_wstring(buffer, allocator), WIN32 };
		};
	$else
		String cwd = os::getcwd(allocator::temp())!;
		return Path { cwd, self.env }.new_append(path_str, allocator)!;
	$endif
}

fn String Path.basename(self)
{
	usz basename_start = self.start_of_base_name();
	String path_str = self.path_string;
	if (basename_start == path_str.len) return "";
	return path_str[basename_start..];
}


fn String Path.dirname(self)
{
	usz basename_start = self.start_of_base_name();
	String path_str = self.path_string;
	if (basename_start == 0) return ".";
	usz start = volume_name_len(path_str, self.env)!!;
	if (basename_start <= start + 1)
	{
		if (self.env == WIN32 && basename_start > start && path_str[0..1] == `\\`)
		{
			return path_str[:basename_start - 1];
		}
		return path_str[:basename_start];
	}
	return path_str[:basename_start - 1];
}

/**
 * Test if the path has the given extension, so given the path /foo/bar.c3
 * this would be true matching the extension "c3"
 *
 * @param [in] extension `The extension name (not including the leading '.')`
 * @require extension.len > 0 : `The extension cannot be empty`
 * @return `true if the extension matches`
 **/
fn bool Path.has_extension(self, String extension)
{
	String basename = self.basename();
	if (basename.len <= extension.len) return false;
	if (basename[^extension.len + 1] != '.') return false;
	return basename[^extension.len..] == extension;
}

fn String! Path.extension(self)
{
	String basename = self.basename();
	usz index = basename.rindex_of(".")!;
	// Plain ".foo" does not have an
	if (index == 0) return SearchResult.MISSING?;
	if (index == basename.len) return "";
	return basename[index + 1..];
}

fn String Path.volume_name(self)
{
	usz len = volume_name_len(self.str_view(), self.env)!!;
	if (!len) return "";
	return self.path_string[:len];
}

fn usz! volume_name_len(String path, PathEnv path_env) @local
{
	usz len = path.len;
	if (len < 2 || path_env != PathEnv.WIN32) return 0;
	switch (path[0])
	{
		case '\\':
			// "\\" paths.. must be longer than 2
			if (len == 2) return 0;
			int count = 1;
			while (count < len && path[count] == '\\') count++;
			// Not 2 => folded paths
			if (count != 2) return 0;
			// Check that we have a name followed by '\'
			isz base_found = 0;
			for (usz i = 2; i < len; i++)
			{
				char c = path[i];
				if (is_win32_separator(c))
				{
					if (base_found) return i;
					base_found = i;
					continue;
				}
				if (is_reserved_win32_path_char(c)) return PathResult.INVALID_PATH?;
			}
			if (base_found > 0 && base_found + 1 < len) return len;
			return PathResult.INVALID_PATH?;
		case 'A'..'Z':
		case 'a'..'z':
			return path[1] == ':' ? 2 : 0;
		default:
			return 0;
	}
}

fn Path! Path.parent(self)
{
	if (self.path_string.len == 1 && is_separator(self.path_string[0], self.env)) return PathResult.NO_PARENT?;
	foreach_r(i, c : self.path_string)
	{
		if (is_separator(c, self.env))
		{
			return { self.path_string[:i], self.env };
		}
	}
	return PathResult.NO_PARENT?;
}

fn String! normalize(String path_str, PathEnv path_env = DEFAULT_PATH_ENV)
{
	if (!path_str.len) return "";
	usz path_start = volume_name_len(path_str, path_env)!;
	if (path_start > 0 && path_env == PathEnv.WIN32)
	{
		for (usz i = 0; i < path_start; i++) if (path_str[i] == '/') path_str[i] = '\\';
	}
	usz path_len = path_str.len;
	if (path_start == path_len) return path_str;
	char path_separator = path_env == PathEnv.WIN32 ? PREFERRED_SEPARATOR_WIN32 : PREFERRED_SEPARATOR_POSIX;
	usz len = path_start;
	bool has_root = is_separator(path_str[path_start], path_env);
	if (has_root)
	{
		path_str[len++] = path_separator;
		path_start++;
	}
	// It is safe to write it as true, since we already dealt with /foo.
	// This allows us to avoid checking whether it is the start of the path.
	bool previous_was_separator = true;

	for (usz i = path_start; i < path_len; i++)
	{
		char c = path_str[i];
		// Fold foo///bar into foo/bar
		if (is_separator(c, path_env))
		{
			// Fold //
			if (previous_was_separator) continue;

			// New /, so mark and rewrite
			path_str.ptr[len++] = path_separator;
			previous_was_separator = true;
			continue;
		}

		// The rest are names of the path elements, so check that the
		// characters are valid.
		if (is_reserved_path_char(c, path_env)) return PathResult.INVALID_PATH?;

		// If we have '.' after a separator
		if (c == '.' && previous_was_separator)
		{
			// Get the number of dots until next separator, expecting 1 or 2
			bool is_last = i == path_len - 1;
			int dots = 1;
			if (!is_last)
			{
				char next = path_str[i + 1];
				switch
				{
					case next == '.':
						dots = 2;
						is_last = i == path_len - 2;
						if (!is_last && !is_separator(path_str[i + 2], path_env))
						{
							dots = 0;
						}
					case !is_separator(next, path_env):
						dots = 0;
				}
			}
			switch (dots)
			{
				case 1:
					// /./abc -> skip to /./abc
					//  ^                  ^
					i++;
					continue;
				case 2:
					// This is an error: /a/../..
					if (len == path_start && has_root) return PathResult.INVALID_PATH?;

					// If this .. at the start, or after ../? If so, we just copy ..
					if (len == path_start ||
						(len - path_start >= 3 && path_str[len - 1] == path_separator
						&& path_str[len - 3] == '.' && path_str[len - 3] == '.' &&
						(len - 3 == 0 || path_str[len - 4] == path_separator)))
					{
						if (i != len)
						{
							path_str[len] = '.';
							path_str[len + 1] = '.';
						}
						len += 2;
						if (len < path_len) path_str[len++] = path_separator;
						i += 2;
						continue;
					}
					// Step back, now looking at '/' abc/def/. -> abc/def/
					len--;
					// Step back until finding a separator or the start.
					while (len > path_start && !is_separator(path_str[len - 1], path_env))
					{
						len--;
					}
					// Reading, we go from /../abc to /../abc
					//                      ^            ^
					i += 2;
					continue;
				default:
					break;

			}
		}
		if (i != len) path_str[len] = c;
		previous_was_separator = false;
		len++;
	}
	if (len > path_start + 1 && is_separator(path_str[len - 1], path_env)) len--;
	if (path_str.len > len) path_str.ptr[len] = 0;
	// Empty path after normalization -> "."
	if (!len) return ".";
	return path_str[:len];
}

fn ZString Path.as_zstr(self) => (ZString)self.path_string.ptr;

fn String Path.root_directory(self)
{
	String path_str = self.str_view();
	usz len = path_str.len;
	if (!len) return "";
	if (path_str == ".") return ".";
	if (self.env == PathEnv.WIN32)
	{
		usz root_len = volume_name_len(path_str, self.env)!!;
		if (root_len == len || !is_win32_separator(path_str[root_len])) return "";
		return path_str[root_len..root_len];
	}
	if (!is_posix_separator(path_str[0])) return "";
	for (usz i = 1; i < len; i++)
	{
		if (is_posix_separator(path_str[i]))
		{
			return path_str[:i];
		}
	}
	return path_str;
}

def PathWalker = fn bool! (Path, bool is_dir, void*);

/*
 * Walk the path recursively. PathWalker is run on every file and
 * directory found. Return true to abort the walk.
 * @require self.env == DEFAULT_PATH_ENV : "This method is only available on native paths"
 */
fn bool! Path.walk(self, PathWalker w, void* data)
{
	const PATH_MAX = 512;
	@stack_mem(PATH_MAX; Allocator allocator)
	{
		Path abs = self.new_absolute(allocator)!;
		PathList files = new_ls(abs, allocator: allocator)!;
		foreach (f : files)
		{
			if (f.str_view() == "." || f.str_view() == "..") continue;
			f = abs.new_append(f.str_view(), allocator)!;
			bool is_directory = is_dir(f);
			if (w(f, is_directory, data)!) return true;
			if (is_directory && f.walk(w, data)!) return true;
		}
	};
	return false;
}

fn String Path.str_view(self) @inline
{
	return self.path_string;
}


fn bool Path.has_suffix(self, String str)
{
	return self.str_view().ends_with(str);
}

fn void Path.free_with_allocator(self, Allocator allocator)
{
	allocator::free(allocator, self.path_string.ptr);
}

fn void Path.free(self)
{
	free(self.path_string.ptr);
}


fn usz! Path.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.print(self.str_view());
}

fn String Path.to_new_string(&self, Allocator allocator = allocator::heap()) @dynamic
{
	return self.str_view().copy(allocator);
}

const bool[256] RESERVED_PATH_CHAR_POSIX = {
	[0] = true,
	['/'] = true,
};
const bool[256] RESERVED_PATH_CHAR_WIN32 = {
	[0..31] = true,
	['>'] = true,
	['<'] = true,
	[':'] = true,
	['\"'] = true,
	['/'] = true,
	['\\'] = true,
	['|'] = true,
	['?'] = true,
	['*'] = true,
};

macro bool is_reserved_win32_path_char(char c)
{
	return RESERVED_PATH_CHAR_WIN32[c];
}

macro bool is_reserved_path_char(char c, PathEnv path_env = DEFAULT_PATH_ENV)
{
	return path_env == PathEnv.WIN32
		? RESERVED_PATH_CHAR_WIN32[c]
		: RESERVED_PATH_CHAR_POSIX[c];
}