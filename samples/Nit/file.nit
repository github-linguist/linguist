# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2004-2008 Jean Privat <jean@pryen.org>
# Copyright 2008 Floréal Morandat <morandat@lirmm.fr>
# Copyright 2008 Jean-Sébastien Gélinas <calestar@gmail.com>
#
# This file is free software, which comes along with NIT.  This software is
# distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
# without  even  the implied warranty of  MERCHANTABILITY or  FITNESS FOR A 
# PARTICULAR PURPOSE.  You can modify it is you want,  provided this header
# is kept unaltered, and a notification of the changes is added.
# You  are  allowed  to  redistribute it and sell it, alone or is a part of
# another product.

# File manipulations (create, read, write, etc.)
module file

intrude import stream
intrude import ropes
import string_search
import time

in "C Header" `{
	#include <dirent.h>
	#include <string.h>
	#include <sys/types.h>
	#include <sys/stat.h>
	#include <unistd.h>
	#include <stdio.h>
	#include <poll.h>
	#include <errno.h>
`}

# File Abstract Stream
abstract class FStream
	super IOS
	# The path of the file.
	var path: nullable String = null

	# The FILE *.
	private var file: nullable NativeFile = null

	fun file_stat: FileStat do return _file.file_stat

	# File descriptor of this file
	fun fd: Int do return _file.fileno
end

# File input stream
class IFStream
	super FStream
	super BufferedIStream
	super PollableIStream
	# Misc

	# Open the same file again.
	# The original path is reused, therefore the reopened file can be a different file.
	fun reopen
	do
		if not eof and not _file.address_is_null then close
		_file = new NativeFile.io_open_read(path.to_cstring)
		if _file.address_is_null then
			last_error = new IOError("Error: Opening file at '{path.as(not null)}' failed with '{sys.errno.strerror}'")
			end_reached = true
			return
		end
		end_reached = false
		_buffer_pos = 0
		_buffer.clear
	end

	redef fun close
	do
		if _file.address_is_null then return
		var i = _file.io_close
		_buffer.clear
		end_reached = true
	end

	redef fun fill_buffer
	do
		var nb = _file.io_read(_buffer.items, _buffer.capacity)
		if nb <= 0 then
			end_reached = true
			nb = 0
		end
		_buffer.length = nb
		_buffer_pos = 0
	end
	# End of file?
	redef var end_reached: Bool = false

	# Open the file at `path` for reading.
	init open(path: String)
	do
		self.path = path
		prepare_buffer(10)
		_file = new NativeFile.io_open_read(path.to_cstring)
		if _file.address_is_null then
			last_error = new IOError("Error: Opening file at '{path}' failed with '{sys.errno.strerror}'")
			end_reached = true
		end
	end

	init from_fd(fd: Int) do
		self.path = ""
		prepare_buffer(10)
		_file = fd_to_stream(fd, read_only)
		if _file.address_is_null then
			last_error = new IOError("Error: Converting fd {fd} to stream failed with '{sys.errno.strerror}'")
			end_reached = true
		end
	end
end

# File output stream
class OFStream
	super FStream
	super OStream
	
	redef fun write(s)
	do
		if last_error != null then return
		if not _is_writable then
			last_error = new IOError("Cannot write to non-writable stream")
			return
		end
		if s isa FlatText then
			write_native(s.to_cstring, s.length)
		else
			for i in s.substrings do write_native(i.to_cstring, i.length)
		end
	end

	redef fun close
	do
		if _file.address_is_null then
			last_error = new IOError("Cannot close non-existing write stream")
			_is_writable = false
			return
		end
		var i = _file.io_close
		_is_writable = false
	end
	redef var is_writable = false
	
	# Write `len` bytes from `native`.
	private fun write_native(native: NativeString, len: Int)
	do
		if last_error != null then return
		if not _is_writable then
			last_error = new IOError("Cannot write to non-writable stream")
			return
		end
		if _file.address_is_null then
			last_error = new IOError("Writing on a null stream")
			_is_writable = false
			return
		end
		var err = _file.io_write(native, len)
		if err != len then
			# Big problem
			last_error = new IOError("Problem in writing : {err} {len} \n")
		end
	end
	
	# Open the file at `path` for writing.
	init open(path: String)
	do
		_file = new NativeFile.io_open_write(path.to_cstring)
		if _file.address_is_null then
			last_error = new IOError("Error: Opening file at '{path}' failed with '{sys.errno.strerror}'")
			self.path = path
			is_writable = false
		end
		self.path = path
		_is_writable = true
	end

	# Creates a new File stream from a file descriptor
	init from_fd(fd: Int) do
		self.path = ""
		_file = fd_to_stream(fd, wipe_write)
		_is_writable = true
		 if _file.address_is_null then
			 last_error = new IOError("Error: Opening stream from file descriptor {fd} failed with '{sys.errno.strerror}'")
			_is_writable = false
		end
	end
end

redef interface Object

	private fun read_only: NativeString do return "r".to_cstring

	private fun wipe_write: NativeString do return "w".to_cstring

	private fun fd_to_stream(fd: Int, mode: NativeString): NativeFile `{
		return fdopen(fd, mode);
	`}

	# returns first available stream to read or write to
	# return null on interruption (possibly a signal)
	protected fun poll( streams : Sequence[FStream] ) : nullable FStream
	do
		var in_fds = new Array[Int]
		var out_fds = new Array[Int]
		var fd_to_stream = new HashMap[Int,FStream]
		for s in streams do
			var fd = s.fd
			if s isa IFStream then in_fds.add( fd )
			if s isa OFStream then out_fds.add( fd )

			fd_to_stream[fd] = s
		end

		var polled_fd = intern_poll( in_fds, out_fds )

		if polled_fd == null then
			return null
		else
			return fd_to_stream[polled_fd]
		end
	end

	private fun intern_poll(in_fds: Array[Int], out_fds: Array[Int]) : nullable Int is extern import Array[Int].length, Array[Int].[], Int.as(nullable Int) `{
		int in_len, out_len, total_len;
		struct pollfd *c_fds;
		sigset_t sigmask;
		int i;
		int first_polled_fd = -1;
		int result;

		in_len = Array_of_Int_length( in_fds );
		out_len = Array_of_Int_length( out_fds );
		total_len = in_len + out_len;
		c_fds = malloc( sizeof(struct pollfd) * total_len );

		/* input streams */
		for ( i=0; i<in_len; i ++ ) {
			int fd;
			fd = Array_of_Int__index( in_fds, i );

			c_fds[i].fd = fd;
			c_fds[i].events = POLLIN;
		}

		/* output streams */
		for ( i=0; i<out_len; i ++ ) {
			int fd;
			fd = Array_of_Int__index( out_fds, i );

			c_fds[i].fd = fd;
			c_fds[i].events = POLLOUT;
		}

		/* poll all fds, unlimited timeout */
		result = poll( c_fds, total_len, -1 );

		if ( result > 0 ) {
			/* analyse results */
			for ( i=0; i<total_len; i++ )
				if ( c_fds[i].revents & c_fds[i].events || /* awaited event */
					 c_fds[i].revents & POLLHUP ) /* closed */
				{
					first_polled_fd = c_fds[i].fd;
					break;
				}

			return Int_as_nullable( first_polled_fd );
		}
		else if ( result < 0 )
			fprintf( stderr, "Error in Stream:poll: %s\n", strerror( errno ) );

		return null_Int();
	`}
end

###############################################################################

class Stdin
	super IFStream

	init do
		_file = new NativeFile.native_stdin
		path = "/dev/stdin"
		prepare_buffer(1)
	end

	redef fun poll_in: Bool is extern "file_stdin_poll_in"
end

class Stdout
	super OFStream
	init do
		_file = new NativeFile.native_stdout
		path = "/dev/stdout"
		_is_writable = true
	end
end

class Stderr
	super OFStream
	init do
		_file = new NativeFile.native_stderr
		path = "/dev/stderr"
		_is_writable = true
	end
end

###############################################################################

redef class Streamable
	# Like `write_to` but take care of creating the file
	fun write_to_file(filepath: String)
	do
		var stream = new OFStream.open(filepath)
		write_to(stream)
		stream.close
	end
end

redef class String
	# return true if a file with this names exists
	fun file_exists: Bool do return to_cstring.file_exists

	# The status of a file. see POSIX stat(2).
	fun file_stat: FileStat do return to_cstring.file_stat

	# The status of a file or of a symlink. see POSIX lstat(2).
	fun file_lstat: FileStat do return to_cstring.file_lstat

	# Remove a file, return true if success
	fun file_delete: Bool do return to_cstring.file_delete

	# Copy content of file at `self` to `dest`
	fun file_copy_to(dest: String)
	do
		var input = new IFStream.open(self)
		var output = new OFStream.open(dest)

		while not input.eof do
			var buffer = input.read(1024)
			output.write buffer
		end

		input.close
		output.close
	end

	# Remove the trailing extension `ext`.
	#
	# `ext` usually starts with a dot but could be anything.
	#
	#     assert "file.txt".strip_extension(".txt")  == "file"
	#     assert "file.txt".strip_extension("le.txt")  == "fi"
	#     assert "file.txt".strip_extension("xt")  == "file.t"
	#
	# if `ext` is not present, `self` is returned unmodified.
	#
	#     assert "file.txt".strip_extension(".tar.gz")  == "file.txt"
	fun strip_extension(ext: String): String
	do
		if has_suffix(ext) then
			return substring(0, length - ext.length)
		end
		return self
	end

	# Extract the basename of a path and remove the extension
	#
	#     assert "/path/to/a_file.ext".basename(".ext")         == "a_file"
	#     assert "path/to/a_file.ext".basename(".ext")          == "a_file"
	#     assert "path/to".basename(".ext")                     == "to"
	#     assert "path/to/".basename(".ext")                    == "to"
	#     assert "path".basename("")                        == "path"
	#     assert "/path".basename("")                       == "path"
	#     assert "/".basename("")                           == "/"
	#     assert "".basename("")                            == ""
	fun basename(ext: String): String
	do
		var l = length - 1 # Index of the last char
		while l > 0 and self.chars[l] == '/' do l -= 1 # remove all trailing `/`
		if l == 0 then return "/"
		var pos = chars.last_index_of_from('/', l)
		var n = self
		if pos >= 0 then
			n = substring(pos+1, l-pos)
		end
		return n.strip_extension(ext)
	end

	# Extract the dirname of a path
	#
	#     assert "/path/to/a_file.ext".dirname         == "/path/to"
	#     assert "path/to/a_file.ext".dirname          == "path/to"
	#     assert "path/to".dirname                     == "path"
	#     assert "path/to/".dirname                    == "path"
	#     assert "path".dirname                        == "."
	#     assert "/path".dirname                       == "/"
	#     assert "/".dirname                           == "/"
	#     assert "".dirname                            == "."
	fun dirname: String
	do
		var l = length - 1 # Index of the last char
		while l > 0 and self.chars[l] == '/' do l -= 1 # remove all trailing `/`
		var pos = chars.last_index_of_from('/', l)
		if pos > 0 then
			return substring(0, pos)
		else if pos == 0 then
			return "/"
		else
			return "."
		end
	end

	# Return the canonicalized absolute pathname (see POSIX function `realpath`)
	fun realpath: String do
		var cs = to_cstring.file_realpath
		var res = cs.to_s_with_copy
		# cs.free_malloc # FIXME memory leak
		return res
	end

	# Simplify a file path by remove useless ".", removing "//", and resolving ".."
	# ".." are not resolved if they start the path
	# starting "/" is not removed
	# trainling "/" is removed
	#
	# Note that the method only wonrk on the string:
	#  * no I/O access is performed
	#  * the validity of the path is not checked
	#
	#     assert "some/./complex/../../path/from/../to/a////file//".simplify_path	     ==  "path/to/a/file"
	#     assert "../dir/file".simplify_path       ==  "../dir/file"
	#     assert "dir/../../".simplify_path        ==  ".."
	#     assert "dir/..".simplify_path            ==  "."
	#     assert "//absolute//path/".simplify_path ==  "/absolute/path"
	#     assert "//absolute//../".simplify_path   ==  "/"
	fun simplify_path: String
	do
		var a = self.split_with("/")
		var a2 = new Array[String]
		for x in a do
			if x == "." then continue
			if x == "" and not a2.is_empty then continue
			if x == ".." and not a2.is_empty and a2.last != ".." then
				a2.pop
				continue
			end
			a2.push(x)
		end
		if a2.is_empty then return "."
		if a2.length == 1 and a2.first == "" then return "/"
		return a2.join("/")
	end

	# Correctly join two path using the directory separator.
	#
	# Using a standard "{self}/{path}" does not work in the following cases:
	#
	# * `self` is empty.
	# * `path` ends with `'/'`.
	# * `path` starts with `'/'`.
	#
	# This method ensures that the join is valid.
	#
	#     assert "hello".join_path("world")   == "hello/world"
	#     assert "hel/lo".join_path("wor/ld") == "hel/lo/wor/ld"
	#     assert "".join_path("world")        == "world"
	#     assert "hello".join_path("/world")  == "/world"
	#     assert "hello/".join_path("world")  == "hello/world"
	#     assert "hello/".join_path("/world") == "/world"
	#
	# Note: You may want to use `simplify_path` on the result.
	#
	# Note: This method works only with POSIX paths.
	fun join_path(path: String): String
	do
		if path.is_empty then return self
		if self.is_empty then return path
		if path.chars[0] == '/' then return path
		if self.last == '/' then return "{self}{path}"
		return "{self}/{path}"
	end

	# Convert the path (`self`) to a program name.
	#
	# Ensure the path (`self`) will be treated as-is by POSIX shells when it is
	# used as a program name. In order to do that, prepend `./` if needed.
	#
	#     assert "foo".to_program_name == "./foo"
	#     assert "/foo".to_program_name == "/foo"
	#     assert "".to_program_name == "./" # At least, your shell will detect the error.
	fun to_program_name: String do
		if self.has_prefix("/") then
			return self
		else
			return "./{self}"
		end
	end

	# Alias for `join_path`
	#
	#     assert "hello" / "world"      ==  "hello/world"
	#     assert "hel/lo" / "wor/ld"    ==  "hel/lo/wor/ld"
	#     assert "" / "world"           ==  "world"
	#     assert "/hello" / "/world"    ==  "/world"
	#
	# This operator is quite useful for chaining changes of path.
	# The next one being relative to the previous one.
	#
	#     var a = "foo"
	#     var b = "/bar"
	#     var c = "baz/foobar"
	#     assert a/b/c == "/bar/baz/foobar"
	fun /(path: String): String do return join_path(path)

	# Returns the relative path needed to go from `self` to `dest`.
	#
	#     assert "/foo/bar".relpath("/foo/baz") == "../baz"
	#     assert "/foo/bar".relpath("/baz/bar") == "../../baz/bar"
	#
	# If `self` or `dest` is relative, they are considered relatively to `getcwd`.
	#
	# In some cases, the result is still independent of the current directory:
	#
	#     assert "foo/bar".relpath("..") == "../../.."
	#
	# In other cases, parts of the current directory may be exhibited:
	#
	#     var p = "../foo/bar".relpath("baz")
	#     var c = getcwd.basename("")
	#     assert p == "../../{c}/baz"
	#
	# For path resolution independent of the current directory (eg. for paths in URL),
	# or to use an other starting directory than the current directory,
	# just force absolute paths:
	#
	#     var start = "/a/b/c/d"
	#     var p2 = (start/"../foo/bar").relpath(start/"baz")
	#     assert p2 == "../../d/baz"
	#
	#
	# Neither `self` or `dest` has to be real paths or to exist in directories since
	# the resolution is only done with string manipulations and without any access to
	# the underlying file system.
	#
	# If `self` and `dest` are the same directory, the empty string is returned:
	#
	#     assert "foo".relpath("foo") == ""
	#     assert "foo/../bar".relpath("bar") == ""
	#
	# The empty string and "." designate both the current directory:
	#
	#     assert "".relpath("foo/bar")  == "foo/bar"
	#     assert ".".relpath("foo/bar") == "foo/bar"
	#     assert "foo/bar".relpath("")  == "../.."
	#     assert "/" + "/".relpath(".") == getcwd
	fun relpath(dest: String): String
	do
		var cwd = getcwd
		var from = (cwd/self).simplify_path.split("/")
		if from.last.is_empty then from.pop # case for the root directory
		var to = (cwd/dest).simplify_path.split("/")
		if to.last.is_empty then to.pop # case for the root directory

		# Remove common prefixes
		while not from.is_empty and not to.is_empty and from.first == to.first do
			from.shift
			to.shift
		end

		# Result is going up in `from` with ".." then going down following `to`
		var from_len = from.length
		if from_len == 0 then return to.join("/")
		var up = "../"*(from_len-1) + ".."
		if to.is_empty then return up
		var res = up + "/" + to.join("/")
		return res
	end

	# Create a directory (and all intermediate directories if needed)
	fun mkdir
	do
		var dirs = self.split_with("/")
		var path = new FlatBuffer
		if dirs.is_empty then return
		if dirs[0].is_empty then
			# it was a starting /
			path.add('/')
		end
		for d in dirs do
			if d.is_empty then continue
			path.append(d)
			path.add('/')
			path.to_s.to_cstring.file_mkdir
		end
	end

	# Delete a directory and all of its content, return `true` on success
	#
	# Does not go through symbolic links and may get stuck in a cycle if there
	# is a cycle in the filesystem.
	fun rmdir: Bool
	do
		var ok = true
		for file in self.files do
			var file_path = self.join_path(file)
			var stat = file_path.file_lstat
			if stat.is_dir then
				ok = file_path.rmdir and ok
			else
				ok = file_path.file_delete and ok
			end
			stat.free
		end

		# Delete the directory itself
		if ok then to_cstring.rmdir

		return ok
	end

	# Change the current working directory
	#
	#     "/etc".chdir
	#     assert getcwd == "/etc"
	#     "..".chdir
	#     assert getcwd == "/"
	#
	# TODO: errno
	fun chdir do to_cstring.file_chdir

	# Return right-most extension (without the dot)
	#
	# Only the last extension is returned.
	# There is no special case for combined extensions.
	#
	#     assert "file.txt".file_extension      == "txt"
	#     assert "file.tar.gz".file_extension   == "gz"
	#
	# For file without extension, `null` is returned.
	# Hoever, for trailing dot, `""` is returned.
	#
	#     assert "file".file_extension          == null
	#     assert "file.".file_extension         == ""
	#
	# The starting dot of hidden files is never considered.
	#
	#     assert ".file.txt".file_extension     == "txt"
	#     assert ".file".file_extension         == null
	fun file_extension: nullable String
	do
		var last_slash = chars.last_index_of('.')
		if last_slash > 0 then
			return substring( last_slash+1, length )
		else
			return null
		end
	end

	# returns files contained within the directory represented by self
	fun files : Set[ String ] is extern import HashSet[String], HashSet[String].add, NativeString.to_s, String.to_cstring, HashSet[String].as(Set[String]) `{
		char *dir_path;
		DIR *dir;

		dir_path = String_to_cstring( recv );
		if ((dir = opendir(dir_path)) == NULL)
		{
			perror( dir_path );
			exit( 1 );
		}
		else
		{
			HashSet_of_String results;
			String file_name;
			struct dirent *de;

			results = new_HashSet_of_String();

			while ( ( de = readdir( dir ) ) != NULL )
				if ( strcmp( de->d_name, ".." ) != 0 &&
					strcmp( de->d_name, "." ) != 0 )
				{
					file_name = NativeString_to_s( strdup( de->d_name ) );
					HashSet_of_String_add( results, file_name );
				}

			closedir( dir );
			return HashSet_of_String_as_Set_of_String( results );
		}
	`}
end

redef class NativeString
	private fun file_exists: Bool is extern "string_NativeString_NativeString_file_exists_0"
	private fun file_stat: FileStat is extern "string_NativeString_NativeString_file_stat_0"
	private fun file_lstat: FileStat `{
		struct stat* stat_element;
		int res;
		stat_element = malloc(sizeof(struct stat));
		res = lstat(recv, stat_element);
		if (res == -1) return NULL;
		return stat_element;
	`}
	private fun file_mkdir: Bool is extern "string_NativeString_NativeString_file_mkdir_0"
	private fun rmdir: Bool `{ return rmdir(recv); `}
	private fun file_delete: Bool is extern "string_NativeString_NativeString_file_delete_0"
	private fun file_chdir is extern "string_NativeString_NativeString_file_chdir_0"
	private fun file_realpath: NativeString is extern "file_NativeString_realpath"
end

# This class is system dependent ... must reify the vfs
extern class FileStat `{ struct stat * `}
	# Returns the permission bits of file
	fun mode: Int is extern "file_FileStat_FileStat_mode_0"
	# Returns the last access time
	fun atime: Int is extern "file_FileStat_FileStat_atime_0"
	# Returns the last status change time 
	fun ctime: Int is extern "file_FileStat_FileStat_ctime_0"
	# Returns the last modification time
	fun mtime: Int is extern "file_FileStat_FileStat_mtime_0"
	# Returns the size
	fun size: Int is extern "file_FileStat_FileStat_size_0"

	# Returns true if it is a regular file (not a device file, pipe, sockect, ...)
	fun is_reg: Bool `{ return S_ISREG(recv->st_mode); `}
	# Returns true if it is a directory
	fun is_dir: Bool `{ return S_ISDIR(recv->st_mode); `}
	# Returns true if it is a character device
	fun is_chr: Bool `{ return S_ISCHR(recv->st_mode); `}
	# Returns true if it is a block device
	fun is_blk: Bool `{ return S_ISBLK(recv->st_mode); `}
	# Returns true if the type is fifo
	fun is_fifo: Bool `{ return S_ISFIFO(recv->st_mode); `}
	# Returns true if the type is a link
	fun is_lnk: Bool `{ return S_ISLNK(recv->st_mode); `}
	# Returns true if the type is a socket
	fun is_sock: Bool `{ return S_ISSOCK(recv->st_mode); `}
end

# Instance of this class are standard FILE * pointers
private extern class NativeFile `{ FILE* `}
	fun io_read(buf: NativeString, len: Int): Int is extern "file_NativeFile_NativeFile_io_read_2"
	fun io_write(buf: NativeString, len: Int): Int is extern "file_NativeFile_NativeFile_io_write_2"
	fun io_close: Int is extern "file_NativeFile_NativeFile_io_close_0"
	fun file_stat: FileStat is extern "file_NativeFile_NativeFile_file_stat_0"
	fun fileno: Int `{ return fileno(recv); `}

	new io_open_read(path: NativeString) is extern "file_NativeFileCapable_NativeFileCapable_io_open_read_1"
	new io_open_write(path: NativeString) is extern "file_NativeFileCapable_NativeFileCapable_io_open_write_1"
	new native_stdin is extern "file_NativeFileCapable_NativeFileCapable_native_stdin_0"
	new native_stdout is extern "file_NativeFileCapable_NativeFileCapable_native_stdout_0"
	new native_stderr is extern "file_NativeFileCapable_NativeFileCapable_native_stderr_0"
end

redef class Sys

	# Standard input
	var stdin: PollableIStream = new Stdin is protected writable

	# Standard output
	var stdout: OStream = new Stdout is protected writable

	# Standard output for errors
	var stderr: OStream = new Stderr is protected writable

end

# Print `objects` on the standard output (`stdout`).
protected fun printn(objects: Object...)
do
	sys.stdout.write(objects.to_s)
end

# Print an `object` on the standard output (`stdout`) and add a newline.
protected fun print(object: Object)
do
	sys.stdout.write(object.to_s)
	sys.stdout.write("\n")
end

# Read a character from the standard input (`stdin`).
protected fun getc: Char
do
	return sys.stdin.read_char.ascii
end

# Read a line from the standard input (`stdin`).
protected fun gets: String
do
	return sys.stdin.read_line
end

# Return the working (current) directory
protected fun getcwd: String do return file_getcwd.to_s
private fun file_getcwd: NativeString is extern "string_NativeString_NativeString_file_getcwd_0"
