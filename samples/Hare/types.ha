// SPDX-License-Identifier: MPL-2.0
// (c) Hare authors <https://harelang.org>

use encoding::utf8;
use errors;
use io;
use strings;
use time;

// An entry of a particular type was sought, but is something else in practice.
// For example, opening a file with [[iter]].
export type wrongtype = !void;

// Returned from [[rename]] if this rename is not possible due to technical
// constraints, such as if it would cause a file to move between filesystems. In
// this situation, other operations (such as copy & remove) may succeed if
// attempted.
export type cannotrename = !void;

// All possible fs error types.
export type error = !(
	errors::noentry |
	errors::noaccess |
	errors::exists |
	errors::busy |
	errors::invalid |
	errors::unsupported |
	utf8::invalid |
	wrongtype |
	cannotrename |
	io::error);

// Returns a human-friendly representation of an error.
export fn strerror(err: error) const str = match (err) {
case wrongtype =>
	yield "Wrong entry type for requested operation";
case cannotrename =>
	yield "Unable to perform rename operation (try move instead)";
case errors::noentry =>
	yield "File or directory not found";
case errors::noaccess =>
	yield "Permission denied";
case errors::exists =>
	yield "File or directory exists";
case errors::invalid =>
	yield "Invalid argument";
case errors::busy =>
	yield "Device is busy";
case errors::unsupported =>
	yield "Operation not supported";
case let err: utf8::invalid =>
	yield utf8::strerror(err);
case let err: io::error =>
	yield io::strerror(err);
};

// File mode information. These bits do not necessarily reflect the underlying
// operating system's mode representation, though they were chosen to be
// consistent with typical Unix file permissions. All implementations shall
// support at least USER_RW, DIR, and REG.
export type mode = enum uint {
	// Read, write, and execute permissions for the file owner
	USER_RWX	= 0o700,
	// Read and write permissions for the file owner
	USER_RW		= 0o600,
	// Read and execute permissions for the file owner
	USER_RX		= 0o500,
	// Read permissions for the file owner
	USER_R		= 0o400,
	// Write permissions for the file owner
	USER_W		= 0o200,
	// Execute permissions for the file owner
	USER_X		= 0o100,

	// Read, write, and execute permissions for group members
	GROUP_RWX	= 0o070,
	// Read and write permissions for group members
	GROUP_RW	= 0o060,
	// Read and execute permissions for group members
	GROUP_RX	= 0o050,
	// Read permissions for group members
	GROUP_R		= 0o040,
	// Write permissions for group members
	GROUP_W		= 0o020,
	// Execute permissions for group members
	GROUP_X		= 0o010,

	// Read, write, and execute permissions for other users
	OTHER_RWX	= 0o007,
	// Read and write permissions for other users
	OTHER_RW	= 0o006,
	// Read and execute permissions for other users
	OTHER_RX	= 0o005,
	// Read permissions for other users
	OTHER_R		= 0o004,
	// Write permissions for other users
	OTHER_W		= 0o002,
	// Execute permissions for other users
	OTHER_X		= 0o001,

	// Entry has the set-uid bit set
	SETUID	= 0o4000,
	// Entry has the set-gid bit set
	SETGID	= 0o2000,
	// Entry has the sticky bit set
	STICKY	= 0o1000,

	// Entry is of an unknown type
	UNKNOWN	= 0,
	// Entry is a FIFO (named pipe)
	FIFO	= 0o010000,
	// Entry is a directory
	DIR	= 0o040000,
	// Entry is a character device
	CHR	= 0o020000,
	// Entry is a block device
	BLK	= 0o060000,
	// Entry is a regular file
	REG	= 0o100000,
	// Entry is a symbolic link
	LINK	= 0o120000,
	// Entry is a Unix socket
	SOCK	= 0o140000,
};

// A mask defining what items are populated in the stat structure.
export type stat_mask = enum uint {
	UID	= 1 << 0,
	GID	= 1 << 1,
	SIZE	= 1 << 2,
	INODE	= 1 << 3,
	ATIME	= 1 << 4,
	MTIME	= 1 << 5,
	CTIME	= 1 << 6,
};

// Information about a file or directory. The mask field defines what other
// fields are set; mode is always set.
export type filestat = struct {
	mask: stat_mask,
	mode: mode,
	uid: uint,
	gid: uint,
	sz: size,
	inode: u64,
	atime: time::instant,
	mtime: time::instant,
	ctime: time::instant,
};

// An entry in a directory. This may be borrowed from the filesystem's internal
// state. If you want to keep this around beyond one call to [[next]], use
// [[dirent_dup]].
export type dirent = struct {
	// The name of this entry. Not fully qualified: for example,
	// "foo/bar/baz.txt" would store "baz.txt" here.
	name: str,

	// The type of this entry. The permission bits may be unset.
	ftype: mode,
};

// Duplicates a [[dirent]] object. Call [[dirent_finish]] to get rid of it
// later.
export fn dirent_dup(e: *dirent) dirent = {
	let new = *e;
	new.name = strings::dup(e.name);
	return new;
};

// Frees memory associated with a [[dirent]] object which was duplicated with
// [[dirent_dup]].
export fn dirent_finish(e: *dirent) void = free(e.name);

// Flags to use for opening a file. Not all operating systems support all flags;
// at a minimum, RDONLY, WRONLY, RDWR, CREATE, and TRUNC will be supported.
// Note that NOCTTY and CLOEXEC are on by default, and the CTTY/NOCLOEXEC flags
// respectively disable them.
export type flag = enum int {
	RDONLY		= 0,
	WRONLY		= 1,
	RDWR		= 2,
	CREATE		= 0o100,
	EXCL		= 0o200,
	CTTY		= 0o400,
	TRUNC		= 0o1000,
	APPEND		= 0o2000,
	NONBLOCK	= 0o4000,
	DSYNC		= 0o10000,
	SYNC		= 0o4010000,
	RSYNC		= 0o4010000,
	DIRECTORY	= 0o200000,
	NOFOLLOW	= 0o400000,
	NOATIME		= 0o1000000,
	NOCLOEXEC	= 0o2000000,
	PATH		= 0o10000000,
	TMPFILE		= 0o20200000,
};

export type closefunc = fn(fs: *fs) void;
export type removefunc = fn(fs: *fs, path: str) (void | error);
export type renamefunc = fn(fs: *fs, oldpath: str, newpath: str) (void | error);
export type iterfunc = fn(fs: *fs, path: str) (*iterator | error);
export type statfunc = fn(fs: *fs, path: str) (filestat | error);
export type fstatfunc = fn(fs: *fs, file: io::file) (filestat | error);
export type mkdirfunc = fn(fs: *fs, path: str, mode: mode) (void | error);
export type rmdirfunc = fn(fs: *fs, path: str) (void | error);
export type chmodfunc = fn(fs: *fs, path: str, mode: mode) (void | error);
export type fchmodfunc = fn(fd: io::file, mode: mode) (void | error);
export type chownfunc = fn(fs: *fs, path: str, uid: uint, gid: uint) (void | error);
export type fchownfunc = fn(fd: io::file, uid: uint, gid: uint) (void | error);
export type chtimesfunc = fn(fs: *fs, path: str, atime: (time::instant | void),
	mtime: (time::instant | void)) (void | error);
export type fchtimesfunc = fn(fd: io::file, atime: (time::instant | void),
	mtime: (time::instant | void)) (void | error);
export type resolvefunc = fn(fs: *fs, path: str) str;
export type readlinkfunc = fn(fs: *fs, path: str) (str | error);
export type linkfunc = fn(fs: *fs, old: str, new: str) (void | error);
export type symlinkfunc = fn(fs: *fs, target: str, path: str) (void | error);

export type openfunc = fn(
	fs: *fs,
	path: str,
	flags: flag,
) (io::handle | error);

export type openfilefunc = fn(
	fs: *fs,
	path: str,
	flags: flag,
) (io::file | error);

export type createfunc = fn(
	fs: *fs,
	path: str,
	mode: mode,
	flags: flag,
) (io::handle | error);

export type createfilefunc = fn(
	fs: *fs,
	path: str,
	mode: mode,
	flags: flag,
) (io::file | error);

// An abstract implementation of a filesystem, which provides common filesystem
// operations such as file creation and deletion, but which may be backed by any
// underlying storage system. See [[os::cwd]] for access to the host filesystem.
//
// To create a custom filesystem implementation, embed this type as the first
// member of a struct with user-specific data and fill out these fields as
// appropriate.
export type fs = struct {
	// Frees resources associated with this filesystem.
	close: nullable *closefunc,

	// Opens a file.
	open: nullable *openfunc,

	// Opens a file as an [[io::file]].
	openfile: nullable *openfilefunc,

	// Creates a new file.
	create: nullable *createfunc,

	// Creates a new file as an [[io::file]].
	createfile: nullable *createfilefunc,

	// Removes a file.
	remove: nullable *removefunc,

	// Renames a file.
	rename: nullable *renamefunc,

	// Returns an iterator for a path, which yields the contents of a
	// directory. Pass empty string to yield from the root.
	//
	// The iterator must return all entries without error. If an error would
	// occur, it should be identified here and returned upfront.
	iter: nullable *iterfunc,

	// Obtains information about a file or directory. If the target is a
	// symbolic link, information is returned about the link, not its
	// target.
	stat: nullable *statfunc,

	// Obtains information about an [[io::file]].
	fstat: nullable *fstatfunc,

	// Returns the path referred to by a symbolic link. The caller will free
	// the return value.
	readlink: nullable *readlinkfunc,

	// Creates a directory.
	mkdir: nullable *mkdirfunc,

	// Removes a directory. The target directory must be empty.
	rmdir: nullable *rmdirfunc,

	// Changes mode flags on a file or directory.
	chmod: nullable *chmodfunc,

	// Changes mode flags on a [[io::file]].
	fchmod: nullable *fchmodfunc,

	// Changes ownership of a file.
	chown: nullable *chownfunc,

	// Changes ownership of a [[io::file]].
	fchown: nullable *fchownfunc,

	// Changes access and modification time of a file.
	chtimes: nullable *chtimesfunc,

	// Changes access and modification time of an [[io::file]].
	fchtimes: nullable *fchtimesfunc,

	// Resolves a path to its absolute, normalized value. If the fs
	// implementation does not provide this, [resolve] presumes that
	// relative paths are rooted (i.e. "foo" == "/foo").
	resolve: nullable *resolvefunc,

	// Creates a new (hard) link.
	link: nullable *linkfunc,

	// Creates a new symbolic link.
	symlink: nullable *symlinkfunc,
};

// A function which returns the next directory from an [[iterator]].
export type nextfunc = fn(iter: *iterator) (dirent | done | error);

// A function which frees state associated with an [[iterator]].
export type finishfunc = fn(iter: *iterator) void;

// A directory iterator. To implement a directory iterator for a filesystem,
// subtype this struct to store any necessary state and populate the pointers
// with your implementation.
export type iterator = struct {
	// Returns the next member of the directory, or done if there are none
	// remaining.
	next: *nextfunc,
	// Frees resources associated with the iterator.
	finish: nullable *finishfunc,
};
