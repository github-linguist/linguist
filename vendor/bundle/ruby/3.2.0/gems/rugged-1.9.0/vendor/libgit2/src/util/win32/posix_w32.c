/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"

#include "../posix.h"
#include "../futils.h"
#include "fs_path.h"
#include "path_w32.h"
#include "utf-conv.h"
#include "reparse.h"
#include <errno.h>
#include <io.h>
#include <fcntl.h>
#include <ws2tcpip.h>

#ifndef FILE_NAME_NORMALIZED
# define FILE_NAME_NORMALIZED 0
#endif

#ifndef IO_REPARSE_TAG_SYMLINK
#define IO_REPARSE_TAG_SYMLINK (0xA000000CL)
#endif

#ifndef SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
# define SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE 0x02
#endif

#ifndef SYMBOLIC_LINK_FLAG_DIRECTORY
# define SYMBOLIC_LINK_FLAG_DIRECTORY 0x01
#endif

/* Allowable mode bits on Win32.  Using mode bits that are not supported on
 * Win32 (eg S_IRWXU) is generally ignored, but Wine warns loudly about it
 * so we simply remove them.
 */
#define WIN32_MODE_MASK (_S_IREAD | _S_IWRITE)

unsigned long git_win32__createfile_sharemode =
 FILE_SHARE_READ | FILE_SHARE_WRITE;
int git_win32__retries = 10;

GIT_INLINE(void) set_errno(void)
{
	switch (GetLastError()) {
	case ERROR_FILE_NOT_FOUND:
	case ERROR_PATH_NOT_FOUND:
	case ERROR_INVALID_DRIVE:
	case ERROR_NO_MORE_FILES:
	case ERROR_BAD_NETPATH:
	case ERROR_BAD_NET_NAME:
	case ERROR_BAD_PATHNAME:
	case ERROR_FILENAME_EXCED_RANGE:
		errno = ENOENT;
		break;
	case ERROR_BAD_ENVIRONMENT:
		errno = E2BIG;
		break;
	case ERROR_BAD_FORMAT:
	case ERROR_INVALID_STARTING_CODESEG:
	case ERROR_INVALID_STACKSEG:
	case ERROR_INVALID_MODULETYPE:
	case ERROR_INVALID_EXE_SIGNATURE:
	case ERROR_EXE_MARKED_INVALID:
	case ERROR_BAD_EXE_FORMAT:
	case ERROR_ITERATED_DATA_EXCEEDS_64k:
	case ERROR_INVALID_MINALLOCSIZE:
	case ERROR_DYNLINK_FROM_INVALID_RING:
	case ERROR_IOPL_NOT_ENABLED:
	case ERROR_INVALID_SEGDPL:
	case ERROR_AUTODATASEG_EXCEEDS_64k:
	case ERROR_RING2SEG_MUST_BE_MOVABLE:
	case ERROR_RELOC_CHAIN_XEEDS_SEGLIM:
	case ERROR_INFLOOP_IN_RELOC_CHAIN:
		errno = ENOEXEC;
		break;
	case ERROR_INVALID_HANDLE:
	case ERROR_INVALID_TARGET_HANDLE:
	case ERROR_DIRECT_ACCESS_HANDLE:
		errno = EBADF;
		break;
	case ERROR_WAIT_NO_CHILDREN:
	case ERROR_CHILD_NOT_COMPLETE:
		errno = ECHILD;
		break;
	case ERROR_NO_PROC_SLOTS:
	case ERROR_MAX_THRDS_REACHED:
	case ERROR_NESTING_NOT_ALLOWED:
		errno = EAGAIN;
		break;
	case ERROR_ARENA_TRASHED:
	case ERROR_NOT_ENOUGH_MEMORY:
	case ERROR_INVALID_BLOCK:
	case ERROR_NOT_ENOUGH_QUOTA:
		errno = ENOMEM;
		break;
	case ERROR_ACCESS_DENIED:
	case ERROR_CURRENT_DIRECTORY:
	case ERROR_WRITE_PROTECT:
	case ERROR_BAD_UNIT:
	case ERROR_NOT_READY:
	case ERROR_BAD_COMMAND:
	case ERROR_CRC:
	case ERROR_BAD_LENGTH:
	case ERROR_SEEK:
	case ERROR_NOT_DOS_DISK:
	case ERROR_SECTOR_NOT_FOUND:
	case ERROR_OUT_OF_PAPER:
	case ERROR_WRITE_FAULT:
	case ERROR_READ_FAULT:
	case ERROR_GEN_FAILURE:
	case ERROR_SHARING_VIOLATION:
	case ERROR_LOCK_VIOLATION:
	case ERROR_WRONG_DISK:
	case ERROR_SHARING_BUFFER_EXCEEDED:
	case ERROR_NETWORK_ACCESS_DENIED:
	case ERROR_CANNOT_MAKE:
	case ERROR_FAIL_I24:
	case ERROR_DRIVE_LOCKED:
	case ERROR_SEEK_ON_DEVICE:
	case ERROR_NOT_LOCKED:
	case ERROR_LOCK_FAILED:
		errno = EACCES;
		break;
	case ERROR_FILE_EXISTS:
	case ERROR_ALREADY_EXISTS:
		errno = EEXIST;
		break;
	case ERROR_NOT_SAME_DEVICE:
		errno = EXDEV;
		break;
	case ERROR_INVALID_FUNCTION:
	case ERROR_INVALID_ACCESS:
	case ERROR_INVALID_DATA:
	case ERROR_INVALID_PARAMETER:
	case ERROR_NEGATIVE_SEEK:
		errno = EINVAL;
		break;
	case ERROR_TOO_MANY_OPEN_FILES:
		errno = EMFILE;
		break;
	case ERROR_DISK_FULL:
		errno = ENOSPC;
		break;
	case ERROR_BROKEN_PIPE:
		errno = EPIPE;
		break;
	case ERROR_DIR_NOT_EMPTY:
		errno = ENOTEMPTY;
		break;
	default:
		errno = EINVAL;
	}
}

GIT_INLINE(bool) last_error_retryable(void)
{
	int os_error = GetLastError();

	return (os_error == ERROR_SHARING_VIOLATION ||
		os_error == ERROR_ACCESS_DENIED);
}

#define do_with_retries(fn, remediation) \
	do {                                                             \
		int __retry, __ret;                                          \
		for (__retry = git_win32__retries; __retry; __retry--) {     \
			if ((__ret = (fn)) != GIT_RETRY)                         \
				return __ret;                                        \
			if (__retry > 1 && (__ret = (remediation)) != 0) {       \
				if (__ret == GIT_RETRY)                              \
					continue;                                        \
				return __ret;                                        \
			}                                                        \
			Sleep(5);                                                \
		}                                                            \
		return -1;                                                   \
	} while (0)                                                      \

static int ensure_writable(wchar_t *path)
{
	DWORD attrs;

	if ((attrs = GetFileAttributesW(path)) == INVALID_FILE_ATTRIBUTES)
		goto on_error;

	if ((attrs & FILE_ATTRIBUTE_READONLY) == 0)
		return 0;

	if (!SetFileAttributesW(path, (attrs & ~FILE_ATTRIBUTE_READONLY)))
		goto on_error;

	return GIT_RETRY;

on_error:
	set_errno();
	return -1;
}

/**
 * Truncate or extend file.
 *
 * We now take a "git_off_t" rather than "long" because
 * files may be longer than 2Gb.
 */
int p_ftruncate(int fd, off64_t size)
{
	if (size < 0) {
		errno = EINVAL;
		return -1;
	}

#if !defined(__MINGW32__) || defined(MINGW_HAS_SECURE_API)
	return ((_chsize_s(fd, size) == 0) ? 0 : -1);
#else
	/* TODO MINGW32 Find a replacement for _chsize() that handles big files. */
	if (size > INT32_MAX) {
		errno = EFBIG;
		return -1;
	}
	return _chsize(fd, (long)size);
#endif
}

int p_mkdir(const char *path, mode_t mode)
{
	git_win32_path buf;

	GIT_UNUSED(mode);

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	return _wmkdir(buf);
}

int p_link(const char *old, const char *new)
{
	GIT_UNUSED(old);
	GIT_UNUSED(new);
	errno = ENOSYS;
	return -1;
}

GIT_INLINE(int) unlink_once(const wchar_t *path)
{
	DWORD error;

	if (DeleteFileW(path))
		return 0;

	if ((error = GetLastError()) == ERROR_ACCESS_DENIED) {
		WIN32_FILE_ATTRIBUTE_DATA fdata;
		if (!GetFileAttributesExW(path, GetFileExInfoStandard, &fdata) ||
		    !(fdata.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) ||
		    !(fdata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
			goto out;

		if (RemoveDirectoryW(path))
			return 0;
	}

out:
	SetLastError(error);

	if (last_error_retryable())
		return GIT_RETRY;

	set_errno();
	return -1;
}

int p_unlink(const char *path)
{
	git_win32_path wpath;

	if (git_win32_path_from_utf8(wpath, path) < 0)
		return -1;

	do_with_retries(unlink_once(wpath), ensure_writable(wpath));
}

int p_fsync(int fd)
{
	HANDLE fh = (HANDLE)_get_osfhandle(fd);

	p_fsync__cnt++;

	if (fh == INVALID_HANDLE_VALUE) {
		errno = EBADF;
		return -1;
	}

	if (!FlushFileBuffers(fh)) {
		DWORD code = GetLastError();

		if (code == ERROR_INVALID_HANDLE)
			errno = EINVAL;
		else
			errno = EIO;

		return -1;
	}

	return 0;
}

#define WIN32_IS_WSEP(CH) ((CH) == L'/' || (CH) == L'\\')

static int lstat_w(
	wchar_t *path,
	struct stat *buf,
	bool posix_enotdir)
{
	WIN32_FILE_ATTRIBUTE_DATA fdata;

	if (GetFileAttributesExW(path, GetFileExInfoStandard, &fdata)) {
		if (!buf)
			return 0;

		return git_win32__file_attribute_to_stat(buf, &fdata, path);
	}

	switch (GetLastError()) {
	case ERROR_ACCESS_DENIED:
		errno = EACCES;
		break;
	default:
		errno = ENOENT;
		break;
	}

	/* To match POSIX behavior, set ENOTDIR when any of the folders in the
	 * file path is a regular file, otherwise set ENOENT.
	 */
	if (errno == ENOENT && posix_enotdir) {
		size_t path_len = wcslen(path);

		/* scan up path until we find an existing item */
		while (1) {
			DWORD attrs;

			/* remove last directory component */
			for (path_len--; path_len > 0 && !WIN32_IS_WSEP(path[path_len]); path_len--);

			if (path_len <= 0)
				break;

			path[path_len] = L'\0';
			attrs = GetFileAttributesW(path);

			if (attrs != INVALID_FILE_ATTRIBUTES) {
				if (!(attrs & FILE_ATTRIBUTE_DIRECTORY))
					errno = ENOTDIR;
				break;
			}
		}
	}

	return -1;
}

static int do_lstat(const char *path, struct stat *buf, bool posixly_correct)
{
	git_win32_path path_w;
	int len;

	if ((len = git_win32_path_from_utf8(path_w, path)) < 0)
		return -1;

	git_win32_path_trim_end(path_w, len);

	return lstat_w(path_w, buf, posixly_correct);
}

int p_lstat(const char *filename, struct stat *buf)
{
	return do_lstat(filename, buf, false);
}

int p_lstat_posixly(const char *filename, struct stat *buf)
{
	return do_lstat(filename, buf, true);
}

int p_readlink(const char *path, char *buf, size_t bufsiz)
{
	git_win32_path path_w, target_w;
	git_win32_utf8_path target;
	int len;

	/* readlink(2) does not NULL-terminate the string written
	 * to the target buffer. Furthermore, the target buffer need
	 * not be large enough to hold the entire result. A truncated
	 * result should be written in this case. Since this truncation
	 * could occur in the middle of the encoding of a code point,
	 * we need to buffer the result on the stack. */

	if (git_win32_path_from_utf8(path_w, path) < 0 ||
		git_win32_path_readlink_w(target_w, path_w) < 0 ||
		(len = git_win32_path_to_utf8(target, target_w)) < 0)
		return -1;

	bufsiz = min((size_t)len, bufsiz);
	memcpy(buf, target, bufsiz);

	return (int)bufsiz;
}

static bool target_is_dir(const char *target, const char *path)
{
	git_str resolved = GIT_STR_INIT;
	git_win32_path resolved_w;
	bool isdir = true;

	if (git_fs_path_is_absolute(target))
		git_win32_path_from_utf8(resolved_w, target);
	else if (git_fs_path_dirname_r(&resolved, path) < 0 ||
		 git_fs_path_apply_relative(&resolved, target) < 0 ||
		 git_win32_path_from_utf8(resolved_w, resolved.ptr) < 0)
		goto out;

	isdir = GetFileAttributesW(resolved_w) & FILE_ATTRIBUTE_DIRECTORY;

out:
	git_str_dispose(&resolved);
	return isdir;
}

int p_symlink(const char *target, const char *path)
{
	git_win32_path target_w, path_w;
	DWORD dwFlags;

	/*
	 * Convert both target and path to Windows-style paths. Note that we do
	 * not want to use `git_win32_path_from_utf8` for converting the target,
	 * as that function will automatically pre-pend the current working
	 * directory in case the path is not absolute. As Git will instead use
	 * relative symlinks, this is not something we want.
	 */
	if (git_win32_path_from_utf8(path_w, path) < 0 ||
	    git_win32_path_relative_from_utf8(target_w, target) < 0)
		return -1;

	dwFlags = SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;
	if (target_is_dir(target, path))
		dwFlags |= SYMBOLIC_LINK_FLAG_DIRECTORY;

	if (!CreateSymbolicLinkW(path_w, target_w, dwFlags))
		return -1;

	return 0;
}

struct open_opts {
	DWORD access;
	DWORD sharing;
	SECURITY_ATTRIBUTES security;
	DWORD creation_disposition;
	DWORD attributes;
	int osf_flags;
};

GIT_INLINE(void) open_opts_from_posix(struct open_opts *opts, int flags, mode_t mode)
{
	memset(opts, 0, sizeof(struct open_opts));

	switch (flags & (O_WRONLY | O_RDWR)) {
	case O_WRONLY:
		opts->access = GENERIC_WRITE;
		break;
	case O_RDWR:
		opts->access = GENERIC_READ | GENERIC_WRITE;
		break;
	default:
		opts->access = GENERIC_READ;
		break;
	}

	opts->sharing = (DWORD)git_win32__createfile_sharemode;

	switch (flags & (O_CREAT | O_TRUNC | O_EXCL)) {
	case O_CREAT | O_EXCL:
	case O_CREAT | O_TRUNC | O_EXCL:
		opts->creation_disposition = CREATE_NEW;
		break;
	case O_CREAT | O_TRUNC:
		opts->creation_disposition = CREATE_ALWAYS;
		break;
	case O_TRUNC:
		opts->creation_disposition = TRUNCATE_EXISTING;
		break;
	case O_CREAT:
		opts->creation_disposition = OPEN_ALWAYS;
		break;
	default:
		opts->creation_disposition = OPEN_EXISTING;
		break;
	}

	opts->attributes = ((flags & O_CREAT) && !(mode & S_IWRITE)) ?
		FILE_ATTRIBUTE_READONLY : FILE_ATTRIBUTE_NORMAL;
	opts->osf_flags = flags & (O_RDONLY | O_APPEND);

	opts->security.nLength = sizeof(SECURITY_ATTRIBUTES);
	opts->security.lpSecurityDescriptor = NULL;
	opts->security.bInheritHandle = 0;
}

GIT_INLINE(int) open_once(
	const wchar_t *path,
	struct open_opts *opts)
{
	int fd;

	HANDLE handle = CreateFileW(path, opts->access, opts->sharing,
		&opts->security, opts->creation_disposition, opts->attributes, 0);

	if (handle == INVALID_HANDLE_VALUE) {
		if (last_error_retryable())
			return GIT_RETRY;

		set_errno();
		return -1;
	}

	if ((fd = _open_osfhandle((intptr_t)handle, opts->osf_flags)) < 0)
		CloseHandle(handle);

	return fd;
}

int p_open(const char *path, int flags, ...)
{
	git_win32_path wpath;
	mode_t mode = 0;
	struct open_opts opts = {0};

	#ifdef GIT_DEBUG_STRICT_OPEN
	if (strstr(path, "//") != NULL) {
		errno = EACCES;
		return -1;
	}
	#endif

	if (git_win32_path_from_utf8(wpath, path) < 0)
		return -1;

	if (flags & O_CREAT) {
		va_list arg_list;

		va_start(arg_list, flags);
		mode = (mode_t)va_arg(arg_list, int);
		va_end(arg_list);
	}

	open_opts_from_posix(&opts, flags, mode);

	do_with_retries(
		open_once(wpath, &opts),
		0);
}

int p_creat(const char *path, mode_t mode)
{
	return p_open(path, O_WRONLY | O_CREAT | O_TRUNC, mode);
}

int p_utimes(const char *path, const struct p_timeval times[2])
{
	git_win32_path wpath;
	int fd, error;
	DWORD attrs_orig, attrs_new = 0;
	struct open_opts opts = { 0 };

	if (git_win32_path_from_utf8(wpath, path) < 0)
		return -1;

	attrs_orig = GetFileAttributesW(wpath);

	if (attrs_orig & FILE_ATTRIBUTE_READONLY) {
		attrs_new = attrs_orig & ~FILE_ATTRIBUTE_READONLY;

		if (!SetFileAttributesW(wpath, attrs_new)) {
			git_error_set(GIT_ERROR_OS, "failed to set attributes");
			return -1;
		}
	}

	open_opts_from_posix(&opts, O_RDWR, 0);

	if ((fd = open_once(wpath, &opts)) < 0) {
		error = -1;
		goto done;
	}

	error = p_futimes(fd, times);
	close(fd);

done:
	if (attrs_orig != attrs_new) {
		DWORD os_error = GetLastError();
		SetFileAttributesW(wpath, attrs_orig);
		SetLastError(os_error);
	}

	return error;
}

int p_futimes(int fd, const struct p_timeval times[2])
{
	HANDLE handle;
	FILETIME atime = { 0 }, mtime = { 0 };

	if (times == NULL) {
		SYSTEMTIME st;

		GetSystemTime(&st);
		SystemTimeToFileTime(&st, &atime);
		SystemTimeToFileTime(&st, &mtime);
	}
	else {
		git_win32__timeval_to_filetime(&atime, times[0]);
		git_win32__timeval_to_filetime(&mtime, times[1]);
	}

	if ((handle = (HANDLE)_get_osfhandle(fd)) == INVALID_HANDLE_VALUE)
		return -1;

	if (SetFileTime(handle, NULL, &atime, &mtime) == 0)
		return -1;

	return 0;
}

int p_getcwd(char *buffer_out, size_t size)
{
	git_win32_path buf;
	wchar_t *cwd = _wgetcwd(buf, GIT_WIN_PATH_UTF16);

	if (!cwd)
		return -1;

	git_win32_path_remove_namespace(cwd, wcslen(cwd));

	/* Convert the working directory back to UTF-8 */
	if (git_utf8_from_16(buffer_out, size, cwd) < 0) {
		DWORD code = GetLastError();

		if (code == ERROR_INSUFFICIENT_BUFFER)
			errno = ERANGE;
		else
			errno = EINVAL;

		return -1;
	}

	git_fs_path_mkposix(buffer_out);
	return 0;
}

static int getfinalpath_w(
	git_win32_path dest,
	const wchar_t *path)
{
	HANDLE hFile;
	DWORD dwChars;

	/* Use FILE_FLAG_BACKUP_SEMANTICS so we can open a directory. Do not
	* specify FILE_FLAG_OPEN_REPARSE_POINT; we want to open a handle to the
	* target of the link. */
	hFile = CreateFileW(path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_DELETE,
		NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

	if (INVALID_HANDLE_VALUE == hFile)
		return -1;

	/* Call GetFinalPathNameByHandle */
	dwChars = GetFinalPathNameByHandleW(hFile, dest, GIT_WIN_PATH_UTF16, FILE_NAME_NORMALIZED);
	CloseHandle(hFile);

	if (!dwChars || dwChars >= GIT_WIN_PATH_UTF16)
		return -1;

	/* The path may be delivered to us with a namespace prefix; remove */
	return (int)git_win32_path_remove_namespace(dest, dwChars);
}

static int follow_and_lstat_link(git_win32_path path, struct stat *buf)
{
	git_win32_path target_w;

	if (getfinalpath_w(target_w, path) < 0)
		return -1;

	return lstat_w(target_w, buf, false);
}

int p_fstat(int fd, struct stat *buf)
{
	BY_HANDLE_FILE_INFORMATION fhInfo;

	HANDLE fh = (HANDLE)_get_osfhandle(fd);

	if (fh == INVALID_HANDLE_VALUE ||
		!GetFileInformationByHandle(fh, &fhInfo)) {
		errno = EBADF;
		return -1;
	}

	git_win32__file_information_to_stat(buf, &fhInfo);
	return 0;
}

int p_stat(const char *path, struct stat *buf)
{
	git_win32_path path_w;
	int len;

	if ((len = git_win32_path_from_utf8(path_w, path)) < 0 ||
		lstat_w(path_w, buf, false) < 0)
		return -1;

	/* The item is a symbolic link or mount point. No need to iterate
	 * to follow multiple links; use GetFinalPathNameFromHandle. */
	if (S_ISLNK(buf->st_mode))
		return follow_and_lstat_link(path_w, buf);

	return 0;
}

int p_chdir(const char *path)
{
	git_win32_path buf;

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	return _wchdir(buf);
}

int p_chmod(const char *path, mode_t mode)
{
	git_win32_path buf;

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	return _wchmod(buf, mode);
}

int p_rmdir(const char *path)
{
	git_win32_path buf;
	int error;

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	error = _wrmdir(buf);

	if (error == -1) {
		switch (GetLastError()) {
			/* _wrmdir() is documented to return EACCES if "A program has an open
			 * handle to the directory."  This sounds like what everybody else calls
			 * EBUSY.  Let's convert appropriate error codes.
			 */
			case ERROR_ACCESS_DENIED:
			case ERROR_SHARING_VIOLATION:
				errno = EBUSY;
				break;

			/* This error can be returned when trying to rmdir an extant file. */
			case ERROR_DIRECTORY:
				errno = ENOTDIR;
				break;
		}
	}

	return error;
}

char *p_realpath(const char *orig_path, char *buffer)
{
	git_win32_path orig_path_w, buffer_w;
	DWORD long_len;

	if (git_win32_path_from_utf8(orig_path_w, orig_path) < 0)
		return NULL;

	/*
	 * POSIX realpath performs two functions: first, it turns relative
	 * paths into absolute paths. For this, we need GetFullPathName.
	 *
	 * Note that if the path provided is a relative path, then the current directory
	 * is used to resolve the path -- which is a concurrency issue because the current
	 * directory is a process-wide variable.
	 */
	if (!GetFullPathNameW(orig_path_w, GIT_WIN_PATH_UTF16, buffer_w, NULL)) {
		if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
			errno = ENAMETOOLONG;
		else
			errno = EINVAL;

		return NULL;
	}

	/*
	 * Then, the path is canonicalized. eg, on macOS,
	 * "/TMP" -> "/private/tmp". For this, we need GetLongPathName.
	 */
	if ((long_len = GetLongPathNameW(buffer_w, buffer_w, GIT_WIN_PATH_UTF16)) == 0) {
		DWORD error = GetLastError();

		if (error == ERROR_FILE_NOT_FOUND ||
		    error == ERROR_PATH_NOT_FOUND)
			errno = ENOENT;
		else if (error == ERROR_ACCESS_DENIED)
			errno = EPERM;
		else
			errno = EINVAL;

		return NULL;
	}

	if (long_len > GIT_WIN_PATH_UTF16) {
		errno = ENAMETOOLONG;
		return NULL;
	}

	if (!buffer && !(buffer = git__malloc(GIT_WIN_PATH_UTF8))) {
		errno = ENOMEM;
		return NULL;
	}

	/* Convert the path to UTF-8. If the caller provided a buffer, then it
	 * is assumed to be GIT_WIN_PATH_UTF8 characters in size. If it isn't,
	 * then we may overflow. */
	if (git_win32_path_to_utf8(buffer, buffer_w) < 0)
		return NULL;

	git_fs_path_mkposix(buffer);
	return buffer;
}

int p_vsnprintf(char *buffer, size_t count, const char *format, va_list argptr)
{
#if defined(_MSC_VER)
	int len;

	if (count == 0)
		return _vscprintf(format, argptr);

	#if _MSC_VER >= 1500
	len = _vsnprintf_s(buffer, count, _TRUNCATE, format, argptr);
	#else
	len = _vsnprintf(buffer, count, format, argptr);
	#endif

	if (len < 0)
		return _vscprintf(format, argptr);

	return len;
#else /* MinGW */
	return vsnprintf(buffer, count, format, argptr);
#endif
}

int p_snprintf(char *buffer, size_t count, const char *format, ...)
{
	va_list va;
	int r;

	va_start(va, format);
	r = p_vsnprintf(buffer, count, format, va);
	va_end(va);

	return r;
}

int p_access(const char *path, mode_t mode)
{
	git_win32_path buf;

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	return _waccess(buf, mode & WIN32_MODE_MASK);
}

GIT_INLINE(int) rename_once(const wchar_t *from, const wchar_t *to)
{
	if (MoveFileExW(from, to, MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED))
		return 0;

	if (last_error_retryable())
		return GIT_RETRY;

	set_errno();
	return -1;
}

int p_rename(const char *from, const char *to)
{
	git_win32_path wfrom, wto;

	if (git_win32_path_from_utf8(wfrom, from) < 0 ||
		git_win32_path_from_utf8(wto, to) < 0)
		return -1;

	do_with_retries(rename_once(wfrom, wto), ensure_writable(wto));
}

int p_recv(GIT_SOCKET socket, void *buffer, size_t length, int flags)
{
	if ((size_t)((int)length) != length)
		return -1; /* git_error_set will be done by caller */

	return recv(socket, buffer, (int)length, flags);
}

int p_send(GIT_SOCKET socket, const void *buffer, size_t length, int flags)
{
	if ((size_t)((int)length) != length)
		return -1; /* git_error_set will be done by caller */

	return send(socket, buffer, (int)length, flags);
}

/**
 * Borrowed from http://old.nabble.com/Porting-localtime_r-and-gmtime_r-td15282276.html
 * On Win32, `gmtime_r` doesn't exist but `gmtime` is threadsafe, so we can use that
 */
struct tm *
p_localtime_r (const time_t *timer, struct tm *result)
{
	struct tm *local_result;
	local_result = localtime (timer);

	if (local_result == NULL || result == NULL)
		return NULL;

	memcpy (result, local_result, sizeof (struct tm));
	return result;
}
struct tm *
p_gmtime_r (const time_t *timer, struct tm *result)
{
	struct tm *local_result;
	local_result = gmtime (timer);

	if (local_result == NULL || result == NULL)
		return NULL;

	memcpy (result, local_result, sizeof (struct tm));
	return result;
}

int p_inet_pton(int af, const char *src, void *dst)
{
	struct sockaddr_storage sin;
	void *addr;
	int sin_len = sizeof(struct sockaddr_storage), addr_len;
	int error = 0;

	if (af == AF_INET) {
		addr = &((struct sockaddr_in *)&sin)->sin_addr;
		addr_len = sizeof(struct in_addr);
	} else if (af == AF_INET6) {
		addr = &((struct sockaddr_in6 *)&sin)->sin6_addr;
		addr_len = sizeof(struct in6_addr);
	} else {
		errno = EAFNOSUPPORT;
		return -1;
	}

	if ((error = WSAStringToAddressA((LPSTR)src, af, NULL, (LPSOCKADDR)&sin, &sin_len)) == 0) {
		memcpy(dst, addr, addr_len);
		return 1;
	}

	switch(WSAGetLastError()) {
	case WSAEINVAL:
		return 0;
	case WSAEFAULT:
		errno = ENOSPC;
		return -1;
	case WSA_NOT_ENOUGH_MEMORY:
		errno = ENOMEM;
		return -1;
	}

	errno = EINVAL;
	return -1;
}

ssize_t p_pread(int fd, void *data, size_t size, off64_t offset)
{
	HANDLE fh;
	DWORD rsize = 0;
	OVERLAPPED ov = {0};
	LARGE_INTEGER pos = {0};
	off64_t final_offset = 0;

	/* Fail if the final offset would have overflowed to match POSIX semantics. */
	if (!git__is_ssizet(size) || git__add_int64_overflow(&final_offset, offset, (int64_t)size)) {
		errno = EINVAL;
		return -1;
	}

	/*
	 * Truncate large writes to the maximum allowable size: the caller
	 * needs to always call this in a loop anyways.
	 */
	if (size > INT32_MAX) {
		size = INT32_MAX;
	}

	pos.QuadPart = offset;
	ov.Offset = pos.LowPart;
	ov.OffsetHigh = pos.HighPart;
	fh = (HANDLE)_get_osfhandle(fd);

	if (ReadFile(fh, data, (DWORD)size, &rsize, &ov)) {
		return (ssize_t)rsize;
	}

	set_errno();
	return -1;
}

ssize_t p_pwrite(int fd, const void *data, size_t size, off64_t offset)
{
	HANDLE fh;
	DWORD wsize = 0;
	OVERLAPPED ov = {0};
	LARGE_INTEGER pos = {0};
	off64_t final_offset = 0;

	/* Fail if the final offset would have overflowed to match POSIX semantics. */
	if (!git__is_ssizet(size) || git__add_int64_overflow(&final_offset, offset, (int64_t)size)) {
		errno = EINVAL;
		return -1;
	}

	/*
	 * Truncate large writes to the maximum allowable size: the caller
	 * needs to always call this in a loop anyways.
	 */
	if (size > INT32_MAX) {
		size = INT32_MAX;
	}

	pos.QuadPart = offset;
	ov.Offset = pos.LowPart;
	ov.OffsetHigh = pos.HighPart;
	fh = (HANDLE)_get_osfhandle(fd);

	if (WriteFile(fh, data, (DWORD)size, &wsize, &ov)) {
		return (ssize_t)wsize;
	}

	set_errno();
	return -1;
}
