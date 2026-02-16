/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "path_w32.h"

#include "fs_path.h"
#include "utf-conv.h"
#include "posix.h"
#include "reparse.h"
#include "dir.h"

#define PATH__NT_NAMESPACE     L"\\\\?\\"
#define PATH__NT_NAMESPACE_LEN 4

#define PATH__ABSOLUTE_LEN     3

#define path__is_nt_namespace(p) \
	(((p)[0] == '\\' && (p)[1] == '\\' && (p)[2] == '?' && (p)[3] == '\\') || \
	 ((p)[0] == '/' && (p)[1] == '/' && (p)[2] == '?' && (p)[3] == '/'))

#define path__is_unc(p) \
	(((p)[0] == '\\' && (p)[1] == '\\') || ((p)[0] == '/' && (p)[1] == '/'))

#define path__startswith_slash(p) \
	((p)[0] == '\\' || (p)[0] == '/')

GIT_INLINE(int) path__cwd(wchar_t *path, int size)
{
	int len;

	if ((len = GetCurrentDirectoryW(size, path)) == 0) {
		errno = GetLastError() == ERROR_ACCESS_DENIED ? EACCES : ENOENT;
		return -1;
	} else if (len > size) {
		errno = ENAMETOOLONG;
		return -1;
	}

	/* The Win32 APIs may return "\\?\" once you've used it first.
	 * But it may not.  What a gloriously predictable API!
	 */
	if (wcsncmp(path, PATH__NT_NAMESPACE, PATH__NT_NAMESPACE_LEN))
		return len;

	len -= PATH__NT_NAMESPACE_LEN;

	memmove(path, path + PATH__NT_NAMESPACE_LEN, sizeof(wchar_t) * len);
	return len;
}

static wchar_t *path__skip_server(wchar_t *path)
{
	wchar_t *c;

	for (c = path; *c; c++) {
		if (git_fs_path_is_dirsep(*c))
			return c + 1;
	}

	return c;
}

static wchar_t *path__skip_prefix(wchar_t *path)
{
	if (path__is_nt_namespace(path)) {
		path += PATH__NT_NAMESPACE_LEN;

		if (wcsncmp(path, L"UNC\\", 4) == 0)
			path = path__skip_server(path + 4);
		else if (git_fs_path_is_absolute(path))
			path += PATH__ABSOLUTE_LEN;
	} else if (git_fs_path_is_absolute(path)) {
		path += PATH__ABSOLUTE_LEN;
	} else if (path__is_unc(path)) {
		path = path__skip_server(path + 2);
	}

	return path;
}

int git_win32_path_canonicalize(git_win32_path path)
{
	wchar_t *base, *from, *to, *next;
	size_t len;

	base = to = path__skip_prefix(path);

	/* Unposixify if the prefix */
	for (from = path; from < to; from++) {
		if (*from == L'/')
			*from = L'\\';
	}

	while (*from) {
		for (next = from; *next; ++next) {
			if (*next == L'/') {
				*next = L'\\';
				break;
			}

			if (*next == L'\\')
				break;
		}

		len = next - from;

		if (len == 1 && from[0] == L'.')
			/* do nothing with singleton dot */;

		else if (len == 2 && from[0] == L'.' && from[1] == L'.') {
			if (to == base) {
				/* no more path segments to strip, eat the "../" */
				if (*next == L'\\')
					len++;

				base = to;
			} else {
				/* back up a path segment */
				while (to > base && to[-1] == L'\\') to--;
				while (to > base && to[-1] != L'\\') to--;
			}
		} else {
			if (*next == L'\\' && *from != L'\\')
				len++;

			if (to != from)
				memmove(to, from, sizeof(wchar_t) * len);

			to += len;
		}

		from += len;

		while (*from == L'\\') from++;
	}

	/* Strip trailing backslashes */
	while (to > base && to[-1] == L'\\') to--;

	*to = L'\0';

	if ((to - path) > INT_MAX) {
		SetLastError(ERROR_FILENAME_EXCED_RANGE);
		return -1;
	}

	return (int)(to - path);
}

static int git_win32_path_join(
	git_win32_path dest,
	const wchar_t *one,
	size_t one_len,
	const wchar_t *two,
	size_t two_len)
{
	size_t backslash = 0;

	if (one_len && two_len && one[one_len - 1] != L'\\')
		backslash = 1;

	if (one_len + two_len + backslash > MAX_PATH) {
		git_error_set(GIT_ERROR_INVALID, "path too long");
		return -1;
	}

	memmove(dest, one, one_len * sizeof(wchar_t));

	if (backslash)
		dest[one_len] = L'\\';

	memcpy(dest + one_len + backslash, two, two_len * sizeof(wchar_t));
	dest[one_len + backslash + two_len] = L'\0';

	return 0;
}

struct win32_path_iter {
	wchar_t *env;
	const wchar_t *current_dir;
};

static int win32_path_iter_init(struct win32_path_iter *iter)
{
	DWORD len = GetEnvironmentVariableW(L"PATH", NULL, 0);

	if (!len && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
		iter->env = NULL;
		iter->current_dir = NULL;
		return 0;
	} else if (!len) {
		git_error_set(GIT_ERROR_OS, "could not load PATH");
		return -1;
	}

	iter->env = git__malloc(len * sizeof(wchar_t));
	GIT_ERROR_CHECK_ALLOC(iter->env);

	len = GetEnvironmentVariableW(L"PATH", iter->env, len);

	if (len == 0) {
		git_error_set(GIT_ERROR_OS, "could not load PATH");
		return -1;
	}

	iter->current_dir = iter->env;
	return 0;
}

static int win32_path_iter_next(
	const wchar_t **out,
	size_t *out_len,
	struct win32_path_iter *iter)
{
	const wchar_t *start;
	wchar_t term;
	size_t len = 0;

	if (!iter->current_dir || !*iter->current_dir)
		return GIT_ITEROVER;

	term = (*iter->current_dir == L'"') ? *iter->current_dir++ : L';';
	start = iter->current_dir;

	while (*iter->current_dir && *iter->current_dir != term) {
		iter->current_dir++;
		len++;
	}

	*out = start;
	*out_len = len;

	if (term == L'"' && *iter->current_dir)
		iter->current_dir++;

	while (*iter->current_dir == L';')
		iter->current_dir++;

	return 0;
}

static void win32_path_iter_dispose(struct win32_path_iter *iter)
{
	if (!iter)
		return;

	git__free(iter->env);
	iter->env = NULL;
	iter->current_dir = NULL;
}

int git_win32_path_find_executable(git_win32_path fullpath, wchar_t *exe)
{
	struct win32_path_iter path_iter;
	const wchar_t *dir;
	size_t dir_len, exe_len = wcslen(exe);
	bool found = false;

	if (win32_path_iter_init(&path_iter) < 0)
		return -1;

	while (win32_path_iter_next(&dir, &dir_len, &path_iter) != GIT_ITEROVER) {
		if (git_win32_path_join(fullpath, dir, dir_len, exe, exe_len) < 0)
			continue;

		if (_waccess(fullpath, 0) == 0) {
			found = true;
			break;
		}
	}

	win32_path_iter_dispose(&path_iter);

	if (found)
		return 0;

	fullpath[0] = L'\0';
	return GIT_ENOTFOUND;
}

static int win32_path_cwd(wchar_t *out, size_t len)
{
	int cwd_len;

	if (len > INT_MAX) {
		errno = ENAMETOOLONG;
		return -1;
	}

	if ((cwd_len = path__cwd(out, (int)len)) < 0)
		return -1;

	/* UNC paths */
	if (wcsncmp(L"\\\\", out, 2) == 0) {
		/* Our buffer must be at least 5 characters larger than the
		 * current working directory:  we swallow one of the leading
		 * '\'s, but we we add a 'UNC' specifier to the path, plus
		 * a trailing directory separator, plus a NUL.
		 */
		if (cwd_len > GIT_WIN_PATH_MAX - 4) {
			errno = ENAMETOOLONG;
			return -1;
		}

		memmove(out+2, out, sizeof(wchar_t) * cwd_len);
		out[0] = L'U';
		out[1] = L'N';
		out[2] = L'C';

		cwd_len += 2;
	}

	/* Our buffer must be at least 2 characters larger than the current
	 * working directory.  (One character for the directory separator,
	 * one for the null.
	 */
	else if (cwd_len > GIT_WIN_PATH_MAX - 2) {
		errno = ENAMETOOLONG;
		return -1;
	}

	return cwd_len;
}

int git_win32_path_from_utf8(git_win32_path out, const char *src)
{
	wchar_t *dest = out;

	/* All win32 paths are in NT-prefixed format, beginning with "\\?\". */
	memcpy(dest, PATH__NT_NAMESPACE, sizeof(wchar_t) * PATH__NT_NAMESPACE_LEN);
	dest += PATH__NT_NAMESPACE_LEN;

	/* See if this is an absolute path (beginning with a drive letter) */
	if (git_fs_path_is_absolute(src)) {
		if (git_utf8_to_16(dest, GIT_WIN_PATH_MAX, src) < 0)
			goto on_error;
	}
	/* File-prefixed NT-style paths beginning with \\?\ */
	else if (path__is_nt_namespace(src)) {
		/* Skip the NT prefix, the destination already contains it */
		if (git_utf8_to_16(dest, GIT_WIN_PATH_MAX, src + PATH__NT_NAMESPACE_LEN) < 0)
			goto on_error;
	}
	/* UNC paths */
	else if (path__is_unc(src)) {
		memcpy(dest, L"UNC\\", sizeof(wchar_t) * 4);
		dest += 4;

		/* Skip the leading "\\" */
		if (git_utf8_to_16(dest, GIT_WIN_PATH_MAX - 2, src + 2) < 0)
			goto on_error;
	}
	/* Absolute paths omitting the drive letter */
	else if (path__startswith_slash(src)) {
		if (path__cwd(dest, GIT_WIN_PATH_MAX) < 0)
			goto on_error;

		if (!git_fs_path_is_absolute(dest)) {
			errno = ENOENT;
			goto on_error;
		}

		/* Skip the drive letter specification ("C:") */
		if (git_utf8_to_16(dest + 2, GIT_WIN_PATH_MAX - 2, src) < 0)
			goto on_error;
	}
	/* Relative paths */
	else {
		int cwd_len;

		if ((cwd_len = win32_path_cwd(dest, GIT_WIN_PATH_MAX)) < 0)
			goto on_error;

		dest[cwd_len++] = L'\\';

		if (git_utf8_to_16(dest + cwd_len, GIT_WIN_PATH_MAX - cwd_len, src) < 0)
			goto on_error;
	}

	return git_win32_path_canonicalize(out);

on_error:
	/* set windows error code so we can use its error message */
	if (errno == ENAMETOOLONG)
		SetLastError(ERROR_FILENAME_EXCED_RANGE);

	return -1;
}

int git_win32_path_relative_from_utf8(git_win32_path out, const char *src)
{
	wchar_t *dest = out, *p;
	int len;

	/* Handle absolute paths */
	if (git_fs_path_is_absolute(src) ||
	    path__is_nt_namespace(src) ||
	    path__is_unc(src) ||
	    path__startswith_slash(src)) {
		return git_win32_path_from_utf8(out, src);
	}

	if ((len = git_utf8_to_16(dest, GIT_WIN_PATH_MAX, src)) < 0)
		return -1;

	for (p = dest; p < (dest + len); p++) {
		if (*p == L'/')
			*p = L'\\';
	}

	return len;
}

int git_win32_path_to_utf8(git_win32_utf8_path dest, const wchar_t *src)
{
	char *out = dest;
	int len;

	/* Strip NT namespacing "\\?\" */
	if (path__is_nt_namespace(src)) {
		src += 4;

		/* "\\?\UNC\server\share" -> "\\server\share" */
		if (wcsncmp(src, L"UNC\\", 4) == 0) {
			src += 4;

			memcpy(dest, "\\\\", 2);
			out = dest + 2;
		}
	}

	if ((len = git_utf8_from_16(out, GIT_WIN_PATH_UTF8, src)) < 0)
		return len;

	git_fs_path_mkposix(dest);

	return len;
}

char *git_win32_path_8dot3_name(const char *path)
{
	git_win32_path longpath, shortpath;
	wchar_t *start;
	char *shortname;
	int len, namelen = 1;

	if (git_win32_path_from_utf8(longpath, path) < 0)
		return NULL;

	len = GetShortPathNameW(longpath, shortpath, GIT_WIN_PATH_UTF16);

	while (len && shortpath[len-1] == L'\\')
		shortpath[--len] = L'\0';

	if (len == 0 || len >= GIT_WIN_PATH_UTF16)
		return NULL;

	for (start = shortpath + (len - 1);
		start > shortpath && *(start-1) != '/' && *(start-1) != '\\';
		start--)
		namelen++;

	/* We may not have actually been given a short name.  But if we have,
	 * it will be in the ASCII byte range, so we don't need to worry about
	 * multi-byte sequences and can allocate naively.
	 */
	if (namelen > 12 || (shortname = git__malloc(namelen + 1)) == NULL)
		return NULL;

	if ((len = git_utf8_from_16(shortname, namelen + 1, start)) < 0)
		return NULL;

	return shortname;
}

static bool path_is_volume(wchar_t *target, size_t target_len)
{
	return (target_len && wcsncmp(target, L"\\??\\Volume{", 11) == 0);
}

/* On success, returns the length, in characters, of the path stored in dest.
 * On failure, returns a negative value. */
int git_win32_path_readlink_w(git_win32_path dest, const git_win32_path path)
{
	BYTE buf[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
	GIT_REPARSE_DATA_BUFFER *reparse_buf = (GIT_REPARSE_DATA_BUFFER *)buf;
	HANDLE handle = NULL;
	DWORD ioctl_ret;
	wchar_t *target;
	size_t target_len;

	int error = -1;

	handle = CreateFileW(path, GENERIC_READ,
		FILE_SHARE_READ | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
		FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);

	if (handle == INVALID_HANDLE_VALUE) {
		errno = ENOENT;
		return -1;
	}

	if (!DeviceIoControl(handle, FSCTL_GET_REPARSE_POINT, NULL, 0,
		reparse_buf, sizeof(buf), &ioctl_ret, NULL)) {
		errno = EINVAL;
		goto on_error;
	}

	switch (reparse_buf->ReparseTag) {
	case IO_REPARSE_TAG_SYMLINK:
		target = reparse_buf->ReparseBuffer.SymbolicLink.PathBuffer +
			(reparse_buf->ReparseBuffer.SymbolicLink.SubstituteNameOffset / sizeof(WCHAR));
		target_len = reparse_buf->ReparseBuffer.SymbolicLink.SubstituteNameLength / sizeof(WCHAR);
	break;
	case IO_REPARSE_TAG_MOUNT_POINT:
		target = reparse_buf->ReparseBuffer.MountPoint.PathBuffer +
			(reparse_buf->ReparseBuffer.MountPoint.SubstituteNameOffset / sizeof(WCHAR));
		target_len = reparse_buf->ReparseBuffer.MountPoint.SubstituteNameLength / sizeof(WCHAR);
	break;
	default:
		errno = EINVAL;
		goto on_error;
	}

	if (path_is_volume(target, target_len)) {
		/* This path is a reparse point that represents another volume mounted
		 * at this location, it is not a symbolic link our input was canonical.
		 */
		errno = EINVAL;
		error = -1;
	} else if (target_len) {
		/* The path may need to have a namespace prefix removed. */
		target_len = git_win32_path_remove_namespace(target, target_len);

		/* Need one additional character in the target buffer
		 * for the terminating NULL. */
		if (GIT_WIN_PATH_UTF16 > target_len) {
			wcscpy(dest, target);
			error = (int)target_len;
		}
	}

on_error:
	CloseHandle(handle);
	return error;
}

/**
 * Removes any trailing backslashes from a path, except in the case of a drive
 * letter path (C:\, D:\, etc.). This function cannot fail.
 *
 * @param path The path which should be trimmed.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32_path_trim_end(wchar_t *str, size_t len)
{
	while (1) {
		if (!len || str[len - 1] != L'\\')
			break;

		/*
		 * Don't trim backslashes from drive letter paths, which
		 * are 3 characters long and of the form C:\, D:\, etc.
		 */
		if (len == 3 && git_win32__isalpha(str[0]) && str[1] == ':')
			break;

		len--;
	}

	str[len] = L'\0';

	return len;
}

/**
 * Removes any of the following namespace prefixes from a path,
 * if found: "\??\", "\\?\", "\\?\UNC\". This function cannot fail.
 *
 * @param path The path which should be converted.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32_path_remove_namespace(wchar_t *str, size_t len)
{
	static const wchar_t dosdevices_namespace[] = L"\\\?\?\\";
	static const wchar_t nt_namespace[] = L"\\\\?\\";
	static const wchar_t unc_namespace_remainder[] = L"UNC\\";
	static const wchar_t unc_prefix[] = L"\\\\";

	const wchar_t *prefix = NULL, *remainder = NULL;
	size_t prefix_len = 0, remainder_len = 0;

	/* "\??\" -- DOS Devices prefix */
	if (len >= CONST_STRLEN(dosdevices_namespace) &&
		!wcsncmp(str, dosdevices_namespace, CONST_STRLEN(dosdevices_namespace))) {
		remainder = str + CONST_STRLEN(dosdevices_namespace);
		remainder_len = len - CONST_STRLEN(dosdevices_namespace);
	}
	/* "\\?\" -- NT namespace prefix */
	else if (len >= CONST_STRLEN(nt_namespace) &&
		!wcsncmp(str, nt_namespace, CONST_STRLEN(nt_namespace))) {
		remainder = str + CONST_STRLEN(nt_namespace);
		remainder_len = len - CONST_STRLEN(nt_namespace);
	}

	/* "\??\UNC\", "\\?\UNC\" -- UNC prefix */
	if (remainder_len >= CONST_STRLEN(unc_namespace_remainder) &&
		!wcsncmp(remainder, unc_namespace_remainder, CONST_STRLEN(unc_namespace_remainder))) {

		/*
		 * The proper Win32 path for a UNC share has "\\" at beginning of it
		 * and looks like "\\server\share\<folderStructure>".  So remove the
		 * UNC namespace and add a prefix of "\\" in its place.
		 */
		remainder += CONST_STRLEN(unc_namespace_remainder);
		remainder_len -= CONST_STRLEN(unc_namespace_remainder);

		prefix = unc_prefix;
		prefix_len = CONST_STRLEN(unc_prefix);
	}

	/*
	 * Sanity check that the new string isn't longer than the old one.
	 * (This could only happen due to programmer error introducing a
	 * prefix longer than the namespace it replaces.)
	 */
	if (remainder && len >= remainder_len + prefix_len) {
		if (prefix)
			memmove(str, prefix, prefix_len * sizeof(wchar_t));

		memmove(str + prefix_len, remainder, remainder_len * sizeof(wchar_t));

		len = remainder_len + prefix_len;
		str[len] = L'\0';
	}

	return git_win32_path_trim_end(str, len);
}
