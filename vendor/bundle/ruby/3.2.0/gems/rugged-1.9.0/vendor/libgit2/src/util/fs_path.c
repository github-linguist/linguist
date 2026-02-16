/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "fs_path.h"

#include "git2_util.h"
#include "futils.h"
#include "posix.h"
#ifdef GIT_WIN32
#include "win32/posix.h"
#include "win32/w32_buffer.h"
#include "win32/w32_util.h"
#include "win32/version.h"
#include <aclapi.h>
#else
#include <dirent.h>
#endif
#include <stdio.h>
#include <ctype.h>

#define ensure_error_set(code) do { \
		const git_error *e = git_error_last(); \
		if (!e || !e->message) \
			git_error_set(e ? e->klass : GIT_ERROR_CALLBACK, \
				"filesystem callback returned %d", code); \
	} while(0)

static int dos_drive_prefix_length(const char *path)
{
	int i;

	/*
	 * Does it start with an ASCII letter (i.e. highest bit not set),
	 * followed by a colon?
	 */
	if (!(0x80 & (unsigned char)*path))
		return *path && path[1] == ':' ? 2 : 0;

	/*
	 * While drive letters must be letters of the English alphabet, it is
	 * possible to assign virtually _any_ Unicode character via `subst` as
	 * a drive letter to "virtual drives". Even `1`, or `ä`. Or fun stuff
	 * like this:
	 *
	 *	subst ֍: %USERPROFILE%\Desktop
	 */
	for (i = 1; i < 4 && (0x80 & (unsigned char)path[i]); i++)
		; /* skip first UTF-8 character */
	return path[i] == ':' ? i + 1 : 0;
}

#ifdef GIT_WIN32
static bool looks_like_network_computer_name(const char *path, int pos)
{
	if (pos < 3)
		return false;

	if (path[0] != '/' || path[1] != '/')
		return false;

	while (pos-- > 2) {
		if (path[pos] == '/')
			return false;
	}

	return true;
}
#endif

/*
 * Based on the Android implementation, BSD licensed.
 * http://android.git.kernel.org/
 *
 * Copyright (C) 2008 The Android Open Source Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
int git_fs_path_basename_r(git_str *buffer, const char *path)
{
	const char *endp, *startp;
	int len, result;

	/* Empty or NULL string gets treated as "." */
	if (path == NULL || *path == '\0') {
		startp = ".";
		len = 1;
		goto Exit;
	}

	/* Strip trailing slashes */
	endp = path + strlen(path) - 1;
	while (endp > path && *endp == '/')
		endp--;

	/* All slashes becomes "/" */
	if (endp == path && *endp == '/') {
		startp = "/";
		len = 1;
		goto Exit;
	}

	/* Find the start of the base */
	startp = endp;
	while (startp > path && *(startp - 1) != '/')
		startp--;

	/* Cast is safe because max path < max int */
	len = (int)(endp - startp + 1);

Exit:
	result = len;

	if (buffer != NULL && git_str_set(buffer, startp, len) < 0)
		return -1;

	return result;
}

/*
 * Determine if the path is a Windows prefix and, if so, returns
 * its actual length. If it is not a prefix, returns -1.
 */
static int win32_prefix_length(const char *path, int len)
{
#ifndef GIT_WIN32
	GIT_UNUSED(path);
	GIT_UNUSED(len);
#else
	/*
	 * Mimic unix behavior where '/.git' returns '/': 'C:/.git'
	 * will return 'C:/' here
	 */
	if (dos_drive_prefix_length(path) == len)
		return len;

	/*
	 * Similarly checks if we're dealing with a network computer name
	 * '//computername/.git' will return '//computername/'
	 */
	if (looks_like_network_computer_name(path, len))
		return len;
#endif

	return -1;
}

/*
 * Based on the Android implementation, BSD licensed.
 * Check http://android.git.kernel.org/
 */
int git_fs_path_dirname_r(git_str *buffer, const char *path)
{
	const char *endp;
	int is_prefix = 0, len;

	/* Empty or NULL string gets treated as "." */
	if (path == NULL || *path == '\0') {
		path = ".";
		len = 1;
		goto Exit;
	}

	/* Strip trailing slashes */
	endp = path + strlen(path) - 1;
	while (endp > path && *endp == '/')
		endp--;

	if (endp - path + 1 > INT_MAX) {
		git_error_set(GIT_ERROR_INVALID, "path too long");
		return -1;
	}

	if ((len = win32_prefix_length(path, (int)(endp - path + 1))) > 0) {
		is_prefix = 1;
		goto Exit;
	}

	/* Find the start of the dir */
	while (endp > path && *endp != '/')
		endp--;

	/* Either the dir is "/" or there are no slashes */
	if (endp == path) {
		path = (*endp == '/') ? "/" : ".";
		len = 1;
		goto Exit;
	}

	do {
		endp--;
	} while (endp > path && *endp == '/');

	if (endp - path + 1 > INT_MAX) {
		git_error_set(GIT_ERROR_INVALID, "path too long");
		return -1;
	}

	if ((len = win32_prefix_length(path, (int)(endp - path + 1))) > 0) {
		is_prefix = 1;
		goto Exit;
	}

	/* Cast is safe because max path < max int */
	len = (int)(endp - path + 1);

Exit:
	if (buffer) {
		if (git_str_set(buffer, path, len) < 0)
			return -1;
		if (is_prefix && git_str_putc(buffer, '/') < 0)
			return -1;
	}

	return len;
}


char *git_fs_path_dirname(const char *path)
{
	git_str buf = GIT_STR_INIT;
	char *dirname;

	git_fs_path_dirname_r(&buf, path);
	dirname = git_str_detach(&buf);
	git_str_dispose(&buf); /* avoid memleak if error occurs */

	return dirname;
}

char *git_fs_path_basename(const char *path)
{
	git_str buf = GIT_STR_INIT;
	char *basename;

	git_fs_path_basename_r(&buf, path);
	basename = git_str_detach(&buf);
	git_str_dispose(&buf); /* avoid memleak if error occurs */

	return basename;
}

size_t git_fs_path_basename_offset(git_str *buffer)
{
	ssize_t slash;

	if (!buffer || buffer->size <= 0)
		return 0;

	slash = git_str_rfind_next(buffer, '/');

	if (slash >= 0 && buffer->ptr[slash] == '/')
		return (size_t)(slash + 1);

	return 0;
}

int git_fs_path_root(const char *path)
{
	int offset = 0, prefix_len;

	/* Does the root of the path look like a windows drive ? */
	if ((prefix_len = dos_drive_prefix_length(path)))
		offset += prefix_len;

#ifdef GIT_WIN32
	/* Are we dealing with a windows network path? */
	else if ((path[0] == '/' && path[1] == '/' && path[2] != '/') ||
		(path[0] == '\\' && path[1] == '\\' && path[2] != '\\'))
	{
		offset += 2;

		/* Skip the computer name segment */
		while (path[offset] && path[offset] != '/' && path[offset] != '\\')
			offset++;
	}

	if (path[offset] == '\\')
		return offset;
#endif

	if (path[offset] == '/')
		return offset;

	return -1;	/* Not a real error - signals that path is not rooted */
}

static void path_trim_slashes(git_str *path)
{
	int ceiling = git_fs_path_root(path->ptr) + 1;

	if (ceiling < 0)
		return;

	while (path->size > (size_t)ceiling) {
		if (path->ptr[path->size-1] != '/')
			break;

		path->ptr[path->size-1] = '\0';
		path->size--;
	}
}

int git_fs_path_join_unrooted(
	git_str *path_out, const char *path, const char *base, ssize_t *root_at)
{
	ssize_t root;

	GIT_ASSERT_ARG(path_out);
	GIT_ASSERT_ARG(path);

	root = (ssize_t)git_fs_path_root(path);

	if (base != NULL && root < 0) {
		if (git_str_joinpath(path_out, base, path) < 0)
			return -1;

		root = (ssize_t)strlen(base);
	} else {
		if (git_str_sets(path_out, path) < 0)
			return -1;

		if (root < 0)
			root = 0;
		else if (base)
			git_fs_path_equal_or_prefixed(base, path, &root);
	}

	if (root_at)
		*root_at = root;

	return 0;
}

void git_fs_path_squash_slashes(git_str *path)
{
	char *p, *q;

	if (path->size == 0)
		return;

	for (p = path->ptr, q = path->ptr; *q; p++, q++) {
		*p = *q;

		while (*q == '/' && *(q+1) == '/') {
			path->size--;
			q++;
		}
	}

	*p = '\0';
}

int git_fs_path_prettify(git_str *path_out, const char *path, const char *base)
{
	char buf[GIT_PATH_MAX];

	GIT_ASSERT_ARG(path_out);
	GIT_ASSERT_ARG(path);

	/* construct path if needed */
	if (base != NULL && git_fs_path_root(path) < 0) {
		if (git_str_joinpath(path_out, base, path) < 0)
			return -1;
		path = path_out->ptr;
	}

	if (p_realpath(path, buf) == NULL) {
		/* git_error_set resets the errno when dealing with a GIT_ERROR_OS kind of error */
		int error = (errno == ENOENT || errno == ENOTDIR) ? GIT_ENOTFOUND : -1;
		git_error_set(GIT_ERROR_OS, "failed to resolve path '%s'", path);

		git_str_clear(path_out);

		return error;
	}

	return git_str_sets(path_out, buf);
}

int git_fs_path_prettify_dir(git_str *path_out, const char *path, const char *base)
{
	int error = git_fs_path_prettify(path_out, path, base);
	return (error < 0) ? error : git_fs_path_to_dir(path_out);
}

int git_fs_path_to_dir(git_str *path)
{
	if (path->asize > 0 &&
		git_str_len(path) > 0 &&
		path->ptr[git_str_len(path) - 1] != '/')
		git_str_putc(path, '/');

	return git_str_oom(path) ? -1 : 0;
}

size_t git_fs_path_dirlen(const char *path)
{
	size_t len = strlen(path);

	while (len > 1 && path[len - 1] == '/')
		len--;

	return len;
}

void git_fs_path_string_to_dir(char *path, size_t size)
{
	size_t end = strlen(path);

	if (end && path[end - 1] != '/' && end < size) {
		path[end] = '/';
		path[end + 1] = '\0';
	}
}

int git__percent_decode(git_str *decoded_out, const char *input)
{
	int len, hi, lo, i;

	GIT_ASSERT_ARG(decoded_out);
	GIT_ASSERT_ARG(input);

	len = (int)strlen(input);
	git_str_clear(decoded_out);

	for(i = 0; i < len; i++)
	{
		char c = input[i];

		if (c != '%')
			goto append;

		if (i >= len - 2)
			goto append;

		hi = git__fromhex(input[i + 1]);
		lo = git__fromhex(input[i + 2]);

		if (hi < 0 || lo < 0)
			goto append;

		c = (char)(hi << 4 | lo);
		i += 2;

append:
		if (git_str_putc(decoded_out, c) < 0)
			return -1;
	}

	return 0;
}

static int error_invalid_local_file_uri(const char *uri)
{
	git_error_set(GIT_ERROR_CONFIG, "'%s' is not a valid local file URI", uri);
	return -1;
}

static int local_file_url_prefixlen(const char *file_url)
{
	int len = -1;

	if (git__prefixcmp(file_url, "file://") == 0) {
		if (file_url[7] == '/')
			len = 8;
		else if (git__prefixcmp(file_url + 7, "localhost/") == 0)
			len = 17;
	}

	return len;
}

bool git_fs_path_is_local_file_url(const char *file_url)
{
	return (local_file_url_prefixlen(file_url) > 0);
}

int git_fs_path_fromurl(git_str *local_path_out, const char *file_url)
{
	int offset;

	GIT_ASSERT_ARG(local_path_out);
	GIT_ASSERT_ARG(file_url);

	if ((offset = local_file_url_prefixlen(file_url)) < 0 ||
		file_url[offset] == '\0' || file_url[offset] == '/')
		return error_invalid_local_file_uri(file_url);

#ifndef GIT_WIN32
	offset--;	/* A *nix absolute path starts with a forward slash */
#endif

	git_str_clear(local_path_out);
	return git__percent_decode(local_path_out, file_url + offset);
}

int git_fs_path_walk_up(
	git_str *path,
	const char *ceiling,
	int (*cb)(void *data, const char *),
	void *data)
{
	int error = 0;
	git_str iter;
	ssize_t stop = 0, scan;
	char oldc = '\0';

	GIT_ASSERT_ARG(path);
	GIT_ASSERT_ARG(cb);

	if (ceiling != NULL) {
		if (git__prefixcmp(path->ptr, ceiling) == 0)
			stop = (ssize_t)strlen(ceiling);
		else
			stop = git_str_len(path);
	}
	scan = git_str_len(path);

	/* empty path: yield only once */
	if (!scan) {
		error = cb(data, "");
		if (error)
			ensure_error_set(error);
		return error;
	}

	iter.ptr = path->ptr;
	iter.size = git_str_len(path);
	iter.asize = path->asize;

	while (scan >= stop) {
		error = cb(data, iter.ptr);
		iter.ptr[scan] = oldc;

		if (error) {
			ensure_error_set(error);
			break;
		}

		scan = git_str_rfind_next(&iter, '/');
		if (scan >= 0) {
			scan++;
			oldc = iter.ptr[scan];
			iter.size = scan;
			iter.ptr[scan] = '\0';
		}
	}

	if (scan >= 0)
		iter.ptr[scan] = oldc;

	/* relative path: yield for the last component */
	if (!error && stop == 0 && iter.ptr[0] != '/') {
		error = cb(data, "");
		if (error)
			ensure_error_set(error);
	}

	return error;
}

bool git_fs_path_exists(const char *path)
{
	GIT_ASSERT_ARG_WITH_RETVAL(path, false);
	return p_access(path, F_OK) == 0;
}

bool git_fs_path_isdir(const char *path)
{
	struct stat st;
	if (p_stat(path, &st) < 0)
		return false;

	return S_ISDIR(st.st_mode) != 0;
}

bool git_fs_path_isfile(const char *path)
{
	struct stat st;

	GIT_ASSERT_ARG_WITH_RETVAL(path, false);
	if (p_stat(path, &st) < 0)
		return false;

	return S_ISREG(st.st_mode) != 0;
}

bool git_fs_path_islink(const char *path)
{
	struct stat st;

	GIT_ASSERT_ARG_WITH_RETVAL(path, false);
	if (p_lstat(path, &st) < 0)
		return false;

	return S_ISLNK(st.st_mode) != 0;
}

#ifdef GIT_WIN32

bool git_fs_path_is_empty_dir(const char *path)
{
	git_win32_path filter_w;
	bool empty = false;

	if (git_win32__findfirstfile_filter(filter_w, path)) {
		WIN32_FIND_DATAW findData;
		HANDLE hFind = FindFirstFileW(filter_w, &findData);

		/* FindFirstFile will fail if there are no children to the given
		 * path, which can happen if the given path is a file (and obviously
		 * has no children) or if the given path is an empty mount point.
		 * (Most directories have at least directory entries '.' and '..',
		 * but ridiculously another volume mounted in another drive letter's
		 * path space do not, and thus have nothing to enumerate.)  If
		 * FindFirstFile fails, check if this is a directory-like thing
		 * (a mount point).
		 */
		if (hFind == INVALID_HANDLE_VALUE)
			return git_fs_path_isdir(path);

		/* If the find handle was created successfully, then it's a directory */
		empty = true;

		do {
			/* Allow the enumeration to return . and .. and still be considered
			 * empty. In the special case of drive roots (i.e. C:\) where . and
			 * .. do not occur, we can still consider the path to be an empty
			 * directory if there's nothing there. */
			if (!git_fs_path_is_dot_or_dotdotW(findData.cFileName)) {
				empty = false;
				break;
			}
		} while (FindNextFileW(hFind, &findData));

		FindClose(hFind);
	}

	return empty;
}

#else

static int path_found_entry(void *payload, git_str *path)
{
	GIT_UNUSED(payload);
	return !git_fs_path_is_dot_or_dotdot(path->ptr);
}

bool git_fs_path_is_empty_dir(const char *path)
{
	int error;
	git_str dir = GIT_STR_INIT;

	if (!git_fs_path_isdir(path))
		return false;

	if ((error = git_str_sets(&dir, path)) != 0)
		git_error_clear();
	else
		error = git_fs_path_direach(&dir, 0, path_found_entry, NULL);

	git_str_dispose(&dir);

	return !error;
}

#endif

int git_fs_path_set_error(int errno_value, const char *path, const char *action)
{
	switch (errno_value) {
	case ENOENT:
	case ENOTDIR:
		git_error_set(GIT_ERROR_OS, "could not find '%s' to %s", path, action);
		return GIT_ENOTFOUND;

	case EINVAL:
	case ENAMETOOLONG:
		git_error_set(GIT_ERROR_OS, "invalid path for filesystem '%s'", path);
		return GIT_EINVALIDSPEC;

	case EEXIST:
		git_error_set(GIT_ERROR_OS, "failed %s - '%s' already exists", action, path);
		return GIT_EEXISTS;

	case EACCES:
		git_error_set(GIT_ERROR_OS, "failed %s - '%s' is locked", action, path);
		return GIT_ELOCKED;

	default:
		git_error_set(GIT_ERROR_OS, "could not %s '%s'", action, path);
		return -1;
	}
}

int git_fs_path_lstat(const char *path, struct stat *st)
{
	if (p_lstat(path, st) == 0)
		return 0;

	return git_fs_path_set_error(errno, path, "stat");
}

static bool _check_dir_contents(
	git_str *dir,
	const char *sub,
	bool (*predicate)(const char *))
{
	bool result;
	size_t dir_size = git_str_len(dir);
	size_t sub_size = strlen(sub);
	size_t alloc_size;

	/* leave base valid even if we could not make space for subdir */
	if (GIT_ADD_SIZET_OVERFLOW(&alloc_size, dir_size, sub_size) ||
		GIT_ADD_SIZET_OVERFLOW(&alloc_size, alloc_size, 2) ||
		git_str_try_grow(dir, alloc_size, false) < 0)
		return false;

	/* save excursion */
	if (git_str_joinpath(dir, dir->ptr, sub) < 0)
		return false;

	result = predicate(dir->ptr);

	/* restore path */
	git_str_truncate(dir, dir_size);
	return result;
}

bool git_fs_path_contains(git_str *dir, const char *item)
{
	return _check_dir_contents(dir, item, &git_fs_path_exists);
}

bool git_fs_path_contains_dir(git_str *base, const char *subdir)
{
	return _check_dir_contents(base, subdir, &git_fs_path_isdir);
}

bool git_fs_path_contains_file(git_str *base, const char *file)
{
	return _check_dir_contents(base, file, &git_fs_path_isfile);
}

int git_fs_path_find_dir(git_str *dir)
{
	int error = 0;
	char buf[GIT_PATH_MAX];

	if (p_realpath(dir->ptr, buf) != NULL)
		error = git_str_sets(dir, buf);

	/* call dirname if this is not a directory */
	if (!error) /* && git_fs_path_isdir(dir->ptr) == false) */
		error = (git_fs_path_dirname_r(dir, dir->ptr) < 0) ? -1 : 0;

	if (!error)
		error = git_fs_path_to_dir(dir);

	return error;
}

int git_fs_path_resolve_relative(git_str *path, size_t ceiling)
{
	char *base, *to, *from, *next;
	size_t len;

	GIT_ERROR_CHECK_ALLOC_STR(path);

	if (ceiling > path->size)
		ceiling = path->size;

	/* recognize drive prefixes, etc. that should not be backed over */
	if (ceiling == 0)
		ceiling = git_fs_path_root(path->ptr) + 1;

	/* recognize URL prefixes that should not be backed over */
	if (ceiling == 0) {
		for (next = path->ptr; *next && git__isalpha(*next); ++next);
		if (next[0] == ':' && next[1] == '/' && next[2] == '/')
			ceiling = (next + 3) - path->ptr;
	}

	base = to = from = path->ptr + ceiling;

	while (*from) {
		for (next = from; *next && *next != '/'; ++next);

		len = next - from;

		if (len == 1 && from[0] == '.')
			/* do nothing with singleton dot */;

		else if (len == 2 && from[0] == '.' && from[1] == '.') {
			/* error out if trying to up one from a hard base */
			if (to == base && ceiling != 0) {
				git_error_set(GIT_ERROR_INVALID,
					"cannot strip root component off url");
				return -1;
			}

			/* no more path segments to strip,
			 * use '../' as a new base path */
			if (to == base) {
				if (*next == '/')
					len++;

				if (to != from)
					memmove(to, from, len);

				to += len;
				/* this is now the base, can't back up from a
				 * relative prefix */
				base = to;
			} else {
				/* back up a path segment */
				while (to > base && to[-1] == '/') to--;
				while (to > base && to[-1] != '/') to--;
			}
		} else {
			if (*next == '/' && *from != '/')
				len++;

			if (to != from)
				memmove(to, from, len);

			to += len;
		}

		from += len;

		while (*from == '/') from++;
	}

	*to = '\0';

	path->size = to - path->ptr;

	return 0;
}

int git_fs_path_apply_relative(git_str *target, const char *relpath)
{
	return git_str_joinpath(target, git_str_cstr(target), relpath) ||
	    git_fs_path_resolve_relative(target, 0);
}

int git_fs_path_cmp(
	const char *name1, size_t len1, int isdir1,
	const char *name2, size_t len2, int isdir2,
	int (*compare)(const char *, const char *, size_t))
{
	unsigned char c1, c2;
	size_t len = len1 < len2 ? len1 : len2;
	int cmp;

	cmp = compare(name1, name2, len);
	if (cmp)
		return cmp;

	c1 = name1[len];
	c2 = name2[len];

	if (c1 == '\0' && isdir1)
		c1 = '/';

	if (c2 == '\0' && isdir2)
		c2 = '/';

	return (c1 < c2) ? -1 : (c1 > c2) ? 1 : 0;
}

size_t git_fs_path_common_dirlen(const char *one, const char *two)
{
	const char *p, *q, *dirsep = NULL;

	for (p = one, q = two; *p && *q; p++, q++) {
		if (*p == '/' && *q == '/')
			dirsep = p;
		else if (*p != *q)
			break;
	}

	return dirsep ? (dirsep - one) + 1 : 0;
}

int git_fs_path_make_relative(git_str *path, const char *parent)
{
	const char *p, *q, *p_dirsep, *q_dirsep;
	size_t plen = path->size, newlen, alloclen, depth = 1, i, offset;

	for (p_dirsep = p = path->ptr, q_dirsep = q = parent; *p && *q; p++, q++) {
		if (*p == '/' && *q == '/') {
			p_dirsep = p;
			q_dirsep = q;
		}
		else if (*p != *q)
			break;
	}

	/* need at least 1 common path segment */
	if ((p_dirsep == path->ptr || q_dirsep == parent) &&
		(*p_dirsep != '/' || *q_dirsep != '/')) {
		git_error_set(GIT_ERROR_INVALID,
			"%s is not a parent of %s", parent, path->ptr);
		return GIT_ENOTFOUND;
	}

	if (*p == '/' && !*q)
		p++;
	else if (!*p && *q == '/')
		q++;
	else if (!*p && !*q)
		return git_str_clear(path), 0;
	else {
		p = p_dirsep + 1;
		q = q_dirsep + 1;
	}

	plen -= (p - path->ptr);

	if (!*q)
		return git_str_set(path, p, plen);

	for (; (q = strchr(q, '/')) && *(q + 1); q++)
		depth++;

	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&newlen, depth, 3);
	GIT_ERROR_CHECK_ALLOC_ADD(&newlen, newlen, plen);

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, newlen, 1);

	/* save the offset as we might realllocate the pointer */
	offset = p - path->ptr;
	if (git_str_try_grow(path, alloclen, 1) < 0)
		return -1;
	p = path->ptr + offset;

	memmove(path->ptr + (depth * 3), p, plen + 1);

	for (i = 0; i < depth; i++)
		memcpy(path->ptr + (i * 3), "../", 3);

	path->size = newlen;
	return 0;
}

bool git_fs_path_has_non_ascii(const char *path, size_t pathlen)
{
	const uint8_t *scan = (const uint8_t *)path, *end;

	for (end = scan + pathlen; scan < end; ++scan)
		if (*scan & 0x80)
			return true;

	return false;
}

#ifdef GIT_USE_ICONV

int git_fs_path_iconv_init_precompose(git_fs_path_iconv_t *ic)
{
	git_str_init(&ic->buf, 0);
	ic->map = iconv_open(GIT_PATH_REPO_ENCODING, GIT_PATH_NATIVE_ENCODING);
	return 0;
}

void git_fs_path_iconv_clear(git_fs_path_iconv_t *ic)
{
	if (ic) {
		if (ic->map != (iconv_t)-1)
			iconv_close(ic->map);
		git_str_dispose(&ic->buf);
	}
}

int git_fs_path_iconv(git_fs_path_iconv_t *ic, const char **in, size_t *inlen)
{
	char *nfd = (char*)*in, *nfc;
	size_t nfdlen = *inlen, nfclen, wantlen = nfdlen, alloclen, rv;
	int retry = 1;

	if (!ic || ic->map == (iconv_t)-1 ||
		!git_fs_path_has_non_ascii(*in, *inlen))
		return 0;

	git_str_clear(&ic->buf);

	while (1) {
		GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, wantlen, 1);
		if (git_str_grow(&ic->buf, alloclen) < 0)
			return -1;

		nfc    = ic->buf.ptr   + ic->buf.size;
		nfclen = ic->buf.asize - ic->buf.size;

		rv = iconv(ic->map, &nfd, &nfdlen, &nfc, &nfclen);

		ic->buf.size = (nfc - ic->buf.ptr);

		if (rv != (size_t)-1)
			break;

		/* if we cannot convert the data (probably because iconv thinks
		 * it is not valid UTF-8 source data), then use original data
		 */
		if (errno != E2BIG)
			return 0;

		/* make space for 2x the remaining data to be converted
		 * (with per retry overhead to avoid infinite loops)
		 */
		wantlen = ic->buf.size + max(nfclen, nfdlen) * 2 + (size_t)(retry * 4);

		if (retry++ > 4)
			goto fail;
	}

	ic->buf.ptr[ic->buf.size] = '\0';

	*in    = ic->buf.ptr;
	*inlen = ic->buf.size;

	return 0;

fail:
	git_error_set(GIT_ERROR_OS, "unable to convert unicode path data");
	return -1;
}

static const char *nfc_file = "\xC3\x85\x73\x74\x72\xC3\xB6\x6D";
static const char *nfd_file = "\x41\xCC\x8A\x73\x74\x72\x6F\xCC\x88\x6D";

/* Check if the platform is decomposing unicode data for us.  We will
 * emulate core Git and prefer to use precomposed unicode data internally
 * on these platforms, composing the decomposed unicode on the fly.
 *
 * This mainly happens on the Mac where HDFS stores filenames as
 * decomposed unicode.  Even on VFAT and SAMBA file systems, the Mac will
 * return decomposed unicode from readdir() even when the actual
 * filesystem is storing precomposed unicode.
 */
bool git_fs_path_does_decompose_unicode(const char *root)
{
	git_str nfc_path = GIT_STR_INIT;
	git_str nfd_path = GIT_STR_INIT;
	int fd;
	bool found_decomposed = false;
	size_t orig_len;
	const char *trailer;

	/* Create a file using a precomposed path and then try to find it
	 * using the decomposed name.  If the lookup fails, then we will mark
	 * that we should precompose unicode for this repository.
	 */
	if (git_str_joinpath(&nfc_path, root, nfc_file) < 0)
		goto done;

	/* record original path length before trailer */
	orig_len = nfc_path.size;

	if ((fd = git_futils_mktmp(&nfc_path, nfc_path.ptr, 0666)) < 0)
		goto done;
	p_close(fd);

	trailer = nfc_path.ptr + orig_len;

	/* try to look up as NFD path */
	if (git_str_joinpath(&nfd_path, root, nfd_file) < 0 ||
	    git_str_puts(&nfd_path, trailer) < 0)
		goto done;

	found_decomposed = git_fs_path_exists(nfd_path.ptr);

	/* remove temporary file (using original precomposed path) */
	(void)p_unlink(nfc_path.ptr);

done:
	git_str_dispose(&nfc_path);
	git_str_dispose(&nfd_path);
	return found_decomposed;
}

#else

bool git_fs_path_does_decompose_unicode(const char *root)
{
	GIT_UNUSED(root);
	return false;
}

#endif

#if defined(__sun) || defined(__GNU__)
typedef char path_dirent_data[sizeof(struct dirent) + FILENAME_MAX + 1];
#else
typedef struct dirent path_dirent_data;
#endif

int git_fs_path_direach(
	git_str *path,
	uint32_t flags,
	int (*fn)(void *, git_str *),
	void *arg)
{
	int error = 0;
	ssize_t wd_len;
	DIR *dir;
	struct dirent *de;

#ifdef GIT_USE_ICONV
	git_fs_path_iconv_t ic = GIT_PATH_ICONV_INIT;
#endif

	GIT_UNUSED(flags);

	if (git_fs_path_to_dir(path) < 0)
		return -1;

	wd_len = git_str_len(path);

	if ((dir = opendir(path->ptr)) == NULL) {
		git_error_set(GIT_ERROR_OS, "failed to open directory '%s'", path->ptr);
		if (errno == ENOENT)
			return GIT_ENOTFOUND;

		return -1;
	}

#ifdef GIT_USE_ICONV
	if ((flags & GIT_FS_PATH_DIR_PRECOMPOSE_UNICODE) != 0)
		(void)git_fs_path_iconv_init_precompose(&ic);
#endif

	while ((de = readdir(dir)) != NULL) {
		const char *de_path = de->d_name;
		size_t de_len = strlen(de_path);

		if (git_fs_path_is_dot_or_dotdot(de_path))
			continue;

#ifdef GIT_USE_ICONV
		if ((error = git_fs_path_iconv(&ic, &de_path, &de_len)) < 0)
			break;
#endif

		if ((error = git_str_put(path, de_path, de_len)) < 0)
			break;

		git_error_clear();
		error = fn(arg, path);

		git_str_truncate(path, wd_len); /* restore path */

		/* Only set our own error if the callback did not set one already */
		if (error != 0) {
			if (!git_error_last())
				ensure_error_set(error);

			break;
		}
	}

	closedir(dir);

#ifdef GIT_USE_ICONV
	git_fs_path_iconv_clear(&ic);
#endif

	return error;
}

#if defined(GIT_WIN32) && !defined(__MINGW32__)

/* Using _FIND_FIRST_EX_LARGE_FETCH may increase performance in Windows 7
 * and better.
 */
#ifndef FIND_FIRST_EX_LARGE_FETCH
# define FIND_FIRST_EX_LARGE_FETCH 2
#endif

int git_fs_path_diriter_init(
	git_fs_path_diriter *diriter,
	const char *path,
	unsigned int flags)
{
	git_win32_path path_filter;

	static int is_win7_or_later = -1;
	if (is_win7_or_later < 0)
		is_win7_or_later = git_has_win32_version(6, 1, 0);

	GIT_ASSERT_ARG(diriter);
	GIT_ASSERT_ARG(path);

	memset(diriter, 0, sizeof(git_fs_path_diriter));
	diriter->handle = INVALID_HANDLE_VALUE;

	if (git_str_puts(&diriter->path_utf8, path) < 0)
		return -1;

	path_trim_slashes(&diriter->path_utf8);

	if (diriter->path_utf8.size == 0) {
		git_error_set(GIT_ERROR_FILESYSTEM, "could not open directory '%s'", path);
		return -1;
	}

	if ((diriter->parent_len = git_win32_path_from_utf8(diriter->path, diriter->path_utf8.ptr)) < 0 ||
			!git_win32__findfirstfile_filter(path_filter, diriter->path_utf8.ptr)) {
		git_error_set(GIT_ERROR_OS, "could not parse the directory path '%s'", path);
		return -1;
	}

	diriter->handle = FindFirstFileExW(
		path_filter,
		is_win7_or_later ? FindExInfoBasic : FindExInfoStandard,
		&diriter->current,
		FindExSearchNameMatch,
		NULL,
		is_win7_or_later ? FIND_FIRST_EX_LARGE_FETCH : 0);

	if (diriter->handle == INVALID_HANDLE_VALUE) {
		git_error_set(GIT_ERROR_OS, "could not open directory '%s'", path);
		return -1;
	}

	diriter->parent_utf8_len = diriter->path_utf8.size;
	diriter->flags = flags;
	return 0;
}

static int diriter_update_paths(git_fs_path_diriter *diriter)
{
	size_t filename_len, path_len;

	filename_len = wcslen(diriter->current.cFileName);

	if (GIT_ADD_SIZET_OVERFLOW(&path_len, diriter->parent_len, filename_len) ||
		GIT_ADD_SIZET_OVERFLOW(&path_len, path_len, 2))
		return -1;

	if (path_len > GIT_WIN_PATH_UTF16) {
		git_error_set(GIT_ERROR_FILESYSTEM,
			"invalid path '%.*ls\\%ls' (path too long)",
			diriter->parent_len, diriter->path, diriter->current.cFileName);
		return -1;
	}

	diriter->path[diriter->parent_len] = L'\\';
	memcpy(&diriter->path[diriter->parent_len+1],
		diriter->current.cFileName, filename_len * sizeof(wchar_t));
	diriter->path[path_len-1] = L'\0';

	git_str_truncate(&diriter->path_utf8, diriter->parent_utf8_len);

	if (diriter->parent_utf8_len > 0 &&
		diriter->path_utf8.ptr[diriter->parent_utf8_len-1] != '/')
		git_str_putc(&diriter->path_utf8, '/');

	git_str_put_w(&diriter->path_utf8, diriter->current.cFileName, filename_len);

	if (git_str_oom(&diriter->path_utf8))
		return -1;

	return 0;
}

int git_fs_path_diriter_next(git_fs_path_diriter *diriter)
{
	bool skip_dot = !(diriter->flags & GIT_FS_PATH_DIR_INCLUDE_DOT_AND_DOTDOT);

	do {
		/* Our first time through, we already have the data from
		 * FindFirstFileW.  Use it, otherwise get the next file.
		 */
		if (!diriter->needs_next)
			diriter->needs_next = 1;
		else if (!FindNextFileW(diriter->handle, &diriter->current))
			return GIT_ITEROVER;
	} while (skip_dot && git_fs_path_is_dot_or_dotdotW(diriter->current.cFileName));

	if (diriter_update_paths(diriter) < 0)
		return -1;

	return 0;
}

int git_fs_path_diriter_filename(
	const char **out,
	size_t *out_len,
	git_fs_path_diriter *diriter)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(out_len);
	GIT_ASSERT_ARG(diriter);
	GIT_ASSERT(diriter->path_utf8.size > diriter->parent_utf8_len);

	*out = &diriter->path_utf8.ptr[diriter->parent_utf8_len+1];
	*out_len = diriter->path_utf8.size - diriter->parent_utf8_len - 1;
	return 0;
}

int git_fs_path_diriter_fullpath(
	const char **out,
	size_t *out_len,
	git_fs_path_diriter *diriter)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(out_len);
	GIT_ASSERT_ARG(diriter);

	*out = diriter->path_utf8.ptr;
	*out_len = diriter->path_utf8.size;
	return 0;
}

int git_fs_path_diriter_stat(struct stat *out, git_fs_path_diriter *diriter)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(diriter);

	return git_win32__file_attribute_to_stat(out,
		(WIN32_FILE_ATTRIBUTE_DATA *)&diriter->current,
		diriter->path);
}

void git_fs_path_diriter_free(git_fs_path_diriter *diriter)
{
	if (diriter == NULL)
		return;

	git_str_dispose(&diriter->path_utf8);

	if (diriter->handle != INVALID_HANDLE_VALUE) {
		FindClose(diriter->handle);
		diriter->handle = INVALID_HANDLE_VALUE;
	}
}

#else

int git_fs_path_diriter_init(
	git_fs_path_diriter *diriter,
	const char *path,
	unsigned int flags)
{
	GIT_ASSERT_ARG(diriter);
	GIT_ASSERT_ARG(path);

	memset(diriter, 0, sizeof(git_fs_path_diriter));

	if (git_str_puts(&diriter->path, path) < 0)
		return -1;

	path_trim_slashes(&diriter->path);

	if (diriter->path.size == 0) {
		git_error_set(GIT_ERROR_FILESYSTEM, "could not open directory '%s'", path);
		return -1;
	}

	if ((diriter->dir = opendir(diriter->path.ptr)) == NULL) {
		git_str_dispose(&diriter->path);

		git_error_set(GIT_ERROR_OS, "failed to open directory '%s'", path);
		return -1;
	}

#ifdef GIT_USE_ICONV
	if ((flags & GIT_FS_PATH_DIR_PRECOMPOSE_UNICODE) != 0)
		(void)git_fs_path_iconv_init_precompose(&diriter->ic);
#endif

	diriter->parent_len = diriter->path.size;
	diriter->flags = flags;

	return 0;
}

int git_fs_path_diriter_next(git_fs_path_diriter *diriter)
{
	struct dirent *de;
	const char *filename;
	size_t filename_len;
	bool skip_dot = !(diriter->flags & GIT_FS_PATH_DIR_INCLUDE_DOT_AND_DOTDOT);
	int error = 0;

	GIT_ASSERT_ARG(diriter);

	errno = 0;

	do {
		if ((de = readdir(diriter->dir)) == NULL) {
			if (!errno)
				return GIT_ITEROVER;

			git_error_set(GIT_ERROR_OS,
				"could not read directory '%s'", diriter->path.ptr);
			return -1;
		}
	} while (skip_dot && git_fs_path_is_dot_or_dotdot(de->d_name));

	filename = de->d_name;
	filename_len = strlen(filename);

#ifdef GIT_USE_ICONV
	if ((diriter->flags & GIT_FS_PATH_DIR_PRECOMPOSE_UNICODE) != 0 &&
		(error = git_fs_path_iconv(&diriter->ic, &filename, &filename_len)) < 0)
		return error;
#endif

	git_str_truncate(&diriter->path, diriter->parent_len);

	if (diriter->parent_len > 0 &&
		diriter->path.ptr[diriter->parent_len-1] != '/')
		git_str_putc(&diriter->path, '/');

	git_str_put(&diriter->path, filename, filename_len);

	if (git_str_oom(&diriter->path))
		return -1;

	return error;
}

int git_fs_path_diriter_filename(
	const char **out,
	size_t *out_len,
	git_fs_path_diriter *diriter)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(out_len);
	GIT_ASSERT_ARG(diriter);
	GIT_ASSERT(diriter->path.size > diriter->parent_len);

	*out = &diriter->path.ptr[diriter->parent_len+1];
	*out_len = diriter->path.size - diriter->parent_len - 1;
	return 0;
}

int git_fs_path_diriter_fullpath(
	const char **out,
	size_t *out_len,
	git_fs_path_diriter *diriter)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(out_len);
	GIT_ASSERT_ARG(diriter);

	*out = diriter->path.ptr;
	*out_len = diriter->path.size;
	return 0;
}

int git_fs_path_diriter_stat(struct stat *out, git_fs_path_diriter *diriter)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(diriter);

	return git_fs_path_lstat(diriter->path.ptr, out);
}

void git_fs_path_diriter_free(git_fs_path_diriter *diriter)
{
	if (diriter == NULL)
		return;

	if (diriter->dir) {
		closedir(diriter->dir);
		diriter->dir = NULL;
	}

#ifdef GIT_USE_ICONV
	git_fs_path_iconv_clear(&diriter->ic);
#endif

	git_str_dispose(&diriter->path);
}

#endif

int git_fs_path_dirload(
	git_vector *contents,
	const char *path,
	size_t prefix_len,
	uint32_t flags)
{
	git_fs_path_diriter iter = GIT_FS_PATH_DIRITER_INIT;
	const char *name;
	size_t name_len;
	char *dup;
	int error;

	GIT_ASSERT_ARG(contents);
	GIT_ASSERT_ARG(path);

	if ((error = git_fs_path_diriter_init(&iter, path, flags)) < 0)
		return error;

	while ((error = git_fs_path_diriter_next(&iter)) == 0) {
		if ((error = git_fs_path_diriter_fullpath(&name, &name_len, &iter)) < 0)
			break;

		GIT_ASSERT(name_len > prefix_len);

		dup = git__strndup(name + prefix_len, name_len - prefix_len);
		GIT_ERROR_CHECK_ALLOC(dup);

		if ((error = git_vector_insert(contents, dup)) < 0)
			break;
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_fs_path_diriter_free(&iter);
	return error;
}

int git_fs_path_from_url_or_path(git_str *local_path_out, const char *url_or_path)
{
	if (git_fs_path_is_local_file_url(url_or_path))
		return git_fs_path_fromurl(local_path_out, url_or_path);
	else
		return git_str_sets(local_path_out, url_or_path);
}

/* Reject paths like AUX or COM1, or those versions that end in a dot or
 * colon.  ("AUX." or "AUX:")
 */
GIT_INLINE(bool) validate_dospath(
	const char *component,
	size_t len,
	const char dospath[3],
	bool trailing_num)
{
	size_t last = trailing_num ? 4 : 3;

	if (len < last || git__strncasecmp(component, dospath, 3) != 0)
		return true;

	if (trailing_num && (component[3] < '1' || component[3] > '9'))
		return true;

	return (len > last &&
		component[last] != '.' &&
		component[last] != ':');
}

GIT_INLINE(bool) validate_char(unsigned char c, unsigned int flags)
{
	if ((flags & GIT_FS_PATH_REJECT_BACKSLASH) && c == '\\')
		return false;

	if ((flags & GIT_FS_PATH_REJECT_SLASH) && c == '/')
		return false;

	if (flags & GIT_FS_PATH_REJECT_NT_CHARS) {
		if (c < 32)
			return false;

		switch (c) {
		case '<':
		case '>':
		case ':':
		case '"':
		case '|':
		case '?':
		case '*':
			return false;
		}
	}

	return true;
}

/*
 * We fundamentally don't like some paths when dealing with user-inputted
 * strings (to avoid escaping a sandbox): we don't want dot or dot-dot
 * anywhere, we want to avoid writing weird paths on Windows that can't
 * be handled by tools that use the non-\\?\ APIs, we don't want slashes
 * or double slashes at the end of paths that can make them ambiguous.
 *
 * For checkout, we don't want to recurse into ".git" either.
 */
static bool validate_component(
	const char *component,
	size_t len,
	unsigned int flags)
{
	if (len == 0)
		return !(flags & GIT_FS_PATH_REJECT_EMPTY_COMPONENT);

	if ((flags & GIT_FS_PATH_REJECT_TRAVERSAL) &&
	    len == 1 && component[0] == '.')
		return false;

	if ((flags & GIT_FS_PATH_REJECT_TRAVERSAL) &&
	    len == 2 && component[0] == '.' && component[1] == '.')
		return false;

	if ((flags & GIT_FS_PATH_REJECT_TRAILING_DOT) &&
	    component[len - 1] == '.')
		return false;

	if ((flags & GIT_FS_PATH_REJECT_TRAILING_SPACE) &&
	    component[len - 1] == ' ')
		return false;

	if ((flags & GIT_FS_PATH_REJECT_TRAILING_COLON) &&
	    component[len - 1] == ':')
		return false;

	if (flags & GIT_FS_PATH_REJECT_DOS_PATHS) {
		if (!validate_dospath(component, len, "CON", false) ||
		    !validate_dospath(component, len, "PRN", false) ||
		    !validate_dospath(component, len, "AUX", false) ||
		    !validate_dospath(component, len, "NUL", false) ||
		    !validate_dospath(component, len, "COM", true)  ||
		    !validate_dospath(component, len, "LPT", true))
			return false;
	}

	return true;
}

#ifdef GIT_WIN32
GIT_INLINE(bool) validate_length(
	const char *path,
	size_t len,
	size_t utf8_char_len)
{
	GIT_UNUSED(path);
	GIT_UNUSED(len);

	return (utf8_char_len <= MAX_PATH);
}
#endif

bool git_fs_path_str_is_valid_ext(
	const git_str *path,
	unsigned int flags,
	bool (*validate_char_cb)(char ch, void *payload),
	bool (*validate_component_cb)(const char *component, size_t len, void *payload),
	bool (*validate_length_cb)(const char *path, size_t len, size_t utf8_char_len),
	void *payload)
{
	const char *start, *c;
	size_t len = 0;

	if (!flags)
		return true;

	for (start = c = path->ptr; *c && len < path->size; c++, len++) {
		if (!validate_char(*c, flags))
			return false;

		if (validate_char_cb && !validate_char_cb(*c, payload))
			return false;

		if (*c != '/')
			continue;

		if (!validate_component(start, (c - start), flags))
			return false;

		if (validate_component_cb &&
		    !validate_component_cb(start, (c - start), payload))
			return false;

		start = c + 1;
	}

	/*
	 * We want to support paths specified as either `const char *`
	 * or `git_str *`; we pass size as `SIZE_MAX` when we use a
	 * `const char *` to avoid a `strlen`.  Ensure that we didn't
	 * have a NUL in the buffer if there was a non-SIZE_MAX length.
	 */
	if (path->size != SIZE_MAX && len != path->size)
		return false;

	if (!validate_component(start, (c - start), flags))
		return false;

	if (validate_component_cb &&
	    !validate_component_cb(start, (c - start), payload))
		return false;

#ifdef GIT_WIN32
	if ((flags & GIT_FS_PATH_REJECT_LONG_PATHS) != 0) {
		size_t utf8_len = git_utf8_char_length(path->ptr, len);

		if (!validate_length(path->ptr, len, utf8_len))
			return false;

		if (validate_length_cb &&
		    !validate_length_cb(path->ptr, len, utf8_len))
			return false;
	}
#else
	GIT_UNUSED(validate_length_cb);
#endif

	return true;
}

int git_fs_path_validate_str_length_with_suffix(
	git_str *path,
	size_t suffix_len)
{
#ifdef GIT_WIN32
	size_t utf8_len = git_utf8_char_length(path->ptr, path->size);
	size_t total_len;

	if (GIT_ADD_SIZET_OVERFLOW(&total_len, utf8_len, suffix_len) ||
	    total_len > MAX_PATH) {

		git_error_set(GIT_ERROR_FILESYSTEM, "path too long: '%.*s'",
			(int)path->size, path->ptr);
		return -1;
	}
#else
	GIT_UNUSED(path);
	GIT_UNUSED(suffix_len);
#endif

	return 0;
}

int git_fs_path_normalize_slashes(git_str *out, const char *path)
{
	int error;
	char *p;

	if ((error = git_str_puts(out, path)) < 0)
		return error;

	for (p = out->ptr; *p; p++) {
		if (*p == '\\')
			*p = '/';
	}

	return 0;
}

bool git_fs_path_supports_symlinks(const char *dir)
{
	git_str path = GIT_STR_INIT;
	bool supported = false;
	struct stat st;
	int fd;

	if ((fd = git_futils_mktmp(&path, dir, 0666)) < 0 ||
	    p_close(fd) < 0 ||
	    p_unlink(path.ptr) < 0 ||
	    p_symlink("testing", path.ptr) < 0 ||
	    p_lstat(path.ptr, &st) < 0)
		goto done;

	supported = (S_ISLNK(st.st_mode) != 0);
done:
	if (path.size)
		(void)p_unlink(path.ptr);
	git_str_dispose(&path);
	return supported;
}

static git_fs_path_owner_t mock_owner = GIT_FS_PATH_OWNER_NONE;

void git_fs_path__set_owner(git_fs_path_owner_t owner)
{
	mock_owner = owner;
}

#ifdef GIT_WIN32
static PSID *sid_dup(PSID sid)
{
	DWORD len;
	PSID dup;

	len = GetLengthSid(sid);

	if ((dup = git__malloc(len)) == NULL)
		return NULL;

	if (!CopySid(len, dup, sid)) {
		git_error_set(GIT_ERROR_OS, "could not duplicate sid");
		git__free(dup);
		return NULL;
	}

	return dup;
}

static int current_user_sid(PSID *out)
{
	TOKEN_USER *info = NULL;
	HANDLE token = NULL;
	DWORD len = 0;
	int error = -1;

	if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)) {
		git_error_set(GIT_ERROR_OS, "could not lookup process information");
		goto done;
	}

	if (GetTokenInformation(token, TokenUser, NULL, 0, &len) ||
		GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
		git_error_set(GIT_ERROR_OS, "could not lookup token metadata");
		goto done;
	}

	info = git__malloc(len);
	GIT_ERROR_CHECK_ALLOC(info);

	if (!GetTokenInformation(token, TokenUser, info, len, &len)) {
		git_error_set(GIT_ERROR_OS, "could not lookup current user");
		goto done;
	}

	if ((*out = sid_dup(info->User.Sid)))
		error = 0;

done:
	if (token)
		CloseHandle(token);

	git__free(info);
	return error;
}

static int file_owner_sid(PSID *out, const char *path)
{
	git_win32_path path_w32;
	PSECURITY_DESCRIPTOR descriptor = NULL;
	PSID owner_sid;
	DWORD ret;
	int error = GIT_EINVALID;

	if (git_win32_path_from_utf8(path_w32, path) < 0)
		return -1;

	ret = GetNamedSecurityInfoW(path_w32, SE_FILE_OBJECT,
		OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
		&owner_sid, NULL, NULL, NULL, &descriptor);

	if (ret == ERROR_FILE_NOT_FOUND || ret == ERROR_PATH_NOT_FOUND)
		error = GIT_ENOTFOUND;
	else if (ret != ERROR_SUCCESS)
		git_error_set(GIT_ERROR_OS, "failed to get security information");
	else if (!IsValidSid(owner_sid))
		git_error_set(GIT_ERROR_OS, "file owner is not valid");
	else if ((*out = sid_dup(owner_sid)))
		error = 0;

	if (descriptor)
		LocalFree(descriptor);

	return error;
}

int git_fs_path_owner_is(
	bool *out,
	const char *path,
	git_fs_path_owner_t owner_type)
{
	PSID owner_sid = NULL, user_sid = NULL;
	BOOL is_admin, admin_owned;
	int error;

	if (mock_owner) {
		*out = ((mock_owner & owner_type) != 0);
		return 0;
	}

	if ((error = file_owner_sid(&owner_sid, path)) < 0)
		goto done;

	if ((owner_type & GIT_FS_PATH_OWNER_CURRENT_USER) != 0) {
		if ((error = current_user_sid(&user_sid)) < 0)
			goto done;

		if (EqualSid(owner_sid, user_sid)) {
			*out = true;
			goto done;
		}
	}

	admin_owned =
		IsWellKnownSid(owner_sid, WinBuiltinAdministratorsSid) ||
		IsWellKnownSid(owner_sid, WinLocalSystemSid);

	if (admin_owned &&
	    (owner_type & GIT_FS_PATH_OWNER_ADMINISTRATOR) != 0) {
		*out = true;
		goto done;
	}

	if (admin_owned &&
	    (owner_type & GIT_FS_PATH_USER_IS_ADMINISTRATOR) != 0 &&
	    CheckTokenMembership(NULL, owner_sid, &is_admin) &&
	    is_admin) {
		*out = true;
		goto done;
	}

	*out = false;

done:
	git__free(owner_sid);
	git__free(user_sid);
	return error;
}

#else

static int sudo_uid_lookup(uid_t *out)
{
	git_str uid_str = GIT_STR_INIT;
	int64_t uid;
	int error = -1;

	if (git__getenv(&uid_str, "SUDO_UID") == 0 &&
		git__strntol64(&uid, uid_str.ptr, uid_str.size, NULL, 10) == 0 &&
		uid == (int64_t)((uid_t)uid)) {
		*out = (uid_t)uid;
		error = 0;
	}

	git_str_dispose(&uid_str);
	return error;
}

int git_fs_path_owner_is(
	bool *out,
	const char *path,
	git_fs_path_owner_t owner_type)
{
	struct stat st;
	uid_t euid, sudo_uid;

	if (mock_owner) {
		*out = ((mock_owner & owner_type) != 0);
		return 0;
	}

	euid = geteuid();

	if (p_lstat(path, &st) != 0) {
		if (errno == ENOENT)
			return GIT_ENOTFOUND;

		git_error_set(GIT_ERROR_OS, "could not stat '%s'", path);
		return -1;
	}

	if ((owner_type & GIT_FS_PATH_OWNER_CURRENT_USER) != 0 &&
	    st.st_uid == euid) {
		*out = true;
		return 0;
	}

	if ((owner_type & GIT_FS_PATH_OWNER_ADMINISTRATOR) != 0 &&
	    st.st_uid == 0) {
		*out = true;
		return 0;
	}

	if ((owner_type & GIT_FS_PATH_OWNER_RUNNING_SUDO) != 0 &&
	    euid == 0 &&
	    sudo_uid_lookup(&sudo_uid) == 0 &&
	    st.st_uid == sudo_uid) {
		*out = true;
		return 0;
	}

	*out = false;
	return 0;
}

#endif

int git_fs_path_owner_is_current_user(bool *out, const char *path)
{
	return git_fs_path_owner_is(out, path, GIT_FS_PATH_OWNER_CURRENT_USER);
}

int git_fs_path_owner_is_system(bool *out, const char *path)
{
	return git_fs_path_owner_is(out, path, GIT_FS_PATH_OWNER_ADMINISTRATOR);
}

int git_fs_path_find_executable(git_str *fullpath, const char *executable)
{
#ifdef GIT_WIN32
	git_win32_path fullpath_w, executable_w;
	int error;

	if (git_utf8_to_16(executable_w, GIT_WIN_PATH_MAX, executable) < 0)
		return -1;

	error = git_win32_path_find_executable(fullpath_w, executable_w);

	if (error == 0)
		error = git_str_put_w(fullpath, fullpath_w, wcslen(fullpath_w));

	return error;
#else
	git_str path = GIT_STR_INIT;
	const char *current_dir, *term;
	bool found = false;

	if (git__getenv(&path, "PATH") < 0)
		return -1;

	current_dir = path.ptr;

	while (*current_dir) {
		if (! (term = strchr(current_dir, GIT_PATH_LIST_SEPARATOR)))
			term = strchr(current_dir, '\0');

		git_str_clear(fullpath);
		if (git_str_put(fullpath, current_dir, (term - current_dir)) < 0 ||
		    git_str_putc(fullpath, '/') < 0 ||
		    git_str_puts(fullpath, executable) < 0)
			return -1;

		if (git_fs_path_isfile(fullpath->ptr)) {
			found = true;
			break;
		}

		current_dir = term;

		while (*current_dir == GIT_PATH_LIST_SEPARATOR)
			current_dir++;
	}

	git_str_dispose(&path);

	if (found)
		return 0;

	git_str_clear(fullpath);
	return GIT_ENOTFOUND;
#endif
}
