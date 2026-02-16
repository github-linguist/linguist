/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "utf-conv.h"

GIT_INLINE(void) git__set_errno(void)
{
	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
		errno = ENAMETOOLONG;
	else
		errno = EINVAL;
}

int git_utf8_to_16(wchar_t *dest, size_t dest_size, const char *src)
{
	/* Length of -1 indicates NULL termination of the input string. */
	return git_utf8_to_16_with_len(dest, dest_size, src, -1);
}

int git_utf8_to_16_with_len(
	wchar_t *dest,
	size_t _dest_size,
	const char *src,
	int src_len)
{
	int dest_size = (int)min(_dest_size, INT_MAX);
	int len;

	/*
	 * Subtract 1 from the result to turn 0 into -1 (an error code) and
	 * to not count the NULL terminator as part of the string's length.
	 * MultiByteToWideChar never returns int's minvalue, so underflow
	 * is not possible.
	 */
	len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
		src, src_len, dest, dest_size) - 1;

	if (len < 0)
		git__set_errno();

	return len;
}

int git_utf8_from_16(char *dest, size_t dest_size, const wchar_t *src)
{
	/* Length of -1 indicates NULL termination of the input string. */
	return git_utf8_from_16_with_len(dest, dest_size, src, -1);
}

int git_utf8_from_16_with_len(
	char *dest,
	size_t _dest_size,
	const wchar_t *src,
	int src_len)
{
	int dest_size = (int)min(_dest_size, INT_MAX);
	int len;

	/*
	 * Subtract 1 from the result to turn 0 into -1 (an error code) and
	 * to not count the NULL terminator as part of the string's length.
	 * WideCharToMultiByte never returns int's minvalue, so underflow
	 * is not possible.
	 */
	len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
		src, src_len, dest, dest_size, NULL, NULL) - 1;

	if (len < 0)
		git__set_errno();

	return len;
}

int git_utf8_to_16_alloc(wchar_t **dest, const char *src)
{
	/* Length of -1 indicates NULL termination of the input string. */
	return git_utf8_to_16_alloc_with_len(dest, src, -1);
}

int git_utf8_to_16_alloc_with_len(wchar_t **dest, const char *src, int src_len)
{
	int utf16_size;

	*dest = NULL;

	utf16_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
		src, src_len, NULL, 0);

	if (!utf16_size) {
		git__set_errno();
		return -1;
	}

	*dest = git__mallocarray(utf16_size, sizeof(wchar_t));
	GIT_ERROR_CHECK_ALLOC(*dest);

	utf16_size = git_utf8_to_16_with_len(*dest, (size_t)utf16_size,
		src, src_len);

	if (utf16_size < 0) {
		git__free(*dest);
		*dest = NULL;
	}

	return utf16_size;
}

int git_utf8_from_16_alloc(char **dest, const wchar_t *src)
{
	/* Length of -1 indicates NULL termination of the input string. */
	return git_utf8_from_16_alloc_with_len(dest, src, -1);
}

int git_utf8_from_16_alloc_with_len(char **dest, const wchar_t *src, int src_len)
{
	int utf8_size;

	*dest = NULL;

	utf8_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
		src, src_len, NULL, 0, NULL, NULL);

	if (!utf8_size) {
		git__set_errno();
		return -1;
	}

	*dest = git__malloc(utf8_size);
	GIT_ERROR_CHECK_ALLOC(*dest);

	utf8_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS,
		src, src_len, *dest, utf8_size, NULL, NULL);

	if (utf8_size < 0) {
		git__free(*dest);
		*dest = NULL;
	}

	return utf8_size;
}
