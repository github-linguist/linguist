/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "w32_buffer.h"

#include "utf-conv.h"

GIT_INLINE(int) handle_wc_error(void)
{
	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
		errno = ENAMETOOLONG;
	else
		errno = EINVAL;

	return -1;
}

int git_str_put_w(git_str *buf, const wchar_t *string_w, size_t len_w)
{
	int utf8_len, utf8_write_len;
	size_t new_size;

	if (!len_w) {
		return 0;
	} else if (len_w > INT_MAX) {
		git_error_set_oom();
		return -1;
	}

	GIT_ASSERT(string_w);

	/* Measure the string necessary for conversion */
	if ((utf8_len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, string_w, (int)len_w, NULL, 0, NULL, NULL)) == 0)
		return 0;

	GIT_ASSERT(utf8_len > 0);

	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, buf->size, (size_t)utf8_len);
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);

	if (git_str_grow(buf, new_size) < 0)
		return -1;

	if ((utf8_write_len = WideCharToMultiByte(
			CP_UTF8, WC_ERR_INVALID_CHARS, string_w, (int)len_w, &buf->ptr[buf->size], utf8_len, NULL, NULL)) == 0)
		return handle_wc_error();

	GIT_ASSERT(utf8_write_len == utf8_len);

	buf->size += utf8_write_len;
	buf->ptr[buf->size] = '\0';
	return 0;
}
