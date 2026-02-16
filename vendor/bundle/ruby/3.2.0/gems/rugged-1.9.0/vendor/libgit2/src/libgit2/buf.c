/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "buf.h"
#include "common.h"

int git_buf_sanitize(git_buf *buf)
{
	GIT_ASSERT_ARG(buf);

	if (buf->reserved > 0)
		buf->ptr[0] = '\0';
	else
		buf->ptr = git_str__initstr;

	buf->size = 0;
	return 0;
}

int git_buf_tostr(git_str *out, git_buf *buf)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(buf);

	if (git_buf_sanitize(buf) < 0)
		return -1;

	out->ptr = buf->ptr;
	out->asize = buf->reserved;
	out->size = buf->size;

	buf->ptr = git_str__initstr;
	buf->reserved = 0;
	buf->size = 0;

	return 0;
}

int git_buf_fromstr(git_buf *out, git_str *str)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(str);

	out->ptr = str->ptr;
	out->reserved = str->asize;
	out->size = str->size;

	str->ptr = git_str__initstr;
	str->asize = 0;
	str->size = 0;

	return 0;
}

void git_buf_dispose(git_buf *buf)
{
	if (!buf)
		return;

	if (buf->ptr != git_str__initstr)
		git__free(buf->ptr);

	buf->ptr = git_str__initstr;
	buf->reserved = 0;
	buf->size = 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_buf_grow(git_buf *buffer, size_t target_size)
{
	char *newptr;

	if (buffer->reserved >= target_size)
		return 0;

	if (buffer->ptr == git_str__initstr)
		newptr = git__malloc(target_size);
	else
		newptr = git__realloc(buffer->ptr, target_size);

	if (!newptr)
		return -1;

	buffer->ptr = newptr;
	buffer->reserved = target_size;
	return 0;
}

int git_buf_set(git_buf *buffer, const void *data, size_t datalen)
{
	size_t alloclen;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, datalen, 1);

	if (git_buf_grow(buffer, alloclen) < 0)
		return -1;

	memmove(buffer->ptr, data, datalen);
	buffer->size = datalen;
	buffer->ptr[buffer->size] = '\0';

	return 0;
}

int git_buf_is_binary(const git_buf *buf)
{
	git_str str = GIT_STR_INIT_CONST(buf->ptr, buf->size);
	return git_str_is_binary(&str);
}

int git_buf_contains_nul(const git_buf *buf)
{
	git_str str = GIT_STR_INIT_CONST(buf->ptr, buf->size);
	return git_str_contains_nul(&str);
}

void git_buf_free(git_buf *buffer)
{
	git_buf_dispose(buffer);
}

#endif
