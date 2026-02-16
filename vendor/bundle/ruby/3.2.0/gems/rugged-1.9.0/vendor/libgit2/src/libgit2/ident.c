/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/sys/filter.h"
#include "filter.h"
#include "str.h"

static int ident_find_id(
	const char **id_start, const char **id_end, const char *start, size_t len)
{
	const char *end = start + len, *found = NULL;

	while (len > 3 && (found = memchr(start, '$', len)) != NULL) {
		size_t remaining = (size_t)(end - found) - 1;
		if (remaining < 3)
			return GIT_ENOTFOUND;

		start = found + 1;
		len   = remaining;

		if (start[0] == 'I' && start[1] == 'd')
			break;
	}

	if (len < 3 || !found)
		return GIT_ENOTFOUND;
	*id_start = found;

	if ((found = memchr(start + 2, '$', len - 2)) == NULL)
		return GIT_ENOTFOUND;

	*id_end = found + 1;
	return 0;
}

static int ident_insert_id(
	git_str *to, const git_str *from, const git_filter_source *src)
{
	char oid[GIT_OID_MAX_HEXSIZE + 1];
	const char *id_start, *id_end, *from_end = from->ptr + from->size;
	size_t need_size;

	/* replace $Id$ with blob id */

	if (!git_filter_source_id(src))
		return GIT_PASSTHROUGH;

	git_oid_tostr(oid, sizeof(oid), git_filter_source_id(src));

	if (ident_find_id(&id_start, &id_end, from->ptr, from->size) < 0)
		return GIT_PASSTHROUGH;

	need_size = (size_t)(id_start - from->ptr) +
		5 /* "$Id: " */ + GIT_OID_MAX_HEXSIZE + 2 /* " $" */ +
		(size_t)(from_end - id_end);

	if (git_str_grow(to, need_size) < 0)
		return -1;

	git_str_set(to, from->ptr, (size_t)(id_start - from->ptr));
	git_str_put(to, "$Id: ", 5);
	git_str_puts(to, oid);
	git_str_put(to, " $", 2);
	git_str_put(to, id_end, (size_t)(from_end - id_end));

	return git_str_oom(to) ? -1 : 0;
}

static int ident_remove_id(
	git_str *to, const git_str *from)
{
	const char *id_start, *id_end, *from_end = from->ptr + from->size;
	size_t need_size;

	if (ident_find_id(&id_start, &id_end, from->ptr, from->size) < 0)
		return GIT_PASSTHROUGH;

	need_size = (size_t)(id_start - from->ptr) +
		4 /* "$Id$" */ + (size_t)(from_end - id_end);

	if (git_str_grow(to, need_size) < 0)
		return -1;

	git_str_set(to, from->ptr, (size_t)(id_start - from->ptr));
	git_str_put(to, "$Id$", 4);
	git_str_put(to, id_end, (size_t)(from_end - id_end));

	return git_str_oom(to) ? -1 : 0;
}

static int ident_apply(
	git_filter     *self,
	void          **payload,
	git_str        *to,
	const git_str  *from,
	const git_filter_source *src)
{
	GIT_UNUSED(self); GIT_UNUSED(payload);

	/* Don't filter binary files */
	if (git_str_is_binary(from))
		return GIT_PASSTHROUGH;

	if (git_filter_source_mode(src) == GIT_FILTER_SMUDGE)
		return ident_insert_id(to, from, src);
	else
		return ident_remove_id(to, from);
}

static int ident_stream(
	git_writestream **out,
	git_filter *self,
	void **payload,
	const git_filter_source *src,
	git_writestream *next)
{
	return git_filter_buffered_stream_new(out,
		self, ident_apply, NULL, payload, src, next);
}

git_filter *git_ident_filter_new(void)
{
	git_filter *f = git__calloc(1, sizeof(git_filter));
	if (f == NULL)
		return NULL;

	f->version = GIT_FILTER_VERSION;
	f->attributes = "+ident"; /* apply to files with ident attribute set */
	f->shutdown = git_filter_free;
	f->stream   = ident_stream;

	return f;
}
