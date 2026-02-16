/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_diff_h__
#define INCLUDE_diff_h__

#include "common.h"

#include "git2/diff.h"
#include "git2/patch.h"
#include "git2/sys/diff.h"
#include "git2/oid.h"

#include "vector.h"
#include "iterator.h"
#include "repository.h"
#include "pool.h"
#include "odb.h"

#define DIFF_OLD_PREFIX_DEFAULT "a/"
#define DIFF_NEW_PREFIX_DEFAULT "b/"

typedef enum {
	GIT_DIFF_TYPE_UNKNOWN = 0,
	GIT_DIFF_TYPE_GENERATED = 1,
	GIT_DIFF_TYPE_PARSED = 2
} git_diff_origin_t;

struct git_diff {
	git_refcount      rc;
	git_repository   *repo;
	git_attr_session  attrsession;
	git_diff_origin_t type;
	git_diff_options  opts;
	git_vector        deltas;    /* vector of git_diff_delta */
	git_pool pool;
	git_iterator_t    old_src;
	git_iterator_t    new_src;
	git_diff_perfdata perf;

	int (*strcomp)(const char *, const char *);
	int (*strncomp)(const char *, const char *, size_t);
	int (*pfxcomp)(const char *str, const char *pfx);
	int (*entrycomp)(const void *a, const void *b);

	int (*patch_fn)(git_patch **out, git_diff *diff, size_t idx);
	void (*free_fn)(git_diff *diff);
};

extern int git_diff_delta__format_file_header(
	git_str *out,
	const git_diff_delta *delta,
	const char *oldpfx,
	const char *newpfx,
	int oid_strlen,
	bool print_index);

extern int git_diff_delta__cmp(const void *a, const void *b);
extern int git_diff_delta__casecmp(const void *a, const void *b);

extern int git_diff__entry_cmp(const void *a, const void *b);
extern int git_diff__entry_icmp(const void *a, const void *b);

#endif
