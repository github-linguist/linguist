/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_pathspec_h__
#define INCLUDE_pathspec_h__

#include "common.h"

#include "git2/pathspec.h"
#include "str.h"
#include "vector.h"
#include "pool.h"
#include "array.h"

/* public compiled pathspec */
struct git_pathspec {
	git_refcount rc;
	char *prefix;
	git_vector pathspec;
	git_pool pool;
};

enum {
	PATHSPEC_DATATYPE_STRINGS = 0,
	PATHSPEC_DATATYPE_DIFF = 1
};

typedef git_array_t(char *) git_pathspec_string_array_t;

/* public interface to pathspec matching */
struct git_pathspec_match_list {
	git_pathspec *pathspec;
	git_array_t(void *) matches;
	git_pathspec_string_array_t failures;
	git_pool pool;
	int datatype;
};

/* what is the common non-wildcard prefix for all items in the pathspec */
extern char *git_pathspec_prefix(const git_strarray *pathspec);

/* is there anything in the spec that needs to be filtered on */
extern bool git_pathspec_is_empty(const git_strarray *pathspec);

/* build a vector of fnmatch patterns to evaluate efficiently */
extern int git_pathspec__vinit(
	git_vector *vspec, const git_strarray *strspec, git_pool *strpool);

/* free data from the pathspec vector */
extern void git_pathspec__vfree(git_vector *vspec);

#define GIT_PATHSPEC_NOMATCH ((size_t)-1)

/*
 * Match a path against the vectorized pathspec.
 * The matched pathspec is passed back into the `matched_pathspec` parameter,
 * unless it is passed as NULL by the caller.
 */
extern bool git_pathspec__match(
	const git_vector *vspec,
	const char *path,
	bool disable_fnmatch,
	bool casefold,
	const char **matched_pathspec,
	size_t *matched_at);

/* easy pathspec setup */

extern int git_pathspec__init(git_pathspec *ps, const git_strarray *paths);

extern void git_pathspec__clear(git_pathspec *ps);

#endif
