/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_attrcache_h__
#define INCLUDE_attrcache_h__

#include "common.h"

#include "attr_file.h"

#define GIT_ATTR_CONFIG       "core.attributesfile"
#define GIT_IGNORE_CONFIG     "core.excludesfile"

typedef struct git_attr_cache git_attr_cache;

extern int git_attr_cache__init(git_repository *repo);

extern const char *git_attr_cache_attributesfile(git_attr_cache *ac);
extern const char *git_attr_cache_excludesfile(git_attr_cache *ac);
extern git_pool *git_attr_cache_pool(git_attr_cache *ac);

/* get file - loading and reload as needed */
extern int git_attr_cache__get(
	git_attr_file **file,
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_file_source *source,
	git_attr_file_parser parser,
	bool allow_macros);

extern bool git_attr_cache__is_cached(
	git_repository *repo,
	git_attr_file_source_t source_type,
	const char *filename);

extern int git_attr_cache__alloc_file_entry(
	git_attr_file_entry **out,
	git_repository *repo,
	const char *base,
	const char *path,
	git_pool *pool);

extern int git_attr_cache__insert_macro(
	git_repository *repo, git_attr_rule *macro);

extern git_attr_rule *git_attr_cache__lookup_macro(
	git_repository *repo, const char *name);

#endif
