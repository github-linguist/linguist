/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_fetchhead_h__
#define INCLUDE_fetchhead_h__

#include "common.h"

#include "oid.h"
#include "vector.h"

typedef struct git_fetchhead_ref {
	git_oid oid;
	unsigned int is_merge;
	char *ref_name;
	char *remote_url;
} git_fetchhead_ref;

int git_fetchhead_ref_create(
	git_fetchhead_ref **fetchhead_ref_out,
	git_oid *oid,
	unsigned int is_merge,
	const char *ref_name,
	const char *remote_url);

int git_fetchhead_ref_cmp(const void *a, const void *b);

int git_fetchhead_write(git_repository *repo, git_vector *fetchhead_refs);

void git_fetchhead_ref_free(git_fetchhead_ref *fetchhead_ref);

#endif
