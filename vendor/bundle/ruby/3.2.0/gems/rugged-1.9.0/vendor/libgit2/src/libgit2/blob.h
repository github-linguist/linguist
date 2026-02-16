/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_blob_h__
#define INCLUDE_blob_h__

#include "common.h"

#include "git2/blob.h"
#include "repository.h"
#include "odb.h"
#include "futils.h"

struct git_blob {
	git_object object;

	union {
		git_odb_object *odb;
		struct {
			const char *data;
			git_object_size_t size;
		} raw;
	} data;
	unsigned int raw:1;
};

#define GIT_ERROR_CHECK_BLOBSIZE(n) \
	do { \
		if (!git__is_sizet(n)) { \
			git_error_set(GIT_ERROR_NOMEMORY, "blob contents too large to fit in memory"); \
			return -1; \
		} \
	} while(0)

void git_blob__free(void *blob);
int git_blob__parse(void *blob, git_odb_object *obj, git_oid_t oid_type);
int git_blob__parse_raw(void *blob, const char *data, size_t size, git_oid_t oid_type);
int git_blob__getbuf(git_str *buffer, git_blob *blob);

extern int git_blob__create_from_paths(
	git_oid *out_oid,
	struct stat *out_st,
	git_repository *repo,
	const char *full_path,
	const char *hint_path,
	mode_t hint_mode,
	bool apply_filters);

#endif
