/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_object_h__
#define INCLUDE_object_h__

#include "common.h"

#include "repository.h"

#define GIT_OBJECT_SIZE_MAX UINT64_MAX

extern bool git_object__strict_input_validation;

/** Base git object for inheritance */
struct git_object {
	git_cached_obj cached;
	git_repository *repo;
};

/* fully free the object; internal method, DO NOT EXPORT */
void git_object__free(void *object);

/*
 * Parse object from raw data. Note that the resulting object is
 * tied to the lifetime of the data, as some objects simply point
 * into it.
 */
int git_object__from_raw(
	git_object **object_out,
	const char *data,
	size_t size,
	git_object_t object_type,
	git_oid_t oid_type);

int git_object__init_from_odb_object(
	git_object **object_out,
	git_repository *repo,
	git_odb_object *odb_obj,
	git_object_t type);

int git_object__from_odb_object(
	git_object **object_out,
	git_repository *repo,
	git_odb_object *odb_obj,
	git_object_t type);

int git_object__resolve_to_type(git_object **obj, git_object_t type);

git_object_t git_object_stringn2type(const char *str, size_t len);

int git_object__parse_oid_header(
	git_oid *oid,
	const char **buffer_out,
	const char *buffer_end,
	const char *header,
	git_oid_t oid_type);

int git_object__write_oid_header(
	git_str *buf,
	const char *header,
	const git_oid *oid);

bool git_object__is_valid(
	git_repository *repo, const git_oid *id, git_object_t expected_type);

GIT_INLINE(git_object_t) git_object__type_from_filemode(git_filemode_t mode)
{
	switch (mode) {
	case GIT_FILEMODE_TREE:
		return GIT_OBJECT_TREE;
	case GIT_FILEMODE_COMMIT:
		return GIT_OBJECT_COMMIT;
	case GIT_FILEMODE_BLOB:
	case GIT_FILEMODE_BLOB_EXECUTABLE:
	case GIT_FILEMODE_LINK:
		return GIT_OBJECT_BLOB;
	default:
		return GIT_OBJECT_INVALID;
	}
}

#endif
