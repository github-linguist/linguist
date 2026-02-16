/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_path_h__
#define INCLUDE_path_h__

#include "common.h"

#include "fs_path.h"
#include <git2/sys/path.h>

#define GIT_PATH_REJECT_DOT_GIT            (GIT_FS_PATH_REJECT_MAX << 1)
#define GIT_PATH_REJECT_DOT_GIT_LITERAL    (GIT_FS_PATH_REJECT_MAX << 2)
#define GIT_PATH_REJECT_DOT_GIT_HFS        (GIT_FS_PATH_REJECT_MAX << 3)
#define GIT_PATH_REJECT_DOT_GIT_NTFS       (GIT_FS_PATH_REJECT_MAX << 4)

/* Paths that should never be written into the working directory. */
#define GIT_PATH_REJECT_WORKDIR_DEFAULTS \
	GIT_FS_PATH_REJECT_FILESYSTEM_DEFAULTS | GIT_PATH_REJECT_DOT_GIT

/* Paths that should never be written to the index. */
#define GIT_PATH_REJECT_INDEX_DEFAULTS \
	GIT_FS_PATH_REJECT_TRAVERSAL | GIT_PATH_REJECT_DOT_GIT

extern bool git_path_str_is_valid(
	git_repository *repo,
	const git_str *path,
	uint16_t file_mode,
	unsigned int flags);

GIT_INLINE(bool) git_path_is_valid(
	git_repository *repo,
	const char *path,
	uint16_t file_mode,
	unsigned int flags)
{
	git_str str = GIT_STR_INIT_CONST(path, SIZE_MAX);
	return git_path_str_is_valid(repo, &str, file_mode, flags);
}

GIT_INLINE(int) git_path_validate_str_length(
	git_repository *repo,
	const git_str *path)
{
	if (!git_path_str_is_valid(repo, path, 0, GIT_FS_PATH_REJECT_LONG_PATHS)) {
		if (path->size == SIZE_MAX)
			git_error_set(GIT_ERROR_FILESYSTEM, "path too long: '%s'", path->ptr);
		else
			git_error_set(GIT_ERROR_FILESYSTEM, "path too long: '%.*s'", (int)path->size, path->ptr);

		return -1;
	}

	return 0;
}

GIT_INLINE(int) git_path_validate_length(
	git_repository *repo,
	const char *path)
{
	git_str str = GIT_STR_INIT_CONST(path, SIZE_MAX);
	return git_path_validate_str_length(repo, &str);
}

#endif
