/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_diff_generate_h__
#define INCLUDE_diff_generate_h__

#include "common.h"

#include "diff.h"
#include "pool.h"
#include "index.h"

enum {
	GIT_DIFFCAPS_HAS_SYMLINKS     = (1 << 0), /* symlinks on platform? */
	GIT_DIFFCAPS_IGNORE_STAT      = (1 << 1), /* use stat? */
	GIT_DIFFCAPS_TRUST_MODE_BITS  = (1 << 2), /* use st_mode? */
	GIT_DIFFCAPS_TRUST_CTIME      = (1 << 3), /* use st_ctime? */
	GIT_DIFFCAPS_USE_DEV          = (1 << 4)  /* use st_dev? */
};

#define DIFF_FLAGS_KNOWN_BINARY (GIT_DIFF_FLAG_BINARY|GIT_DIFF_FLAG_NOT_BINARY)
#define DIFF_FLAGS_NOT_BINARY   (GIT_DIFF_FLAG_NOT_BINARY|GIT_DIFF_FLAG__NO_DATA)

enum {
	GIT_DIFF_FLAG__FREE_PATH  = (1 << 7),  /* `path` is allocated memory */
	GIT_DIFF_FLAG__FREE_DATA  = (1 << 8),  /* internal file data is allocated */
	GIT_DIFF_FLAG__UNMAP_DATA = (1 << 9),  /* internal file data is mmap'ed */
	GIT_DIFF_FLAG__NO_DATA    = (1 << 10), /* file data should not be loaded */
	GIT_DIFF_FLAG__FREE_BLOB  = (1 << 11), /* release the blob when done */
	GIT_DIFF_FLAG__LOADED     = (1 << 12), /* file data has been loaded */

	GIT_DIFF_FLAG__TO_DELETE  = (1 << 16), /* delete entry during rename det. */
	GIT_DIFF_FLAG__TO_SPLIT   = (1 << 17), /* split entry during rename det. */
	GIT_DIFF_FLAG__IS_RENAME_TARGET = (1 << 18),
	GIT_DIFF_FLAG__IS_RENAME_SOURCE = (1 << 19),
	GIT_DIFF_FLAG__HAS_SELF_SIMILARITY = (1 << 20)
};

#define GIT_DIFF_FLAG__CLEAR_INTERNAL(F) (F) = ((F) & 0x00FFFF)

#define GIT_DIFF__VERBOSE  (1 << 30)

extern void git_diff_addref(git_diff *diff);

extern bool git_diff_delta__should_skip(
	const git_diff_options *opts, const git_diff_delta *delta);

extern int git_diff__from_iterators(
	git_diff **diff_ptr,
	git_repository *repo,
	git_iterator *old_iter,
	git_iterator *new_iter,
	const git_diff_options *opts);

extern int git_diff__commit(
	git_diff **diff, git_repository *repo, const git_commit *commit, const git_diff_options *opts);

extern int git_diff__paired_foreach(
	git_diff *idx2head,
	git_diff *wd2idx,
	int (*cb)(git_diff_delta *i2h, git_diff_delta *w2i, void *payload),
	void *payload);

/* Merge two `git_diff`s according to the callback given by `cb`. */

typedef git_diff_delta *(*git_diff__merge_cb)(
	const git_diff_delta *left,
	const git_diff_delta *right,
	git_pool *pool);

extern int git_diff__merge(
	git_diff *onto, const git_diff *from, git_diff__merge_cb cb);

extern git_diff_delta *git_diff__merge_like_cgit(
	const git_diff_delta *a,
	const git_diff_delta *b,
	git_pool *pool);

/* Duplicate a `git_diff_delta` out of the `git_pool` */
extern git_diff_delta *git_diff__delta_dup(
	const git_diff_delta *d, git_pool *pool);

extern int git_diff__oid_for_file(
	git_oid *out,
	git_diff *diff,
	const char *path,
	uint16_t mode,
	git_object_size_t size);

extern int git_diff__oid_for_entry(
	git_oid *out,
	git_diff *diff,
	const git_index_entry *src,
	uint16_t mode,
	const git_oid *update_match);

/*
 * Sometimes a git_diff_file will have a zero size; this attempts to
 * fill in the size without loading the blob if possible.  If that is
 * not possible, then it will return the git_odb_object that had to be
 * loaded and the caller can use it or dispose of it as needed.
 */
GIT_INLINE(int) git_diff_file__resolve_zero_size(
	git_diff_file *file, git_odb_object **odb_obj, git_repository *repo)
{
	int error;
	git_odb *odb;
	size_t len;
	git_object_t type;

	if ((error = git_repository_odb(&odb, repo)) < 0)
		return error;

	error = git_odb__read_header_or_object(
		odb_obj, &len, &type, odb, &file->id);

	git_odb_free(odb);

	if (!error) {
		file->size = (git_object_size_t)len;
		file->flags |= GIT_DIFF_FLAG_VALID_SIZE;
	}

	return error;
}

#endif
