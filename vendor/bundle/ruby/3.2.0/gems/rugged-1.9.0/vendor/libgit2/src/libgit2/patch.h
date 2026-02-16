/*
* Copyright (C) the libgit2 contributors. All rights reserved.
*
* This file is part of libgit2, distributed under the GNU GPL v2 with
* a Linking Exception. For full terms see the included COPYING file.
*/
#ifndef INCLUDE_patch_h__
#define INCLUDE_patch_h__

#include "common.h"

#include "git2/patch.h"
#include "array.h"

/* cached information about a hunk in a patch */
typedef struct git_patch_hunk {
	git_diff_hunk hunk;
	size_t line_start;
	size_t line_count;
} git_patch_hunk;

struct git_patch {
	git_refcount rc;

	git_repository *repo; /* may be null */

	git_diff_options diff_opts;

	git_diff_delta *delta;
	git_diff_binary binary;
	git_array_t(git_patch_hunk) hunks;
	git_array_t(git_diff_line) lines;

	size_t header_size;
	size_t content_size;
	size_t context_size;

	void (*free_fn)(git_patch *patch);
};

extern int git_patch__invoke_callbacks(
	git_patch *patch,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb line_cb,
	void *payload);

extern int git_patch_line_stats(
	size_t *total_ctxt,
	size_t *total_adds,
	size_t *total_dels,
	const git_patch *patch);

/** Options for parsing patch files. */
typedef struct {
	/**
	 * The length of the prefix (in path segments) for the filenames.
	 * This prefix will be removed when looking for files.  The default is 1.
	 */
	uint32_t prefix_len;

	/**
	 * The type of object IDs in the patch file. The default is
	 * `GIT_OID_DEFAULT`.
	 */
	git_oid_t oid_type;
} git_patch_options;

#define GIT_PATCH_OPTIONS_INIT { 1, GIT_OID_DEFAULT }

extern int git_patch__to_buf(git_str *out, git_patch *patch);
extern void git_patch_free(git_patch *patch);

#endif
