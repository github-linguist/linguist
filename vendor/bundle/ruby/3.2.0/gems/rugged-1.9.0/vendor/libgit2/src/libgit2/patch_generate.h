/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_patch_generate_h__
#define INCLUDE_patch_generate_h__

#include "common.h"

#include "diff.h"
#include "diff_file.h"
#include "patch.h"

enum {
	GIT_PATCH_GENERATED_ALLOCATED = (1 << 0),
	GIT_PATCH_GENERATED_INITIALIZED = (1 << 1),
	GIT_PATCH_GENERATED_LOADED = (1 << 2),
	/* the two sides are different */
	GIT_PATCH_GENERATED_DIFFABLE = (1 << 3),
	/* the difference between the two sides has been computed */
	GIT_PATCH_GENERATED_DIFFED = (1 << 4),
	GIT_PATCH_GENERATED_FLATTENED = (1 << 5)
};

struct git_patch_generated {
	struct git_patch base;

	git_diff *diff; /* for refcount purposes, maybe NULL for blob diffs */
	size_t delta_index;
	git_diff_file_content ofile;
	git_diff_file_content nfile;
	uint32_t flags;
	git_pool flattened;
};

typedef struct git_patch_generated git_patch_generated;

extern git_diff_driver *git_patch_generated_driver(git_patch_generated *);

extern int git_patch_generated_old_data(
	char **, long *, git_patch_generated *);
extern int git_patch_generated_new_data(
	char **, long *, git_patch_generated *);
extern int git_patch_generated_from_diff(
	git_patch **, git_diff *, size_t);

typedef struct git_patch_generated_output git_patch_generated_output;

struct git_patch_generated_output {
	/* these callbacks are issued with the diff data */
	git_diff_file_cb file_cb;
	git_diff_binary_cb binary_cb;
	git_diff_hunk_cb hunk_cb;
	git_diff_line_cb data_cb;
	void *payload;

	/* this records the actual error in cases where it may be obscured */
	int error;

	/* this callback is used to do the diff and drive the other callbacks.
	 * see diff_xdiff.h for how to use this in practice for now.
	 */
	int (*diff_cb)(git_patch_generated_output *output,
		git_patch_generated *patch);
};

#endif
