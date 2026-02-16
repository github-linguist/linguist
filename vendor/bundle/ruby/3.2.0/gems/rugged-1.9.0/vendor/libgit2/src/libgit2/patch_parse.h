/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_patch_parse_h__
#define INCLUDE_patch_parse_h__

#include "common.h"

#include "parse.h"
#include "patch.h"

typedef struct {
	git_refcount rc;

	git_patch_options opts;

	git_parse_ctx parse_ctx;
} git_patch_parse_ctx;

extern git_patch_parse_ctx *git_patch_parse_ctx_init(
	const char *content,
	size_t content_len,
	const git_patch_options *opts);

extern void git_patch_parse_ctx_free(git_patch_parse_ctx *ctx);

/**
 * Create a patch for a single file from the contents of a patch buffer.
 *
 * @param out The patch to be created
 * @param contents The contents of a patch file
 * @param contents_len The length of the patch file
 * @param opts The git_patch_options
 * @return 0 on success, <0 on failure.
 */
extern int git_patch_from_buffer(
	git_patch **out,
	const char *contents,
	size_t contents_len,
	const git_patch_options *opts);

extern int git_patch_parse(
	git_patch **out,
	git_patch_parse_ctx *ctx);

extern int git_patch_parsed_from_diff(git_patch **, git_diff *, size_t);

#endif
