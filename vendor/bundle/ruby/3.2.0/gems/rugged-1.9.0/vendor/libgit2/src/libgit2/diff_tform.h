/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_diff_tform_h__
#define INCLUDE_diff_tform_h__

#include "common.h"

#include "diff_file.h"

extern int git_diff_find_similar__hashsig_for_file(
	void **out, const git_diff_file *f, const char *path, void *p);

extern int git_diff_find_similar__hashsig_for_buf(
	void **out, const git_diff_file *f, const char *buf, size_t len, void *p);

extern void git_diff_find_similar__hashsig_free(void *sig, void *payload);

extern int git_diff_find_similar__calc_similarity(
	int *score, void *siga, void *sigb, void *payload);

#endif
