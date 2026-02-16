/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_apply_h__
#define INCLUDE_apply_h__

#include "common.h"

#include "git2/patch.h"
#include "git2/apply.h"
#include "str.h"

extern int git_apply__patch(
	git_str *out,
	char **filename,
	unsigned int *mode,
	const char *source,
	size_t source_len,
	git_patch *patch,
	const git_apply_options *opts);

#endif
