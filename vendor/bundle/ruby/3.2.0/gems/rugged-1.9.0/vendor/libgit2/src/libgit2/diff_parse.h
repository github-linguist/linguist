/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_diff_parse_h__
#define INCLUDE_diff_parse_h__

#include "common.h"

#include "diff.h"

typedef struct {
	struct git_diff base;

	git_vector patches;
} git_diff_parsed;

#endif
