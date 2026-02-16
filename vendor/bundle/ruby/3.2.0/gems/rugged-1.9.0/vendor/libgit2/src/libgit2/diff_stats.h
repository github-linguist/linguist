/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_diff_stats_h__
#define INCLUDE_diff_stats_h__

#include "common.h"

int git_diff__stats_to_buf(
	git_str *out,
	const git_diff_stats *stats,
	git_diff_stats_format_t format,
	size_t width);

#endif
