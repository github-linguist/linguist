/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_status_h__
#define INCLUDE_status_h__

#include "common.h"

#include "diff.h"
#include "git2/status.h"
#include "git2/diff.h"

struct git_status_list {
	git_status_options opts;

	git_diff *head2idx;
	git_diff *idx2wd;

	git_vector paired;
};

#endif
