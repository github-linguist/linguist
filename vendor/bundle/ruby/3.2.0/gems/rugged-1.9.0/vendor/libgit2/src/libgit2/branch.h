/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_branch_h__
#define INCLUDE_branch_h__

#include "common.h"

#include "str.h"

int git_branch__remote_name(
	git_str *out,
	git_repository *repo,
	const char *refname);
int git_branch__upstream_remote(
	git_str *out,
	git_repository *repo,
	const char *refname);
int git_branch__upstream_merge(
	git_str *out,
	git_repository *repo,
	const char *refname);
int git_branch__upstream_name(
	git_str *tracking_name,
	git_repository *repo,
	const char *canonical_branch_name);

#endif
