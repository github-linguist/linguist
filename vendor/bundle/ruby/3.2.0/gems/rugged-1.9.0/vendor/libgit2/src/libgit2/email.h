/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_email_h__
#define INCLUDE_email_h__

#include "common.h"

#include "git2/email.h"

extern int git_email__append_from_diff(
	git_str *out,
	git_diff *diff,
	size_t patch_idx,
	size_t patch_count,
	const git_oid *commit_id,
	const char *summary,
	const char *body,
	const git_signature *author,
	const git_email_create_options *given_opts);

#endif
