/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_clone_h__
#define INCLUDE_clone_h__

#include "common.h"

#include "git2/clone.h"

extern int git_clone__submodule(git_repository **out,
	const char *url, const char *local_path,
	const git_clone_options *_options);

extern int git_clone__should_clone_local(
	bool *out,
	const char *url,
	git_clone_local_t local);

#endif
