/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_blame_git__
#define INCLUDE_blame_git__

#include "common.h"

#include "blame.h"

int git_blame__get_origin(
		git_blame__origin **out,
		git_blame *sb,
		git_commit *commit,
		const char *path);
void git_blame__free_entry(git_blame__entry *ent);
int git_blame__like_git(git_blame *sb, uint32_t flags);

#endif
