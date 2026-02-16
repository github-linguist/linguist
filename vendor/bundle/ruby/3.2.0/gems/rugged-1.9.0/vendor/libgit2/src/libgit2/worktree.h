/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_worktree_h__
#define INCLUDE_worktree_h__

#include "common.h"

#include "git2/common.h"
#include "git2/worktree.h"

struct git_worktree {
	/* Name of the working tree. This is the name of the
	 * containing directory in the `$PARENT/.git/worktrees/`
	 * directory. */
	char *name;

	/* Path to the where the worktree lives in the filesystem */
	char *worktree_path;
	/* Path to the .git file in the working tree's repository */
	char *gitlink_path;
	/* Path to the .git directory inside the parent's
	 * worktrees directory */
	char *gitdir_path;
	/* Path to the common directory contained in the parent
	 * repository */
	char *commondir_path;
	/* Path to the parent's working directory */
	char *parent_path;

	unsigned int locked:1;
};

char *git_worktree__read_link(const char *base, const char *file);

#endif
