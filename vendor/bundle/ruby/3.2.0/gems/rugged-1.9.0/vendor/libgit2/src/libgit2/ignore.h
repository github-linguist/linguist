/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_ignore_h__
#define INCLUDE_ignore_h__

#include "common.h"

#include "repository.h"
#include "vector.h"
#include "attr_file.h"

#define GIT_IGNORE_FILE			".gitignore"
#define GIT_IGNORE_FILE_INREPO	"exclude"
#define GIT_IGNORE_FILE_XDG		"ignore"

/* The git_ignores structure maintains three sets of ignores:
 * - internal ignores
 * - per directory ignores
 * - global ignores (at lower priority than the others)
 * As you traverse from one directory to another, you can push and pop
 * directories onto git_ignores list efficiently.
 */
typedef struct {
	git_repository *repo;
	git_str dir; /* current directory reflected in ign_path */
	git_attr_file *ign_internal;
	git_vector ign_path;
	git_vector ign_global;
	size_t dir_root; /* offset in dir to repo root */
	int ignore_case;
	int depth;
} git_ignores;

extern int git_ignore__for_path(
	git_repository *repo, const char *path, git_ignores *ign);

extern int git_ignore__push_dir(git_ignores *ign, const char *dir);

extern int git_ignore__pop_dir(git_ignores *ign);

extern void git_ignore__free(git_ignores *ign);

enum {
	GIT_IGNORE_UNCHECKED = -2,
	GIT_IGNORE_NOTFOUND = -1,
	GIT_IGNORE_FALSE = 0,
	GIT_IGNORE_TRUE = 1
};

extern int git_ignore__lookup(int *out, git_ignores *ign, const char *path, git_dir_flag dir_flag);

/* command line Git sometimes generates an error message if given a
 * pathspec that contains an exact match to an ignored file (provided
 * --force isn't also given).  This makes it easy to check it that has
 * happened.  Returns GIT_EINVALIDSPEC if the pathspec contains ignored
 * exact matches (that are not already present in the index).
 */
extern int git_ignore__check_pathspec_for_exact_ignores(
	git_repository *repo, git_vector *pathspec, bool no_fnmatch);

#endif
