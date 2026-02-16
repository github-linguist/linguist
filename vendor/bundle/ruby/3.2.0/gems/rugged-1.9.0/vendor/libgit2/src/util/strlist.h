/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_runtime_h__
#define INCLUDE_runtime_h__

#include "git2_util.h"

extern int git_strlist_copy(char ***out, const char **in, size_t len);

extern int git_strlist_copy_with_null(
	char ***out,
	const char **in,
	size_t len);

extern bool git_strlist_contains_prefix(
	const char **strings,
	size_t len,
	const char *str,
	size_t n);

extern bool git_strlist_contains_key(
	const char **strings,
	size_t len,
	const char *key,
	char delimiter);

extern void git_strlist_free(char **strings, size_t len);

extern void git_strlist_free_with_null(char **strings);

#endif
