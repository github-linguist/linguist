/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"
#include <git2/sys/alloc.h>

static void *rugged_gmalloc(size_t n, const char *file, int line)
{
	return xmalloc(n);
}

static void *rugged_grealloc(void *ptr, size_t size, const char *file, int line)
{
	return xrealloc(ptr, size);
}

static void rugged_gfree(void *ptr)
{
	xfree(ptr);
}

void rugged_set_allocator(void)
{
	git_allocator allocator;

	allocator.gmalloc = rugged_gmalloc;
	allocator.grealloc = rugged_grealloc;
	allocator.gfree = rugged_gfree;

	git_libgit2_opts(GIT_OPT_SET_ALLOCATOR, &allocator);
}
