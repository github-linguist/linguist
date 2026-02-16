/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "stdalloc.h"

static void *stdalloc__malloc(size_t len, const char *file, int line)
{
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return malloc(len);
}

static void *stdalloc__realloc(void *ptr, size_t size, const char *file, int line)
{
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return realloc(ptr, size);
}

static void stdalloc__free(void *ptr)
{
	free(ptr);
}

int git_stdalloc_init_allocator(git_allocator *allocator)
{
	allocator->gmalloc = stdalloc__malloc;
	allocator->grealloc = stdalloc__realloc;
	allocator->gfree = stdalloc__free;
	return 0;
}
