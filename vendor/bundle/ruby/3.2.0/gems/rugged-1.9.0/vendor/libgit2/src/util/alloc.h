/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_alloc_h__
#define INCLUDE_alloc_h__

#include "git2/sys/alloc.h"

#include "git2_util.h"

extern git_allocator git__allocator;

GIT_INLINE(void *) git__malloc(size_t len)
{
	void *p = git__allocator.gmalloc(len, __FILE__, __LINE__);

	if (!p)
		git_error_set_oom();

	return p;
}

GIT_INLINE(void *) git__realloc(void *ptr, size_t size)
{
	void *p = git__allocator.grealloc(ptr, size, __FILE__, __LINE__);

	if (!p)
		git_error_set_oom();

	return p;
}

GIT_INLINE(void) git__free(void *ptr)
{
	git__allocator.gfree(ptr);
}

extern void *git__calloc(size_t nelem, size_t elsize);
extern void *git__mallocarray(size_t nelem, size_t elsize);
extern void *git__reallocarray(void *ptr, size_t nelem, size_t elsize);

extern char *git__strdup(const char *str);
extern char *git__strndup(const char *str, size_t n);
extern char *git__substrdup(const char *str, size_t n);

/**
 * This function is being called by our global setup routines to
 * initialize the standard allocator.
 */
int git_allocator_global_init(void);

/**
 * Switch out libgit2's global memory allocator
 *
 * @param allocator The new allocator that should be used. All function pointers
 *                  of it need to be set correctly.
 * @return An error code or 0.
 */
int git_allocator_setup(git_allocator *allocator);

#endif
