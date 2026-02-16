/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_pool_h__
#define INCLUDE_pool_h__

#include "git2_util.h"

#include "vector.h"

typedef struct git_pool_page git_pool_page;

#ifndef GIT_DEBUG_POOL
/**
 * Chunked allocator.
 *
 * A `git_pool` can be used when you want to cheaply allocate
 * multiple items of the same type and are willing to free them
 * all together with a single call.  The two most common cases
 * are a set of fixed size items (such as lots of OIDs) or a
 * bunch of strings.
 *
 * Internally, a `git_pool` allocates pages of memory and then
 * deals out blocks from the trailing unused portion of each page.
 * The pages guarantee that the number of actual allocations done
 * will be much smaller than the number of items needed.
 *
 * For examples of how to set up a `git_pool` see `git_pool_init`.
 */
typedef struct {
	git_pool_page *pages; /* allocated pages */
	size_t item_size;  /* size of single alloc unit in bytes */
	size_t page_size;  /* size of page in bytes */
} git_pool;

#define GIT_POOL_INIT { NULL, 0, 0 }

#else

/**
 * Debug chunked allocator.
 *
 * Acts just like `git_pool` but instead of actually pooling allocations it
 * passes them through to `git__malloc`. This makes it possible to easily debug
 * systems that use `git_pool` using valgrind.
 *
 * In order to track allocations during the lifetime of the pool we use a
 * `git_vector`. When the pool is deallocated everything in the vector is
 * freed.
 *
 * `API is exactly the same as the standard `git_pool` with one exception.
 * Since we aren't allocating pages to hand out in chunks we can't easily
 * implement `git_pool__open_pages`.
 */
typedef struct {
	git_vector allocations;
	size_t item_size;
	size_t page_size;
} git_pool;

#define GIT_POOL_INIT { GIT_VECTOR_INIT, 0, 0 }

#endif

/**
 * Initialize a pool.
 *
 * To allocation strings, use like this:
 *
 *     git_pool_init(&string_pool, 1);
 *     my_string = git_pool_strdup(&string_pool, your_string);
 *
 * To allocate items of fixed size, use like this:
 *
 *     git_pool_init(&pool, sizeof(item));
 *     my_item = git_pool_malloc(&pool, 1);
 *
 * Of course, you can use this in other ways, but those are the
 * two most common patterns.
 */
extern int git_pool_init(git_pool *pool, size_t item_size);

GIT_INLINE(bool) git_pool_is_initialized(git_pool *pool)
{
	return (pool->item_size > 0);
}

/**
 * Free all items in pool
 */
extern void git_pool_clear(git_pool *pool);

/**
 * Swap two pools with one another
 */
extern void git_pool_swap(git_pool *a, git_pool *b);

/**
 * Allocate space for one or more items from a pool.
 */
extern void *git_pool_malloc(git_pool *pool, size_t items);
extern void *git_pool_mallocz(git_pool *pool, size_t items);

/**
 * Allocate space and duplicate string data into it.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strndup(git_pool *pool, const char *str, size_t n);

/**
 * Allocate space and duplicate a string into it.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strdup(git_pool *pool, const char *str);

/**
 * Allocate space and duplicate a string into it, NULL is no error.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strdup_safe(git_pool *pool, const char *str);

/**
 * Allocate space for the concatenation of two strings.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strcat(git_pool *pool, const char *a, const char *b);

/*
 * Misc utilities
 */
#ifndef GIT_DEBUG_POOL
extern uint32_t git_pool__open_pages(git_pool *pool);
#endif
extern bool git_pool__ptr_in_pool(git_pool *pool, void *ptr);

/**
 * This function is being called by our global setup routines to
 * initialize the system pool size.
 *
 * @return 0 on success, <0 on failure
 */
extern int git_pool_global_init(void);

#endif
