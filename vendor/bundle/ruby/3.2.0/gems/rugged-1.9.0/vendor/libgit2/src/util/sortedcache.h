/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_sorted_cache_h__
#define INCLUDE_sorted_cache_h__

#include "git2_util.h"

#include "util.h"
#include "futils.h"
#include "vector.h"
#include "thread.h"
#include "pool.h"
#include "hashmap_str.h"

#include <stddef.h>

/*
 * The purpose of this data structure is to cache the parsed contents of a
 * file (a.k.a. the backing file) where each item in the file can be
 * identified by a key string and you want to both look them up by name
 * and traverse them in sorted order.  Each item is assumed to itself end
 * in a GIT_FLEX_ARRAY.
 */

typedef void (*git_sortedcache_free_item_fn)(void *payload, void *item);

typedef struct {
	git_refcount rc;
	git_rwlock   lock;
	size_t       item_path_offset;
	git_sortedcache_free_item_fn free_item;
	void         *free_item_payload;
	git_pool     pool;
	git_vector   items;
	git_hashmap_str map;
	git_futils_filestamp stamp;
	char         path[GIT_FLEX_ARRAY];
} git_sortedcache;

/* Create a new sortedcache
 *
 * Even though every sortedcache stores items with a GIT_FLEX_ARRAY at
 * the end containing their key string, you have to provide the item_cmp
 * sorting function because the sorting function doesn't get a payload
 * and therefore can't know the offset to the item key string. :-(
 *
 * @param out The allocated git_sortedcache
 * @param item_path_offset Offset to the GIT_FLEX_ARRAY item key in the
 *        struct - use offsetof(struct mine, key-field) to get this
 * @param free_item Optional callback to free each item
 * @param free_item_payload Optional payload passed to free_item callback
 * @param item_cmp Compare the keys of two items
 * @param path The path to the backing store file for this cache; this
 *        may be NULL.  The cache makes it easy to load this and check
 *        if it has been modified since the last load and/or write.
 */
GIT_WARN_UNUSED_RESULT int git_sortedcache_new(
	git_sortedcache **out,
	size_t item_path_offset, /* use offsetof(struct, path-field) macro */
	git_sortedcache_free_item_fn free_item,
	void *free_item_payload,
	git_vector_cmp item_cmp,
	const char *path);

/* Copy a sorted cache
 *
 * - `copy_item` can be NULL to just use memcpy
 * - if `lock`, grabs read lock on `src` during copy and releases after
 */
GIT_WARN_UNUSED_RESULT int git_sortedcache_copy(
	git_sortedcache **out,
	git_sortedcache *src,
	bool lock,
	int (*copy_item)(void *payload, void *tgt_item, void *src_item),
	void *payload);

/* Free sorted cache (first calling `free_item` callbacks)
 *
 * Don't call on a locked collection - it may acquire a write lock
 */
void git_sortedcache_free(git_sortedcache *sc);

/* Increment reference count - balance with call to free */
void git_sortedcache_incref(git_sortedcache *sc);

/* Get the pathname associated with this cache at creation time */
const char *git_sortedcache_path(git_sortedcache *sc);

/*
 * CACHE WRITE FUNCTIONS
 *
 * The following functions require you to have a writer lock to make the
 * modification.  Some of the functions take a `wlock` parameter and
 * will optionally lock and unlock for you if that is passed as true.
 *
 */

/* Lock sortedcache for write */
GIT_WARN_UNUSED_RESULT int git_sortedcache_wlock(git_sortedcache *sc);

/* Unlock sorted cache when done with write */
void git_sortedcache_wunlock(git_sortedcache *sc);

/* Lock cache and load backing file into a buffer.
 *
 * This grabs a write lock on the cache then looks at the modification
 * time and size of the file on disk.
 *
 * If the file appears to have changed, this loads the file contents into
 * the buffer and returns a positive value leaving the cache locked - the
 * caller should parse the file content, update the cache as needed, then
 * release the lock.  NOTE: In this case, the caller MUST unlock the cache.
 *
 * If the file appears to be unchanged, then this automatically releases
 * the lock on the cache, clears the buffer, and returns 0.
 *
 * @return 0 if up-to-date, 1 if out-of-date, <0 on error
 */
GIT_WARN_UNUSED_RESULT int git_sortedcache_lockandload(
	git_sortedcache *sc, git_str *buf);

/* Refresh file timestamp after write completes
 * You should already be holding the write lock when you call this.
 */
void git_sortedcache_updated(git_sortedcache *sc);

/* Release all items in sorted cache
 *
 * If `wlock` is true, grabs write lock and releases when done, otherwise
 * you should already be holding a write lock when you call this.
 */
GIT_WARN_UNUSED_RESULT int git_sortedcache_clear(
	git_sortedcache *sc, bool wlock);

/* Find and/or insert item, returning pointer to item data.
 * You should already be holding the write lock when you call this.
 */
GIT_WARN_UNUSED_RESULT int git_sortedcache_upsert(
	void **out, git_sortedcache *sc, const char *key);

/* Removes entry at pos from cache
 * You should already be holding the write lock when you call this.
 */
int git_sortedcache_remove(git_sortedcache *sc, size_t pos);

/*
 * CACHE READ FUNCTIONS
 *
 * The following functions access items in the cache.  To prevent the
 * results from being invalidated before they can be used, you should be
 * holding either a read lock or a write lock when using these functions.
 *
 */

/* Lock sortedcache for read */
GIT_WARN_UNUSED_RESULT int git_sortedcache_rlock(git_sortedcache *sc);

/* Unlock sorted cache when done with read */
void git_sortedcache_runlock(git_sortedcache *sc);

/* Lookup item by key - returns NULL if not found */
void *git_sortedcache_lookup(git_sortedcache *sc, const char *key);

/* Get how many items are in the cache
 *
 * You can call this function without holding a lock, but be aware
 * that it may change before you use it.
 */
size_t git_sortedcache_entrycount(const git_sortedcache *sc);

/* Lookup item by index - returns NULL if out of range */
void *git_sortedcache_entry(git_sortedcache *sc, size_t pos);

/* Lookup index of item by key - returns GIT_ENOTFOUND if not found */
int git_sortedcache_lookup_index(
	size_t *out, git_sortedcache *sc, const char *key);

#endif
