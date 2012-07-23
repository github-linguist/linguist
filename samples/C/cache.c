/*
 * Copyright (C) 2009-2012 the libgit2 contributors
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "repository.h"
#include "commit.h"
#include "thread-utils.h"
#include "util.h"
#include "cache.h"

int git_cache_init(git_cache *cache, size_t size, git_cached_obj_freeptr free_ptr)
{
	if (size < 8)
		size = 8;
	size = git__size_t_powerof2(size);

	cache->size_mask = size - 1;
	cache->lru_count = 0;
	cache->free_obj = free_ptr;

	git_mutex_init(&cache->lock);

	cache->nodes = git__malloc(size * sizeof(git_cached_obj *));
	GITERR_CHECK_ALLOC(cache->nodes);

	memset(cache->nodes, 0x0, size * sizeof(git_cached_obj *));
	return 0;
}

void git_cache_free(git_cache *cache)
{
	size_t i;

	for (i = 0; i < (cache->size_mask + 1); ++i) {
		if (cache->nodes[i] != NULL)
			git_cached_obj_decref(cache->nodes[i], cache->free_obj);
	}

	git__free(cache->nodes);
}

void *git_cache_get(git_cache *cache, const git_oid *oid)
{
	uint32_t hash;
	git_cached_obj *node = NULL, *result = NULL;

	memcpy(&hash, oid->id, sizeof(hash));

	git_mutex_lock(&cache->lock);
	{
		node = cache->nodes[hash & cache->size_mask];

		if (node != NULL && git_oid_cmp(&node->oid, oid) == 0) {
			git_cached_obj_incref(node);
			result = node;
		}
	}
	git_mutex_unlock(&cache->lock);

	return result;
}

void *git_cache_try_store(git_cache *cache, void *_entry)
{
	git_cached_obj *entry = _entry;
	uint32_t hash;

	memcpy(&hash, &entry->oid, sizeof(uint32_t));

	/* increase the refcount on this object, because
	 * the cache now owns it */
	git_cached_obj_incref(entry);

	git_mutex_lock(&cache->lock);
	{
		git_cached_obj *node = cache->nodes[hash & cache->size_mask];

		if (node == NULL) {
			cache->nodes[hash & cache->size_mask] = entry;
		} else if (git_oid_cmp(&node->oid, &entry->oid) == 0) {
			git_cached_obj_decref(entry, cache->free_obj);
			entry = node;
		} else {
			git_cached_obj_decref(node, cache->free_obj);
			cache->nodes[hash & cache->size_mask] = entry;
		}
	}
	git_mutex_unlock(&cache->lock);

	/* increase the refcount again, because we are
	 * returning it to the user */
	git_cached_obj_incref(entry);

	return entry;
}
