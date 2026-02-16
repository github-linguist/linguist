/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_hashmap_h__
#define INCLUDE_hashmap_h__

/*
 * This is a variation on khash.h from khlib 2013-05-02 (0.2.8)
 *
 * The MIT License
 *
 * Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define GIT_HASHMAP_INIT {0}
#define GIT_HASHSET_INIT {0}

#define GIT_HASHMAP_EMPTY
#define GIT_HASHMAP_INLINE GIT_INLINE(GIT_HASHMAP_EMPTY)

#define GIT_HASHMAP_IS_EMPTY(flag, i)        ((flag[i>>4]>>((i&0xfU)<<1))&2)
#define GIT_HASHMAP_IS_DELETE(flag, i)       ((flag[i>>4]>>((i&0xfU)<<1))&1)
#define GIT_HASHMAP_IS_EITHER(flag, i)       ((flag[i>>4]>>((i&0xfU)<<1))&3)
#define GIT_HASHMAP_SET_EMPTY_FALSE(flag, i)  (flag[i>>4]&=~(2ul<<((i&0xfU)<<1)))
#define GIT_HASHMAP_SET_DELETE_TRUE(flag, i)  (flag[i>>4]|=1ul<<((i&0xfU)<<1))
#define GIT_HASHMAP_SET_DELETE_FALSE(flag, i) (flag[i>>4]&=~(1ul<<((i&0xfU)<<1)))
#define GIT_HASHMAP_SET_BOTH_FALSE(flag, i)   (flag[i>>4]&=~(3ul<<((i&0xfU)<<1)))

#define GIT_HASHMAP_FLAGSIZE(m)              ((m) < 16? 1 : (m)>>4)
#define GIT_HASHMAP_ROUNDUP(x)                (--(x), (x)|=(x)>>1, \
	(x)|=(x)>>2, (x)|=(x)>>4, (x)|=(x)>>8, (x)|=(x)>>16, ++(x))

#define GIT_HASHSET_VAL_T void *

typedef uint32_t git_hashmap_iter_t;
#define GIT_HASHMAP_ITER_INIT 0

#define GIT_HASHMAP_STRUCT_MEMBERS(key_t, val_t) \
	uint32_t n_buckets, \
	         size, \
	         n_occupied, \
	         upper_bound; \
	uint32_t *flags; \
	key_t *keys; \
	val_t *vals;

#define GIT_HASHMAP_STRUCT(name, key_t, val_t) \
	typedef struct { \
		GIT_HASHMAP_STRUCT_MEMBERS(key_t, val_t) \
	} name;
#define GIT_HASHSET_STRUCT(name, key_t) \
	GIT_HASHMAP_STRUCT(name, key_t, void *)


#define GIT_HASHMAP__COMMON_PROTOTYPES(name, key_t, val_t) \
	extern uint32_t name##_size(name *h); \
	extern bool name##_contains(name *h, key_t key); \
	extern int name##_remove(name *h, key_t key); \
	extern void name##_clear(name *h); \
	extern void name##_dispose(name *h);

#define GIT_HASHMAP_PROTOTYPES(name, key_t, val_t) \
	GIT_HASHMAP__COMMON_PROTOTYPES(name, key_t, val_t) \
	extern int name##_get(val_t *out, name *h, key_t key); \
	extern int name##_put(name *h, key_t key, val_t val); \
	extern int name##_iterate(git_hashmap_iter_t *iter, key_t *key, val_t *val, name *h); \
	extern int name##_foreach(name *h, int (*cb)(key_t, val_t));

#define GIT_HASHSET_PROTOTYPES(name, key_t) \
	GIT_HASHMAP__COMMON_PROTOTYPES(name, key_t, GIT_HASHSET_VAL_T) \
	extern int name##_add(name *h, key_t key); \
	extern int name##_iterate(git_hashmap_iter_t *iter, key_t *key, name *h); \
	extern int name##_foreach(name *h, int (*cb)(key_t)); \


#define GIT_HASHMAP__COMMON_FUNCTIONS(name, is_map, scope, key_t, val_t, __hash_fn, __equal_fn) \
	GIT_UNUSED_FUNCTION scope uint32_t name##_size(name *h) \
	{ \
		return h->size; \
	} \
	GIT_INLINE(int) name##__idx(uint32_t *out, name *h, key_t key) \
	{ \
		if (h->n_buckets) { \
			uint32_t k, i, last, mask, step = 0; \
			GIT_ASSERT((h)->flags); \
			mask = h->n_buckets - 1; \
			k = __hash_fn(key); \
			i = k & mask; \
			last = i; \
			while (!GIT_HASHMAP_IS_EMPTY(h->flags, i) && \
			       (GIT_HASHMAP_IS_DELETE(h->flags, i) || !__equal_fn(h->keys[i], key))) { \
				i = (i + (++step)) & mask; \
				if (i == last) \
					return GIT_ENOTFOUND; \
			} \
			if (GIT_HASHMAP_IS_EITHER(h->flags, i)) \
				return GIT_ENOTFOUND; \
			*out = i; \
			return 0; \
		} \
		return GIT_ENOTFOUND; \
	} \
	GIT_UNUSED_FUNCTION scope bool name##_contains(name *h, key_t key) \
	{ \
		uint32_t idx; \
		return name##__idx(&idx, h, key) == 0; \
	} \
	GIT_INLINE(int) name##__remove_at_idx(name *h, uint32_t idx) \
	{ \
		if (idx < h->n_buckets && !GIT_HASHMAP_IS_EITHER(h->flags, idx)) { \
			GIT_HASHMAP_SET_DELETE_TRUE(h->flags, idx); \
			--h->size; \
			return 0; \
		} \
		return GIT_ENOTFOUND; \
	} \
	GIT_UNUSED_FUNCTION scope int name##_remove(name *h, key_t key) \
	{ \
		uint32_t idx; \
		int error; \
		if ((error = name##__idx(&idx, h, key)) == 0) \
			error = name##__remove_at_idx(h, idx); \
		return error; \
	} \
	GIT_INLINE(int) name##__resize(name *h, uint32_t new_n_buckets) \
	{ \
		/* This function uses 0.25*n_buckets bytes of working \
		 * space instead of [sizeof(key_t+val_t)+.25]*n_buckets. \
		 */ \
		double git_hashmap__upper_bound = 0.77; \
		uint32_t *new_flags = 0; \
		uint32_t j = 1; \
		{ \
			GIT_HASHMAP_ROUNDUP(new_n_buckets); \
			if (new_n_buckets < 4) \
				new_n_buckets = 4; \
			if (h->size >= (uint32_t)(new_n_buckets * git_hashmap__upper_bound + 0.5)) { \
				/* Requested size is too small */ \
				j = 0; \
			} else { \
				/* Shrink or expand; rehash */ \
				new_flags = git__reallocarray(NULL, GIT_HASHMAP_FLAGSIZE(new_n_buckets), sizeof(uint32_t)); \
				if (!new_flags) \
					return -1; \
				memset(new_flags, 0xaa, GIT_HASHMAP_FLAGSIZE(new_n_buckets) * sizeof(uint32_t)); \
				if (h->n_buckets < new_n_buckets) { \
					/* Expand */ \
					key_t *new_keys = git__reallocarray(h->keys, new_n_buckets, sizeof(key_t)); \
					if (!new_keys) { \
						git__free(new_flags); \
						return -1; \
					} \
					h->keys = new_keys; \
					if (is_map) { \
						val_t *new_vals = git__reallocarray(h->vals, new_n_buckets, sizeof(val_t)); \
						if (!new_vals) { \
							git__free(new_flags); \
							return -1; \
						} \
						h->vals = new_vals; \
					} \
				} \
			} \
		} \
		if (j) { \
			/* Rehashing is needed */ \
			for (j = 0; j != h->n_buckets; ++j) { \
				if (GIT_HASHMAP_IS_EITHER(h->flags, j) == 0) { \
					key_t key = h->keys[j]; \
					val_t val; \
					uint32_t new_mask; \
					new_mask = new_n_buckets - 1; \
					if (is_map) \
						val = h->vals[j]; \
					GIT_HASHMAP_SET_DELETE_TRUE(h->flags, j); \
					while (1) { \
						/* Kick-out process; sort of like in Cuckoo hashing */ \
						uint32_t k, i, step = 0; \
						k = __hash_fn(key); \
						i = k & new_mask; \
						while (!GIT_HASHMAP_IS_EMPTY(new_flags, i)) \
							i = (i + (++step)) & new_mask; \
						GIT_HASHMAP_SET_EMPTY_FALSE(new_flags, i); \
						if (i < h->n_buckets && GIT_HASHMAP_IS_EITHER(h->flags, i) == 0) { \
							/* Kick out the existing element */ \
							{ \
								key_t tmp = h->keys[i]; \
								h->keys[i] = key; \
								key = tmp; \
							} \
							if (is_map) { \
								val_t tmp = h->vals[i]; \
								h->vals[i] = val; \
								val = tmp; \
							} \
							/* Mark it as deleted in the old hash table */ \
							GIT_HASHMAP_SET_DELETE_TRUE(h->flags, i); \
						} else { \
							/* Write the element and jump out of the loop */ \
							h->keys[i] = key; \
							if (is_map) \
								h->vals[i] = val; \
							break; \
						} \
					} \
				} \
			} \
			if (h->n_buckets > new_n_buckets) { \
				/* Shrink the hash table */ \
				h->keys = git__reallocarray(h->keys, new_n_buckets, sizeof(key_t)); \
				if (is_map) \
					h->vals = git__reallocarray(h->vals, new_n_buckets, sizeof(val_t)); \
			} \
			/* free the working space */ \
			git__free(h->flags); \
			h->flags = new_flags; \
			h->n_buckets = new_n_buckets; \
			h->n_occupied = h->size; \
			h->upper_bound = (uint32_t)(h->n_buckets * git_hashmap__upper_bound + 0.5); \
		} \
		return 0; \
	} \
	GIT_INLINE(int) name##__put_idx(uint32_t *idx, bool *key_exists, name *h, key_t key) \
	{ \
		uint32_t x; \
		if (h->n_occupied >= h->upper_bound) { \
			/* Update the hash table */ \
			if (h->n_buckets > (h->size<<1)) { \
				/* Clear "deleted" elements */ \
				if (name##__resize(h, h->n_buckets - 1) < 0) \
					return -1; \
			} else if (name##__resize(h, h->n_buckets + 1) < 0) { \
				return -1; \
			} \
		} \
		GIT_ASSERT((h)->flags); \
		GIT_ASSERT((h)->keys); \
		/* TODO: to implement automatically shrinking; resize() already support shrinking */ \
		{ \
			uint32_t k, i, site, last, mask = h->n_buckets - 1, step = 0; \
			x = site = h->n_buckets; \
			k = __hash_fn(key); \
			i = k & mask; \
			if (GIT_HASHMAP_IS_EMPTY(h->flags, i)) { \
				/* for speed up */ \
				x = i; \
			} else { \
				last = i; \
				while (!GIT_HASHMAP_IS_EMPTY(h->flags, i) && (GIT_HASHMAP_IS_DELETE(h->flags, i) || !__equal_fn(h->keys[i], key))) { \
					if (GIT_HASHMAP_IS_DELETE(h->flags, i)) \
						site = i; \
					i = (i + (++step)) & mask; \
					if (i == last) { \
						x = site; \
						break; \
					} \
				} \
				if (x == h->n_buckets) { \
					if (GIT_HASHMAP_IS_EMPTY(h->flags, i) && site != h->n_buckets) \
						x = site; \
					else \
						x = i; \
				} \
			} \
		} \
		if (GIT_HASHMAP_IS_EMPTY(h->flags, x)) { \
			/* not present at all */ \
			h->keys[x] = key; \
			GIT_HASHMAP_SET_BOTH_FALSE(h->flags, x); \
			++h->size; \
			++h->n_occupied; \
			*key_exists = 1; \
		} else if (GIT_HASHMAP_IS_DELETE(h->flags, x)) { \
			/* deleted */ \
			h->keys[x] = key; \
			GIT_HASHMAP_SET_BOTH_FALSE(h->flags, x); \
			++h->size; \
			*key_exists = 1; \
		} else { \
			/* Don't touch h->keys[x] if present and not deleted */ \
			*key_exists = 0; \
		} \
		*idx = x; \
		return 0; \
	} \
	GIT_UNUSED_FUNCTION scope void name##_clear(name *h) \
	{ \
		if (h && h->flags) { \
			memset(h->flags, 0xaa, GIT_HASHMAP_FLAGSIZE(h->n_buckets) * sizeof(uint32_t)); \
			h->size = h->n_occupied = 0; \
		} \
	} \
	GIT_UNUSED_FUNCTION scope void name##_dispose(name *h) \
	{ \
		git__free(h->flags); \
		git__free(h->keys); \
		git__free(h->vals); \
		memset(h, 0, sizeof(name)); \
	}

#define GIT_HASHMAP_FUNCTIONS(name, scope, key_t, val_t, __hash_fn, __equal_fn) \
	GIT_HASHMAP__COMMON_FUNCTIONS(name, true, scope, key_t, val_t, __hash_fn, __equal_fn) \
	\
	GIT_UNUSED_FUNCTION scope int name##_get(val_t *out, name *h, key_t key) \
	{ \
		uint32_t idx; \
		int error; \
		if ((error = name##__idx(&idx, h, key)) == 0) \
			*out = (h)->vals[idx]; \
		return error; \
	} \
	GIT_UNUSED_FUNCTION scope int name##_put(name *h, key_t key, val_t val) \
	{ \
		uint32_t idx; \
		bool key_exists; \
		int error = name##__put_idx(&idx, &key_exists, h, key); \
		if (error) \
			return error; \
		GIT_ASSERT((h)->vals); \
		if (!key_exists)  \
			(h)->keys[idx] = key; \
		(h)->vals[idx] = val; \
		return 0; \
	} \
	GIT_UNUSED_FUNCTION scope int name##_iterate(git_hashmap_iter_t *iter, key_t *key, val_t *val, name *h) \
	{ \
		for (; *iter < h->n_buckets; (*iter)++) { \
			if (GIT_HASHMAP_IS_EITHER(h->flags, *iter)) \
				continue; \
			if (key) \
				*key = h->keys[*iter]; \
			if (val) \
				*val = h->vals[*iter]; \
			(*iter)++; \
			return 0; \
		} \
		return GIT_ITEROVER; \
	} \
	GIT_UNUSED_FUNCTION scope int name##_foreach(name *h, int (*cb)(key_t, val_t)) \
	{ \
		uint32_t idx = 0; \
		key_t key; \
		val_t val; \
		int ret; \
		while ((ret = name##_iterate(&idx, &key, &val, h)) == 0) { \
			if ((ret = cb(key, val)) != 0) \
				return ret; \
		} \
		return ret == GIT_ITEROVER ? 0 : ret; \
	}

#define GIT_HASHSET_FUNCTIONS(name, scope, key_t, __hash_fn, __equal_fn) \
	GIT_HASHMAP__COMMON_FUNCTIONS(name, false, scope, key_t, void *, __hash_fn, __equal_fn) \
	\
	GIT_UNUSED_FUNCTION scope int name##_add(name *h, key_t key) \
	{ \
		uint32_t idx; \
		bool key_exists; \
		int error = name##__put_idx(&idx, &key_exists, h, key); \
		if (error) \
			return error; \
		if (!key_exists) { \
			(h)->keys[idx] = key; \
		} \
		return 0; \
	} \
	GIT_UNUSED_FUNCTION scope int name##_iterate(git_hashmap_iter_t *iter, key_t *key, name *h) \
	{ \
		for (; *iter < h->n_buckets; (*iter)++) { \
			if (GIT_HASHMAP_IS_EITHER(h->flags, *iter)) \
				continue; \
			*key = h->keys[*iter]; \
			return 0; \
		} \
		return GIT_ITEROVER; \
	} \
	GIT_UNUSED_FUNCTION scope int name##_foreach(name *h, int (*cb)(key_t)) \
	{ \
		git_hashmap_iter_t iter = 0; \
		key_t key; \
		int ret; \
		while ((ret = name##_iterate(&iter, &key, h)) == 0) { \
			if ((ret = cb(key)) != 0) \
				return ret; \
		} \
		return ret == GIT_ITEROVER ? 0 : ret; \
	}


#define GIT_HASHSET_SETUP(name, key_t, __hash_fn, __equal_fn) \
	GIT_HASHSET_STRUCT(name, key_t) \
	GIT_HASHSET_FUNCTIONS(name, GIT_HASHMAP_INLINE, key_t, __hash_fn, __equal_fn)
#define GIT_HASHMAP_SETUP(name, key_t, val_t, __hash_fn, __equal_fn) \
	GIT_HASHMAP_STRUCT(name, key_t, val_t) \
	GIT_HASHMAP_FUNCTIONS(name, GIT_HASHMAP_INLINE, key_t, val_t, __hash_fn, __equal_fn)

#endif
