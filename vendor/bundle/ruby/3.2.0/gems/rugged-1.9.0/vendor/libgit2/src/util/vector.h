/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_vector_h__
#define INCLUDE_vector_h__

#include "git2_util.h"

typedef int (*git_vector_cmp)(const void *, const void *);

enum {
	GIT_VECTOR_SORTED = (1u << 0),
	GIT_VECTOR_FLAG_MAX = (1u << 1)
};

typedef struct git_vector {
	size_t _alloc_size;
	git_vector_cmp _cmp;
	void **contents;
	size_t length;
	uint32_t flags;
} git_vector;

#define GIT_VECTOR_INIT {0}

GIT_WARN_UNUSED_RESULT int git_vector_init(
	git_vector *v, size_t initial_size, git_vector_cmp cmp);
void git_vector_dispose(git_vector *v);
void git_vector_dispose_deep(git_vector *v); /* free each entry and self */
void git_vector_clear(git_vector *v);
GIT_WARN_UNUSED_RESULT int git_vector_dup(
	git_vector *v, const git_vector *src, git_vector_cmp cmp);
void git_vector_swap(git_vector *a, git_vector *b);
int git_vector_size_hint(git_vector *v, size_t size_hint);

void **git_vector_detach(size_t *size, size_t *asize, git_vector *v);

void git_vector_sort(git_vector *v);

/** Linear search for matching entry using internal comparison function */
int git_vector_search(size_t *at_pos, const git_vector *v, const void *entry);

/** Linear search for matching entry using explicit comparison function */
int git_vector_search2(size_t *at_pos, const git_vector *v, git_vector_cmp cmp, const void *key);

/**
 * Binary search for matching entry using explicit comparison function that
 * returns position where item would go if not found.
 */
int git_vector_bsearch2(
	size_t *at_pos, git_vector *v, git_vector_cmp cmp, const void *key);

/** Binary search for matching entry using internal comparison function */
GIT_INLINE(int) git_vector_bsearch(size_t *at_pos, git_vector *v, const void *key)
{
	return git_vector_bsearch2(at_pos, v, v->_cmp, key);
}

GIT_INLINE(void *) git_vector_get(const git_vector *v, size_t position)
{
	return (position < v->length) ? v->contents[position] : NULL;
}

#define GIT_VECTOR_GET(V,I) ((I) < (V)->length ? (V)->contents[(I)] : NULL)

GIT_INLINE(size_t) git_vector_length(const git_vector *v)
{
	return v->length;
}

GIT_INLINE(void *) git_vector_last(const git_vector *v)
{
	return (v->length > 0) ? git_vector_get(v, v->length - 1) : NULL;
}

#define git_vector_foreach(v, iter, elem)	\
	for ((iter) = 0; (iter) < (v)->length && ((elem) = (v)->contents[(iter)], 1); (iter)++ )

#define git_vector_rforeach(v, iter, elem)	\
	for ((iter) = (v)->length - 1; (iter) < SIZE_MAX && ((elem) = (v)->contents[(iter)], 1); (iter)-- )

int git_vector_insert(git_vector *v, void *element);
int git_vector_insert_sorted(git_vector *v, void *element,
	int (*on_dup)(void **old, void *new));
int git_vector_remove(git_vector *v, size_t idx);
void git_vector_pop(git_vector *v);
void git_vector_uniq(git_vector *v, void  (*git_free_cb)(void *));

void git_vector_remove_matching(
	git_vector *v,
	int (*match)(const git_vector *v, size_t idx, void *payload),
	void *payload);

int git_vector_resize_to(git_vector *v, size_t new_length);
int git_vector_insert_null(git_vector *v, size_t idx, size_t insert_len);
int git_vector_remove_range(git_vector *v, size_t idx, size_t remove_len);

int git_vector_set(void **old, git_vector *v, size_t position, void *value);

/** Check if vector is sorted */
#define git_vector_is_sorted(V) (((V)->flags & GIT_VECTOR_SORTED) != 0)

/** Directly set sorted state of vector */
#define git_vector_set_sorted(V,S) do { \
	(V)->flags = (S) ? ((V)->flags | GIT_VECTOR_SORTED) : \
		((V)->flags & ~GIT_VECTOR_SORTED); } while (0)

/** Set the comparison function used for sorting the vector */
GIT_INLINE(void) git_vector_set_cmp(git_vector *v, git_vector_cmp cmp)
{
	if (cmp != v->_cmp) {
		v->_cmp = cmp;
		git_vector_set_sorted(v, 0);
	}
}

/* Just use this in tests, not for realz. returns -1 if not sorted */
int git_vector_verify_sorted(const git_vector *v);

/**
 * Reverse the vector in-place.
 */
void git_vector_reverse(git_vector *v);

#endif
