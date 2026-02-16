/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_array_h__
#define INCLUDE_array_h__

#include "git2_util.h"

/*
 * Use this to declare a typesafe resizable array of items, a la:
 *
 *     git_array_t(int) my_ints = GIT_ARRAY_INIT;
 *     ...
 *     int *i = git_array_alloc(my_ints);
 *     GIT_ERROR_CHECK_ALLOC(i);
 *     ...
 *     git_array_clear(my_ints);
 *
 * You may also want to do things like:
 *
 *     typedef git_array_t(my_struct) my_struct_array_t;
 */
#define git_array_t(type) struct { type *ptr; size_t size, asize; }

#define GIT_ARRAY_INIT { NULL, 0, 0 }

#define git_array_init(a) \
	do { (a).size = (a).asize = 0; (a).ptr = NULL; } while (0)

#define git_array_init_to_size(a, desired) \
	do { (a).size = 0; (a).asize = desired; (a).ptr = git__calloc(desired, sizeof(*(a).ptr)); } while (0)

#define git_array_dispose(a) \
	do { git__free((a).ptr); } while (0)

#define git_array_clear(a) \
	do { git__free((a).ptr); git_array_init(a); } while (0)

#define GIT_ERROR_CHECK_ARRAY(a) GIT_ERROR_CHECK_ALLOC((a).ptr)

GIT_INLINE(void *) git_array__alloc(void *arr, size_t *size, size_t *asize, size_t item_size)
{
	size_t new_size;
	void *new_array;

	if (*size < *asize)
		return arr;

	if (*size < 8) {
		new_size = 8;
	} else {
		if (GIT_MULTIPLY_SIZET_OVERFLOW(&new_size, *asize, 3))
			goto on_oom;

		new_size /= 2;
	}

	if ((new_array = git__reallocarray(arr, new_size, item_size)) == NULL)
		goto on_oom;

	*asize = new_size;

	return new_array;

on_oom:
	git__free(arr);
	*size = 0;
	*asize = 0;
	return NULL;
}

#define git_array_alloc(a) \
	(((a).size < (a).asize || \
	 ((a).ptr = git_array__alloc((a).ptr, &(a).size, &(a).asize, sizeof(*(a).ptr))) != NULL) ? &(a).ptr[(a).size++] : (void *)NULL)

#define git_array_last(a) ((a).size ? &(a).ptr[(a).size - 1] : (void *)NULL)

#define git_array_pop(a) ((a).size ? &(a).ptr[--(a).size] : (void *)NULL)

#define git_array_get(a, i) (((i) < (a).size) ? &(a).ptr[(i)] : (void *)NULL)

#define git_array_size(a) (a).size

#define git_array_valid_index(a, i) ((i) < (a).size)

#define git_array_foreach(a, i, element) \
	for ((i) = 0; (i) < (a).size && ((element) = &(a).ptr[(i)]); (i)++)

typedef int (*git_array_compare_cb)(const void *, const void *);

GIT_INLINE(int) git_array__search(
	size_t *out,
	void *array_ptr,
	size_t item_size,
	size_t array_len,
	git_array_compare_cb compare,
	const void *key)
{
	size_t lim;
	unsigned char *part, *array = array_ptr, *base = array_ptr;
	int cmp = -1;

	for (lim = array_len; lim != 0; lim >>= 1) {
		part = base + (lim >> 1) * item_size;
		cmp = (*compare)(key, part);

		if (cmp == 0) {
			base = part;
			break;
		}
		if (cmp > 0) { /* key > p; take right partition */
			base = part + 1 * item_size;
			lim--;
		} /* else take left partition */
	}

	if (out)
		*out = (base - array) / item_size;

	return (cmp == 0) ? 0 : GIT_ENOTFOUND;
}

#define git_array_search(out, a, cmp, key) \
	git_array__search(out, (a).ptr, sizeof(*(a).ptr), (a).size, \
		(cmp), (key))

#endif
