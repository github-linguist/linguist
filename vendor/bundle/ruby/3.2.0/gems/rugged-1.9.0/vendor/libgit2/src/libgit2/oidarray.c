/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "oidarray.h"

#include "git2/oidarray.h"
#include "array.h"

void git_oidarray_dispose(git_oidarray *arr)
{
	git__free(arr->ids);
}

void git_oidarray__from_array(git_oidarray *out, const git_array_oid_t *array)
{
	out->count = array->size;
	out->ids = array->ptr;
}

void git_oidarray__to_array(git_array_oid_t *out, const git_oidarray *array)
{
	out->ptr = array->ids;
	out->size = array->count;
	out->asize = array->count;
}

void git_oidarray__reverse(git_oidarray *arr)
{
	size_t i;
	git_oid tmp;

	for (i = 0; i < arr->count / 2; i++) {
		git_oid_cpy(&tmp, &arr->ids[i]);
		git_oid_cpy(&arr->ids[i], &arr->ids[(arr->count-1)-i]);
		git_oid_cpy(&arr->ids[(arr->count-1)-i], &tmp);
	}
}

int git_oidarray__add(git_array_oid_t *arr, git_oid *id)
{
	git_oid *add, *iter;
	size_t i;

	git_array_foreach(*arr, i, iter) {
		if (git_oid_cmp(iter, id) == 0)
			return 0;
	}

	if ((add = git_array_alloc(*arr)) == NULL)
		return -1;

	git_oid_cpy(add, id);
	return 0;
}

bool git_oidarray__remove(git_array_oid_t *arr, git_oid *id)
{
	bool found = false;
	size_t remain, i;
	git_oid *iter;

	git_array_foreach(*arr, i, iter) {
		if (git_oid_cmp(iter, id) == 0) {
			arr->size--;
			remain = arr->size - i;

			if (remain > 0)
				memmove(&arr->ptr[i], &arr->ptr[i+1], remain * sizeof(git_oid));

			found = true;
			break;
		}
	}

	return found;
}

#ifndef GIT_DEPRECATE_HARD

void git_oidarray_free(git_oidarray *arr)
{
	git_oidarray_dispose(arr);
}

#endif
