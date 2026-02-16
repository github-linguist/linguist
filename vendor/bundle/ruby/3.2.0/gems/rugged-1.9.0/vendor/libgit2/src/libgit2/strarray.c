/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "util.h"

#include "common.h"
#include "strarray.h"

int git_strarray_copy(git_strarray *tgt, const git_strarray *src)
{
	size_t i;

	GIT_ASSERT_ARG(tgt);
	GIT_ASSERT_ARG(src);

	memset(tgt, 0, sizeof(*tgt));

	if (!src->count)
		return 0;

	tgt->strings = git__calloc(src->count, sizeof(char *));
	GIT_ERROR_CHECK_ALLOC(tgt->strings);

	for (i = 0; i < src->count; ++i) {
		if (!src->strings[i])
			continue;

		tgt->strings[tgt->count] = git__strdup(src->strings[i]);
		if (!tgt->strings[tgt->count]) {
			git_strarray_dispose(tgt);
			memset(tgt, 0, sizeof(*tgt));
			return -1;
		}

		tgt->count++;
	}

	return 0;
}

void git_strarray_dispose(git_strarray *array)
{
	size_t i;

	if (array == NULL)
		return;

	for (i = 0; i < array->count; ++i)
		git__free(array->strings[i]);

	git__free(array->strings);

	memset(array, 0, sizeof(*array));
}

#ifndef GIT_DEPRECATE_HARD
void git_strarray_free(git_strarray *array)
{
	git_strarray_dispose(array);
}
#endif
