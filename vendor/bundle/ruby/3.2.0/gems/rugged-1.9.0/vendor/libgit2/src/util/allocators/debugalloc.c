/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "debugalloc.h"

static void *debugalloc__malloc(size_t len, const char *file, int line)
{
	unsigned char *ptr;
	size_t total = len + sizeof(size_t);

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	if (!len || (ptr = malloc(total)) == NULL)
		return NULL;

	memcpy(ptr, &len, sizeof(size_t));
	return ptr + sizeof(size_t);
}

static void *debugalloc__realloc(void *_ptr, size_t len, const char *file, int line)
{
	unsigned char *ptr = _ptr, *newptr;
	size_t original_len;
	size_t total = len + sizeof(size_t);

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	if (!len && !ptr)
		return NULL;

	if (!len) {
		free(ptr - sizeof(size_t));
		return NULL;
	}

	if ((newptr = malloc(total)) == NULL)
		return NULL;

	if (ptr) {
		memcpy(&original_len, ptr - sizeof(size_t), sizeof(size_t));
		memcpy(newptr + sizeof(size_t), ptr, min(len, original_len));

		memset(ptr - sizeof(size_t), 0xfd, original_len + sizeof(size_t));
		free(ptr - sizeof(size_t));
	}

	memcpy(newptr, &len, sizeof(size_t));
	return newptr + sizeof(size_t);
}

static void debugalloc__free(void *_ptr)
{
	unsigned char *ptr = _ptr;

	if (!ptr)
		return;

	free(ptr - sizeof(size_t));
}

int git_debugalloc_init_allocator(git_allocator *allocator)
{
	allocator->gmalloc = debugalloc__malloc;
	allocator->grealloc = debugalloc__realloc;
	allocator->gfree = debugalloc__free;
	return 0;
}
