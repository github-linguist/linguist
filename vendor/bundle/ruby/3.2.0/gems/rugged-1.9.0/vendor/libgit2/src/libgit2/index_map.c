/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "hashmap.h"
#include "index_map.h"

typedef git_index_entrymap git_index_entrymap_default;
typedef git_index_entrymap git_index_entrymap_icase;

/* This is __ac_X31_hash_string but with tolower and it takes the entry's stage into account */
GIT_INLINE(uint32_t) git_index_entrymap_hash(const git_index_entry *e)
{
	const char *s = e->path;
	uint32_t h = (uint32_t)git__tolower(*s);
	if (h) {
		for (++s ; *s; ++s)
			h = (h << 5) - h + (uint32_t)git__tolower(*s);
	}
	return h + GIT_INDEX_ENTRY_STAGE(e);
}

#define git_index_entrymap_equal_default(a, b) (GIT_INDEX_ENTRY_STAGE(a) == GIT_INDEX_ENTRY_STAGE(b) && strcmp(a->path, b->path) == 0)
#define git_index_entrymap_equal_icase(a, b) (GIT_INDEX_ENTRY_STAGE(a) == GIT_INDEX_ENTRY_STAGE(b) && strcasecmp(a->path, b->path) == 0)

GIT_HASHMAP_FUNCTIONS(git_index_entrymap_default, GIT_HASHMAP_INLINE, git_index_entry *, git_index_entry *, git_index_entrymap_hash, git_index_entrymap_equal_default)
GIT_HASHMAP_FUNCTIONS(git_index_entrymap_icase, GIT_HASHMAP_INLINE, git_index_entry *, git_index_entry *, git_index_entrymap_hash, git_index_entrymap_equal_icase)

int git_index_entrymap_put(git_index_entrymap *map, git_index_entry *e)
{
	if (map->ignore_case)
		return git_index_entrymap_icase_put((git_index_entrymap_icase *)map, e, e);
	else
		return git_index_entrymap_default_put((git_index_entrymap_default *)map, e, e);
}

int git_index_entrymap_get(git_index_entry **out, git_index_entrymap *map, git_index_entry *e)
{
	if (map->ignore_case)
		return git_index_entrymap_icase_get(out, (git_index_entrymap_icase *)map, e);
	else
		return git_index_entrymap_default_get(out, (git_index_entrymap_default *)map, e);
}

int git_index_entrymap_remove(git_index_entrymap *map, git_index_entry *e)
{
	if (map->ignore_case)
		return git_index_entrymap_icase_remove((git_index_entrymap_icase *)map, e);
	else
		return git_index_entrymap_default_remove((git_index_entrymap_default *)map, e);
}

int git_index_entrymap_resize(git_index_entrymap *map, size_t count)
{
	if (count > UINT32_MAX) {
		git_error_set(GIT_ERROR_INDEX, "index map is out of bounds");
		return -1;
	}

	if (map->ignore_case)
		return git_index_entrymap_icase__resize((git_index_entrymap_icase *)map, (uint32_t)count);
	else
		return git_index_entrymap_default__resize((git_index_entrymap_default *)map, (uint32_t)count);
}

void git_index_entrymap_swap(git_index_entrymap *a, git_index_entrymap *b)
{
	git_index_entrymap t;

	if (a != b) {
		memcpy(&t, a, sizeof(t));
		memcpy(a, b, sizeof(t));
		memcpy(b, &t, sizeof(t));
	}
}

void git_index_entrymap_clear(git_index_entrymap *map)
{
	if (map->ignore_case)
		git_index_entrymap_icase_clear((git_index_entrymap_icase *)map);
	else
		git_index_entrymap_default_clear((git_index_entrymap_default *)map);
}

void git_index_entrymap_dispose(git_index_entrymap *map)
{
	if (map->ignore_case)
		git_index_entrymap_icase_dispose((git_index_entrymap_icase *)map);
	else
		git_index_entrymap_default_dispose((git_index_entrymap_default *)map);
}
