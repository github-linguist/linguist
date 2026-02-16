/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_index_map_h__
#define INCLUDE_index_map_h__

#include "common.h"
#include "hashmap.h"

typedef struct {
	unsigned int ignore_case;
	GIT_HASHMAP_STRUCT_MEMBERS(git_index_entry *, git_index_entry *)
} git_index_entrymap;

#define GIT_INDEX_ENTRYMAP_INIT { 0 }

extern int git_index_entrymap_get(git_index_entry **out, git_index_entrymap *map, git_index_entry *e);
extern int git_index_entrymap_put(git_index_entrymap *map, git_index_entry *e);
extern int git_index_entrymap_remove(git_index_entrymap *map, git_index_entry *e);
extern int git_index_entrymap_resize(git_index_entrymap *map, size_t count);
extern void git_index_entrymap_swap(git_index_entrymap *a, git_index_entrymap *b);
extern void git_index_entrymap_clear(git_index_entrymap *map);
extern void git_index_entrymap_dispose(git_index_entrymap *map);

#endif
