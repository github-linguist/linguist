/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_commit_list_h__
#define INCLUDE_commit_list_h__

#include "common.h"

#include "git2/oid.h"

#define PARENT1  (1 << 0)
#define PARENT2  (1 << 1)
#define RESULT   (1 << 2)
#define STALE    (1 << 3)
#define ALL_FLAGS (PARENT1 | PARENT2 | STALE | RESULT)

#define PARENTS_PER_COMMIT	2
#define COMMIT_ALLOC \
	(sizeof(git_commit_list_node) + PARENTS_PER_COMMIT * sizeof(git_commit_list_node *))

#define FLAG_BITS 4

typedef struct git_commit_list_node {
	git_oid oid;
	int64_t time;
	uint32_t generation;
	unsigned int seen:1,
			 uninteresting:1,
			 topo_delay:1,
			 parsed:1,
			 added:1,
			 flags : FLAG_BITS;

	uint16_t in_degree;
	uint16_t out_degree;

	struct git_commit_list_node **parents;
} git_commit_list_node;

typedef struct git_commit_list {
	git_commit_list_node *item;
	struct git_commit_list *next;
} git_commit_list;

git_commit_list_node *git_commit_list_alloc_node(git_revwalk *walk);
int git_commit_list_generation_cmp(const void *a, const void *b);
int git_commit_list_time_cmp(const void *a, const void *b);
void git_commit_list_free(git_commit_list **list_p);
git_commit_list *git_commit_list_create(git_commit_list_node *item, git_commit_list *next);
git_commit_list *git_commit_list_insert(git_commit_list_node *item, git_commit_list **list_p);
git_commit_list *git_commit_list_insert_by_date(git_commit_list_node *item, git_commit_list **list_p);
int git_commit_list_parse(git_revwalk *walk, git_commit_list_node *commit);
git_commit_list_node *git_commit_list_pop(git_commit_list **stack);

#endif
