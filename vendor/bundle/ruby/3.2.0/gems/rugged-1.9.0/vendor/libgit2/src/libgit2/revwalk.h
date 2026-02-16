/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_revwalk_h__
#define INCLUDE_revwalk_h__

#include "common.h"

#include "git2/revwalk.h"
#include "commit_list.h"
#include "pqueue.h"
#include "pool.h"
#include "vector.h"
#include "hashmap_oid.h"

GIT_HASHMAP_OID_STRUCT(git_revwalk_oidmap, git_commit_list_node *);

struct git_revwalk {
	git_repository *repo;
	git_odb *odb;

	git_revwalk_oidmap commits;
	git_pool commit_pool;

	git_commit_list *iterator_topo;
	git_commit_list *iterator_rand;
	git_commit_list *iterator_reverse;
	git_pqueue iterator_time;

	int (*get_next)(git_commit_list_node **, git_revwalk *);
	int (*enqueue)(git_revwalk *, git_commit_list_node *);

	unsigned walking:1,
		first_parent: 1,
		did_hide: 1,
		did_push: 1,
		limited: 1;
	unsigned int sorting;

	/* the pushes and hides */
	git_commit_list *user_input;

	/* hide callback */
	git_revwalk_hide_cb hide_cb;
	void *hide_cb_payload;
};

git_commit_list_node *git_revwalk__commit_lookup(git_revwalk *walk, const git_oid *oid);

typedef struct {
	int uninteresting;
	int from_glob;
	int insert_by_date;
} git_revwalk__push_options;

#define GIT_REVWALK__PUSH_OPTIONS_INIT { 0 }

int git_revwalk__push_commit(git_revwalk *walk,
	const git_oid *oid,
	const git_revwalk__push_options *opts);

int git_revwalk__push_ref(git_revwalk *walk,
	const char *refname,
	const git_revwalk__push_options *opts);

int git_revwalk__push_glob(git_revwalk *walk,
	const char *glob,
	const git_revwalk__push_options *given_opts);

#endif
