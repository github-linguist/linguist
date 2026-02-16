/*
 * Copyright (C) the libgit2 contributors.  All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_pqueue_h__
#define INCLUDE_pqueue_h__

#include "git2_util.h"

#include "vector.h"

typedef git_vector git_pqueue;

enum {
	/* flag meaning: don't grow heap, keep highest values only */
	GIT_PQUEUE_FIXED_SIZE = (GIT_VECTOR_FLAG_MAX << 1)
};

/**
 * Initialize priority queue
 *
 * @param pq The priority queue struct to initialize
 * @param flags Flags (see above) to control queue behavior
 * @param init_size The initial queue size
 * @param cmp The entry priority comparison function
 * @return 0 on success, <0 on error
 */
extern int git_pqueue_init(
	git_pqueue *pq,
	uint32_t flags,
	size_t init_size,
	git_vector_cmp cmp);

#define git_pqueue_free  git_vector_dispose
#define git_pqueue_clear git_vector_clear
#define git_pqueue_size  git_vector_length
#define git_pqueue_get   git_vector_get
#define git_pqueue_reverse git_vector_reverse

/**
 * Insert a new item into the queue
 *
 * @param pq The priority queue
 * @param item Pointer to the item data
 * @return 0 on success, <0 on failure
 */
extern int git_pqueue_insert(git_pqueue *pq, void *item);

/**
 * Remove the top item in the priority queue
 *
 * @param pq The priority queue
 * @return item from heap on success, NULL if queue is empty
 */
extern void *git_pqueue_pop(git_pqueue *pq);

#endif
