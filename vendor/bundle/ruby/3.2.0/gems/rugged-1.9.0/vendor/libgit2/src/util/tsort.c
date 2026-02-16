/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"

/**
 * An array-of-pointers implementation of Python's Timsort
 * Based on code by Christopher Swenson under the MIT license
 *
 * Copyright (c) 2010 Christopher Swenson
 * Copyright (c) 2011 Vicent Marti
 */

#ifndef MAX
#	define MAX(x,y) (((x) > (y) ? (x) : (y)))
#endif

#ifndef MIN
#	define MIN(x,y) (((x) < (y) ? (x) : (y)))
#endif

static int binsearch(
	void **dst, const void *x, size_t size, git__sort_r_cmp cmp, void *payload)
{
	int l, c, r;
	void *lx, *cx;

	l = 0;
	r = (int)size - 1;
	c = r >> 1;
	lx = dst[l];

	/* check for beginning conditions */
	if (cmp(x, lx, payload) < 0)
		return 0;

	else if (cmp(x, lx, payload) == 0) {
		int i = 1;
		while (cmp(x, dst[i], payload) == 0)
			i++;
		return i;
	}

	/* guaranteed not to be >= rx */
	cx = dst[c];
	while (1) {
		const int val = cmp(x, cx, payload);
		if (val < 0) {
			if (c - l <= 1) return c;
			r = c;
		} else if (val > 0) {
			if (r - c <= 1) return c + 1;
			l = c;
			lx = cx;
		} else {
			do {
				cx = dst[++c];
			} while (cmp(x, cx, payload) == 0);
			return c;
		}
		c = l + ((r - l) >> 1);
		cx = dst[c];
	}
}

/* Binary insertion sort, but knowing that the first "start" entries are sorted. Used in timsort. */
static void bisort(
	void **dst, size_t start, size_t size, git__sort_r_cmp cmp, void *payload)
{
	size_t i;
	void *x;
	int location;

	for (i = start; i < size; i++) {
		int j;
		/* If this entry is already correct, just move along */
		if (cmp(dst[i - 1], dst[i], payload) <= 0)
			continue;

		/* Else we need to find the right place, shift everything over, and squeeze in */
		x = dst[i];
		location = binsearch(dst, x, i, cmp, payload);
		for (j = (int)i - 1; j >= location; j--) {
			dst[j + 1] = dst[j];
		}
		dst[location] = x;
	}
}


/* timsort implementation, based on timsort.txt */
struct tsort_run {
	ssize_t start;
	ssize_t length;
};

struct tsort_store {
	size_t alloc;
	git__sort_r_cmp cmp;
	void *payload;
	void **storage;
};

static void reverse_elements(void **dst, ssize_t start, ssize_t end)
{
	while (start < end) {
		void *tmp = dst[start];
		dst[start] = dst[end];
		dst[end] = tmp;

		start++;
		end--;
	}
}

static ssize_t count_run(
	void **dst, ssize_t start, ssize_t size, struct tsort_store *store)
{
	ssize_t curr = start + 2;

	if (size - start == 1)
		return 1;

	if (start >= size - 2) {
		if (store->cmp(dst[size - 2], dst[size - 1], store->payload) > 0) {
			void *tmp = dst[size - 1];
			dst[size - 1] = dst[size - 2];
			dst[size - 2] = tmp;
		}

		return 2;
	}

	if (store->cmp(dst[start], dst[start + 1], store->payload) <= 0) {
		while (curr < size - 1 &&
				store->cmp(dst[curr - 1], dst[curr], store->payload) <= 0)
			curr++;

		return curr - start;
	} else {
		while (curr < size - 1 &&
				store->cmp(dst[curr - 1], dst[curr], store->payload) > 0)
			curr++;

		/* reverse in-place */
		reverse_elements(dst, start, curr - 1);
		return curr - start;
	}
}

static size_t compute_minrun(size_t n)
{
	int r = 0;
	while (n >= 64) {
		r |= n & 1;
		n >>= 1;
	}
	return n + r;
}

static int check_invariant(struct tsort_run *stack, ssize_t stack_curr)
{
	if (stack_curr < 2)
		return 1;

	else if (stack_curr == 2) {
		const ssize_t A = stack[stack_curr - 2].length;
		const ssize_t B = stack[stack_curr - 1].length;
		return (A > B);
	} else {
		const ssize_t A = stack[stack_curr - 3].length;
		const ssize_t B = stack[stack_curr - 2].length;
		const ssize_t C = stack[stack_curr - 1].length;
		return !((A <= B + C) || (B <= C));
	}
}

static int resize(struct tsort_store *store, size_t new_size)
{
	if (store->alloc < new_size) {
		void **tempstore;

		tempstore = git__reallocarray(store->storage, new_size, sizeof(void *));

		/**
		 * Do not propagate on OOM; this will abort the sort and
		 * leave the array unsorted, but no error code will be
		 * raised
		 */
		if (tempstore == NULL)
			return -1;

		store->storage = tempstore;
		store->alloc = new_size;
	}

	return 0;
}

static void merge(void **dst, const struct tsort_run *stack, ssize_t stack_curr, struct tsort_store *store)
{
	const ssize_t A = stack[stack_curr - 2].length;
	const ssize_t B = stack[stack_curr - 1].length;
	const ssize_t curr = stack[stack_curr - 2].start;

	void **storage;
	ssize_t i, j, k;

	if (resize(store, MIN(A, B)) < 0)
		return;

	storage = store->storage;

	/* left merge */
	if (A < B) {
		memcpy(storage, &dst[curr], A * sizeof(void *));
		i = 0;
		j = curr + A;

		for (k = curr; k < curr + A + B; k++) {
			if ((i < A) && (j < curr + A + B)) {
				if (store->cmp(storage[i], dst[j], store->payload) <= 0)
					dst[k] = storage[i++];
				else
					dst[k] = dst[j++];
			} else if (i < A) {
				dst[k] = storage[i++];
			} else
				dst[k] = dst[j++];
		}
	} else {
		memcpy(storage, &dst[curr + A], B * sizeof(void *));
		i = B - 1;
		j = curr + A - 1;

		for (k = curr + A + B - 1; k >= curr; k--) {
			if ((i >= 0) && (j >= curr)) {
				if (store->cmp(dst[j], storage[i], store->payload) > 0)
					dst[k] = dst[j--];
				else
					dst[k] = storage[i--];
			} else if (i >= 0)
				dst[k] = storage[i--];
			else
				dst[k] = dst[j--];
		}
	}
}

static ssize_t collapse(void **dst, struct tsort_run *stack, ssize_t stack_curr, struct tsort_store *store, ssize_t size)
{
	ssize_t A, B, C;

	while (1) {
		/* if the stack only has one thing on it, we are done with the collapse */
		if (stack_curr <= 1)
			break;

		/* if this is the last merge, just do it */
		if ((stack_curr == 2) && (stack[0].length + stack[1].length == size)) {
			merge(dst, stack, stack_curr, store);
			stack[0].length += stack[1].length;
			stack_curr--;
			break;
		}

		/* check if the invariant is off for a stack of 2 elements */
		else if ((stack_curr == 2) && (stack[0].length <= stack[1].length)) {
			merge(dst, stack, stack_curr, store);
			stack[0].length += stack[1].length;
			stack_curr--;
			break;
		}
		else if (stack_curr == 2)
			break;

		A = stack[stack_curr - 3].length;
		B = stack[stack_curr - 2].length;
		C = stack[stack_curr - 1].length;

		/* check first invariant */
		if (A <= B + C) {
			if (A < C) {
				merge(dst, stack, stack_curr - 1, store);
				stack[stack_curr - 3].length += stack[stack_curr - 2].length;
				stack[stack_curr - 2] = stack[stack_curr - 1];
				stack_curr--;
			} else {
				merge(dst, stack, stack_curr, store);
				stack[stack_curr - 2].length += stack[stack_curr - 1].length;
				stack_curr--;
			}
		} else if (B <= C) {
			merge(dst, stack, stack_curr, store);
			stack[stack_curr - 2].length += stack[stack_curr - 1].length;
			stack_curr--;
		} else
			break;
	}

	return stack_curr;
}

#define PUSH_NEXT() do {\
	len = count_run(dst, curr, size, store);\
	run = minrun;\
	if (run > (ssize_t)size - curr) run = size - curr;\
	if (run > len) {\
		bisort(&dst[curr], len, run, cmp, payload);\
		len = run;\
	}\
	run_stack[stack_curr].start = curr;\
	run_stack[stack_curr++].length = len;\
	curr += len;\
	if (curr == (ssize_t)size) {\
		/* finish up */ \
		while (stack_curr > 1) { \
			merge(dst, run_stack, stack_curr, store); \
			run_stack[stack_curr - 2].length += run_stack[stack_curr - 1].length; \
			stack_curr--; \
		} \
		if (store->storage != NULL) {\
			git__free(store->storage);\
			store->storage = NULL;\
		}\
		return;\
	}\
}\
while (0)

void git__tsort_r(
	void **dst, size_t size, git__sort_r_cmp cmp, void *payload)
{
	struct tsort_store _store, *store = &_store;
	struct tsort_run run_stack[128];

	ssize_t stack_curr = 0;
	ssize_t len, run;
	ssize_t curr = 0;
	ssize_t minrun;

	if (size < 64) {
		bisort(dst, 1, size, cmp, payload);
		return;
	}

	/* compute the minimum run length */
	minrun = (ssize_t)compute_minrun(size);

	/* temporary storage for merges */
	store->alloc = 0;
	store->storage = NULL;
	store->cmp = cmp;
	store->payload = payload;

	PUSH_NEXT();
	PUSH_NEXT();
	PUSH_NEXT();

	while (1) {
		if (!check_invariant(run_stack, stack_curr)) {
			stack_curr = collapse(dst, run_stack, stack_curr, store, size);
			continue;
		}

		PUSH_NEXT();
	}
}

static int tsort_r_cmp(const void *a, const void *b, void *payload)
{
	return ((git__tsort_cmp)payload)(a, b);
}

void git__tsort(void **dst, size_t size, git__tsort_cmp cmp)
{
	git__tsort_r(dst, size, tsort_r_cmp, cmp);
}
