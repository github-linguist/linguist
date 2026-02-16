/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/sys/hashsig.h"
#include "futils.h"
#include "util.h"

typedef uint32_t hashsig_t;
typedef uint64_t hashsig_state;

#define HASHSIG_SCALE 100

#define HASHSIG_MAX_RUN 80
#define HASHSIG_HASH_START	INT64_C(0x012345678ABCDEF0)
#define HASHSIG_HASH_SHIFT  5

#define HASHSIG_HASH_MIX(S,CH) \
	(S) = ((S) << HASHSIG_HASH_SHIFT) - (S) + (hashsig_state)(CH)

#define HASHSIG_HEAP_SIZE ((1 << 7) - 1)
#define HASHSIG_HEAP_MIN_SIZE 4

typedef int (*hashsig_cmp)(const void *a, const void *b, void *);

typedef struct {
	int size, asize;
	hashsig_cmp cmp;
	hashsig_t values[HASHSIG_HEAP_SIZE];
} hashsig_heap;

struct git_hashsig {
	hashsig_heap mins;
	hashsig_heap maxs;
	size_t lines;
	git_hashsig_option_t opt;
};

#define HEAP_LCHILD_OF(I) (((I)<<1)+1)
#define HEAP_RCHILD_OF(I) (((I)<<1)+2)
#define HEAP_PARENT_OF(I) (((I)-1)>>1)

static void hashsig_heap_init(hashsig_heap *h, hashsig_cmp cmp)
{
	h->size  = 0;
	h->asize = HASHSIG_HEAP_SIZE;
	h->cmp   = cmp;
}

static int hashsig_cmp_max(const void *a, const void *b, void *payload)
{
	hashsig_t av = *(const hashsig_t *)a, bv = *(const hashsig_t *)b;
	GIT_UNUSED(payload);
	return (av < bv) ? -1 : (av > bv) ? 1 : 0;
}

static int hashsig_cmp_min(const void *a, const void *b, void *payload)
{
	hashsig_t av = *(const hashsig_t *)a, bv = *(const hashsig_t *)b;
	GIT_UNUSED(payload);
	return (av > bv) ? -1 : (av < bv) ? 1 : 0;
}

static void hashsig_heap_up(hashsig_heap *h, int el)
{
	int parent_el = HEAP_PARENT_OF(el);

	while (el > 0 && h->cmp(&h->values[parent_el], &h->values[el], NULL) > 0) {
		hashsig_t t = h->values[el];
		h->values[el] = h->values[parent_el];
		h->values[parent_el] = t;

		el = parent_el;
		parent_el = HEAP_PARENT_OF(el);
	}
}

static void hashsig_heap_down(hashsig_heap *h, int el)
{
	hashsig_t v, lv, rv;

	/* 'el < h->size / 2' tests if el is bottom row of heap */

	while (el < h->size / 2) {
		int lel = HEAP_LCHILD_OF(el), rel = HEAP_RCHILD_OF(el), swapel;

		v  = h->values[el];
		lv = h->values[lel];
		rv = h->values[rel];

		if (h->cmp(&v, &lv, NULL) < 0 && h->cmp(&v, &rv, NULL) < 0)
			break;

		swapel = (h->cmp(&lv, &rv, NULL) < 0) ? lel : rel;

		h->values[el] = h->values[swapel];
		h->values[swapel] = v;

		el = swapel;
	}
}

static void hashsig_heap_sort(hashsig_heap *h)
{
	/* only need to do this at the end for signature comparison */
	git__qsort_r(h->values, h->size, sizeof(hashsig_t), h->cmp, NULL);
}

static void hashsig_heap_insert(hashsig_heap *h, hashsig_t val)
{
	/* if heap is not full, insert new element */
	if (h->size < h->asize) {
		h->values[h->size++] = val;
		hashsig_heap_up(h, h->size - 1);
	}

	/* if heap is full, pop top if new element should replace it */
	else if (h->cmp(&val, &h->values[0], NULL) > 0) {
		h->size--;
		h->values[0] = h->values[h->size];
		hashsig_heap_down(h, 0);
	}

}

typedef struct {
	int use_ignores;
	uint8_t ignore_ch[256];
} hashsig_in_progress;

static int hashsig_in_progress_init(
	hashsig_in_progress *prog, git_hashsig *sig)
{
	int i;

	/* no more than one can be set */
	GIT_ASSERT(!(sig->opt & GIT_HASHSIG_IGNORE_WHITESPACE) ||
		   !(sig->opt & GIT_HASHSIG_SMART_WHITESPACE));

	if (sig->opt & GIT_HASHSIG_IGNORE_WHITESPACE) {
		for (i = 0; i < 256; ++i)
			prog->ignore_ch[i] = git__isspace_nonlf(i);
		prog->use_ignores = 1;
	} else if (sig->opt & GIT_HASHSIG_SMART_WHITESPACE) {
		for (i = 0; i < 256; ++i)
			prog->ignore_ch[i] = git__isspace(i);
		prog->use_ignores = 1;
	} else {
		memset(prog, 0, sizeof(*prog));
	}

	return 0;
}

static int hashsig_add_hashes(
	git_hashsig *sig,
	const uint8_t *data,
	size_t size,
	hashsig_in_progress *prog)
{
	const uint8_t *scan = data, *end = data + size;
	hashsig_state state = HASHSIG_HASH_START;
	int use_ignores = prog->use_ignores, len;
	uint8_t ch;

	while (scan < end) {
		state = HASHSIG_HASH_START;

		for (len = 0; scan < end && len < HASHSIG_MAX_RUN; ) {
			ch = *scan;

			if (use_ignores)
				for (; scan < end && git__isspace_nonlf(ch); ch = *scan)
					++scan;
			else if (sig->opt &
					 (GIT_HASHSIG_IGNORE_WHITESPACE | GIT_HASHSIG_SMART_WHITESPACE))
				for (; scan < end && ch == '\r'; ch = *scan)
					++scan;

			/* peek at next character to decide what to do next */
			if (sig->opt & GIT_HASHSIG_SMART_WHITESPACE)
				use_ignores = (ch == '\n');

			if (scan >= end)
				break;
			++scan;

			/* check run terminator */
			if (ch == '\n' || ch == '\0') {
				sig->lines++;
				break;
			}

			++len;
			HASHSIG_HASH_MIX(state, ch);
		}

		if (len > 0) {
			hashsig_heap_insert(&sig->mins, (hashsig_t)state);
			hashsig_heap_insert(&sig->maxs, (hashsig_t)state);

			while (scan < end && (*scan == '\n' || !*scan))
				++scan;
		}
	}

	prog->use_ignores = use_ignores;

	return 0;
}

static int hashsig_finalize_hashes(git_hashsig *sig)
{
	if (sig->mins.size < HASHSIG_HEAP_MIN_SIZE &&
		!(sig->opt & GIT_HASHSIG_ALLOW_SMALL_FILES)) {
		git_error_set(GIT_ERROR_INVALID,
			"file too small for similarity signature calculation");
		return GIT_EBUFS;
	}

	hashsig_heap_sort(&sig->mins);
	hashsig_heap_sort(&sig->maxs);

	return 0;
}

static git_hashsig *hashsig_alloc(git_hashsig_option_t opts)
{
	git_hashsig *sig = git__calloc(1, sizeof(git_hashsig));
	if (!sig)
		return NULL;

	hashsig_heap_init(&sig->mins, hashsig_cmp_min);
	hashsig_heap_init(&sig->maxs, hashsig_cmp_max);
	sig->opt = opts;

	return sig;
}

int git_hashsig_create(
	git_hashsig **out,
	const char *buf,
	size_t buflen,
	git_hashsig_option_t opts)
{
	int error;
	hashsig_in_progress prog;
	git_hashsig *sig = hashsig_alloc(opts);
	GIT_ERROR_CHECK_ALLOC(sig);

	if ((error = hashsig_in_progress_init(&prog, sig)) < 0)
		return error;

	error = hashsig_add_hashes(sig, (const uint8_t *)buf, buflen, &prog);

	if (!error)
		error = hashsig_finalize_hashes(sig);

	if (!error)
		*out = sig;
	else
		git_hashsig_free(sig);

	return error;
}

int git_hashsig_create_fromfile(
	git_hashsig **out,
	const char *path,
	git_hashsig_option_t opts)
{
	uint8_t buf[0x1000];
	ssize_t buflen = 0;
	int error = 0, fd;
	hashsig_in_progress prog;
	git_hashsig *sig = hashsig_alloc(opts);
	GIT_ERROR_CHECK_ALLOC(sig);

	if ((fd = git_futils_open_ro(path)) < 0) {
		git__free(sig);
		return fd;
	}

	if ((error = hashsig_in_progress_init(&prog, sig)) < 0) {
		p_close(fd);
		return error;
	}

	while (!error) {
		if ((buflen = p_read(fd, buf, sizeof(buf))) <= 0) {
			if ((error = (int)buflen) < 0)
				git_error_set(GIT_ERROR_OS,
					"read error on '%s' calculating similarity hashes", path);
			break;
		}

		error = hashsig_add_hashes(sig, buf, buflen, &prog);
	}

	p_close(fd);

	if (!error)
		error = hashsig_finalize_hashes(sig);

	if (!error)
		*out = sig;
	else
		git_hashsig_free(sig);

	return error;
}

void git_hashsig_free(git_hashsig *sig)
{
	git__free(sig);
}

static int hashsig_heap_compare(const hashsig_heap *a, const hashsig_heap *b)
{
	int matches = 0, i, j, cmp;

	GIT_ASSERT_WITH_RETVAL(a->cmp == b->cmp, 0);

	/* hash heaps are sorted - just look for overlap vs total */

	for (i = 0, j = 0; i < a->size && j < b->size; ) {
		cmp = a->cmp(&a->values[i], &b->values[j], NULL);

		if (cmp < 0)
			++i;
		else if (cmp > 0)
			++j;
		else {
			++i; ++j; ++matches;
		}
	}

	return HASHSIG_SCALE * (matches * 2) / (a->size + b->size);
}

int git_hashsig_compare(const git_hashsig *a, const git_hashsig *b)
{
	/* if we have no elements in either file then each file is either
	 * empty or blank.  if we're ignoring whitespace then the files are
	 * similar, otherwise they're dissimilar.
	 */
	if (a->mins.size == 0 && b->mins.size == 0) {
		if ((!a->lines && !b->lines) ||
			(a->opt & GIT_HASHSIG_IGNORE_WHITESPACE))
			return HASHSIG_SCALE;
		else
			return 0;
	}

	/* if we have fewer than the maximum number of elements, then just use
	 * one array since the two arrays will be the same
	 */
	if (a->mins.size < HASHSIG_HEAP_SIZE) {
		return hashsig_heap_compare(&a->mins, &b->mins);
	} else {
		int mins, maxs;

		if ((mins = hashsig_heap_compare(&a->mins, &b->mins)) < 0)
			return mins;
		if ((maxs = hashsig_heap_compare(&a->maxs, &b->maxs)) < 0)
			return maxs;

		return (mins + maxs) / 2;
	}
}
