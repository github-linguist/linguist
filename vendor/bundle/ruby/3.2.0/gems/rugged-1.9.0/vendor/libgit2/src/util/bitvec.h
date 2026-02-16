/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_bitvec_h__
#define INCLUDE_bitvec_h__

#include "common.h"

/*
 * This is a silly little fixed length bit vector type that will store
 * vectors of 64 bits or less directly in the structure and allocate
 * memory for vectors longer than 64 bits.  You can use the two versions
 * transparently through the API and avoid heap allocation completely when
 * using a short bit vector as a result.
 */
typedef struct {
	size_t length;
	union {
		uint64_t *words;
		uint64_t bits;
	} u;
} git_bitvec;

GIT_INLINE(int) git_bitvec_init(git_bitvec *bv, size_t capacity)
{
	memset(bv, 0x0, sizeof(*bv));

	if (capacity >= 64) {
		bv->length = (capacity / 64) + 1;
		bv->u.words = git__calloc(bv->length, sizeof(uint64_t));
		if (!bv->u.words)
			return -1;
	}

	return 0;
}

#define GIT_BITVEC_MASK(BIT) ((uint64_t)1 << (BIT % 64))
#define GIT_BITVEC_WORD(BV, BIT) (BV->length ? &BV->u.words[BIT / 64] : &BV->u.bits)

GIT_INLINE(void) git_bitvec_set(git_bitvec *bv, size_t bit, bool on)
{
	uint64_t *word = GIT_BITVEC_WORD(bv, bit);
	uint64_t mask = GIT_BITVEC_MASK(bit);

	if (on)
		*word |= mask;
	else
		*word &= ~mask;
}

GIT_INLINE(bool) git_bitvec_get(git_bitvec *bv, size_t bit)
{
	uint64_t *word = GIT_BITVEC_WORD(bv, bit);
	return (*word & GIT_BITVEC_MASK(bit)) != 0;
}

GIT_INLINE(void) git_bitvec_clear(git_bitvec *bv)
{
	if (!bv->length)
		bv->u.bits = 0;
	else
		memset(bv->u.words, 0x0, bv->length * sizeof(uint64_t));
}

GIT_INLINE(void) git_bitvec_free(git_bitvec *bv)
{
	if (bv->length)
		git__free(bv->u.words);
}

#endif
