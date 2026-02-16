/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_rand_h__
#define INCLUDE_rand_h__

#include "git2_util.h"

/**
 * Initialize the random number generation subsystem.  This will
 * seed the random number generator with the system's entropy pool,
 * if available, and will fall back to the current time and
 * system information if not.
 */
int git_rand_global_init(void);

/**
 * Seed the pseudo-random number generator.  This is not needed to be
 * called; the PRNG is seeded by `git_rand_global_init`, but it may
 * be useful for testing.  When the same seed is specified, the same
 * sequence of random numbers from `git_rand_next` is emitted.
 *
 * @param seed the seed to use
 */
void git_rand_seed(uint64_t seed);

/**
 * Get the next pseudo-random number in the sequence.
 *
 * @return a 64-bit pseudo-random number
 */
uint64_t git_rand_next(void);

#endif
