/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_h__
#define INCLUDE_hash_h__

#include "git2_util.h"

#include "hash/sha.h"

typedef struct {
	void *data;
	size_t len;
} git_str_vec;

typedef enum {
	GIT_HASH_ALGORITHM_NONE = 0,
	GIT_HASH_ALGORITHM_SHA1,
	GIT_HASH_ALGORITHM_SHA256
} git_hash_algorithm_t;

#define GIT_HASH_MAX_SIZE GIT_HASH_SHA256_SIZE

typedef struct git_hash_ctx {
	union {
		git_hash_sha1_ctx sha1;
		git_hash_sha256_ctx sha256;
	} ctx;
	git_hash_algorithm_t algorithm;
} git_hash_ctx;

int git_hash_global_init(void);

int git_hash_ctx_init(git_hash_ctx *ctx, git_hash_algorithm_t algorithm);
void git_hash_ctx_cleanup(git_hash_ctx *ctx);

int git_hash_init(git_hash_ctx *c);
int git_hash_update(git_hash_ctx *c, const void *data, size_t len);
int git_hash_final(unsigned char *out, git_hash_ctx *c);

int git_hash_buf(unsigned char *out, const void *data, size_t len, git_hash_algorithm_t algorithm);
int git_hash_vec(unsigned char *out, git_str_vec *vec, size_t n, git_hash_algorithm_t algorithm);

int git_hash_fmt(char *out, unsigned char *hash, size_t hash_len);

GIT_INLINE(size_t) git_hash_size(git_hash_algorithm_t algorithm) {
	switch (algorithm) {
		case GIT_HASH_ALGORITHM_SHA1:
			return GIT_HASH_SHA1_SIZE;
		case GIT_HASH_ALGORITHM_SHA256:
			return GIT_HASH_SHA256_SIZE;
		default:
			return 0;
	}
}

#endif
