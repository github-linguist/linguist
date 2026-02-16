/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common_crypto.h"

#define CC_LONG_MAX ((CC_LONG)-1)

#ifdef GIT_SHA1_COMMON_CRYPTO

int git_hash_sha1_global_init(void)
{
	return 0;
}

int git_hash_sha1_ctx_init(git_hash_sha1_ctx *ctx)
{
	return git_hash_sha1_init(ctx);
}

void git_hash_sha1_ctx_cleanup(git_hash_sha1_ctx *ctx)
{
	GIT_UNUSED(ctx);
}

int git_hash_sha1_init(git_hash_sha1_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	CC_SHA1_Init(&ctx->c);
	return 0;
}

int git_hash_sha1_update(git_hash_sha1_ctx *ctx, const void *_data, size_t len)
{
	const unsigned char *data = _data;

	GIT_ASSERT_ARG(ctx);

	while (len > 0) {
		CC_LONG chunk = (len > CC_LONG_MAX) ? CC_LONG_MAX : (CC_LONG)len;

		CC_SHA1_Update(&ctx->c, data, chunk);

		data += chunk;
		len -= chunk;
	}

	return 0;
}

int git_hash_sha1_final(unsigned char *out, git_hash_sha1_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	CC_SHA1_Final(out, &ctx->c);
	return 0;
}

#endif

#ifdef GIT_SHA256_COMMON_CRYPTO

int git_hash_sha256_global_init(void)
{
	return 0;
}

int git_hash_sha256_ctx_init(git_hash_sha256_ctx *ctx)
{
	return git_hash_sha256_init(ctx);
}

void git_hash_sha256_ctx_cleanup(git_hash_sha256_ctx *ctx)
{
	GIT_UNUSED(ctx);
}

int git_hash_sha256_init(git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	CC_SHA256_Init(&ctx->c);
	return 0;
}

int git_hash_sha256_update(git_hash_sha256_ctx *ctx, const void *_data, size_t len)
{
	const unsigned char *data = _data;

	GIT_ASSERT_ARG(ctx);

	while (len > 0) {
		CC_LONG chunk = (len > CC_LONG_MAX) ? CC_LONG_MAX : (CC_LONG)len;

		CC_SHA256_Update(&ctx->c, data, chunk);

		data += chunk;
		len -= chunk;
	}

	return 0;
}

int git_hash_sha256_final(unsigned char *out, git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	CC_SHA256_Final(out, &ctx->c);
	return 0;
}

#endif
