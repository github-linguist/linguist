/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "mbedtls.h"

#ifdef GIT_SHA1_MBEDTLS

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
	if (ctx)
		mbedtls_sha1_free(&ctx->c);
}

int git_hash_sha1_init(git_hash_sha1_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	mbedtls_sha1_init(&ctx->c);
	mbedtls_sha1_starts(&ctx->c);
	return 0;
}

int git_hash_sha1_update(git_hash_sha1_ctx *ctx, const void *data, size_t len)
{
	GIT_ASSERT_ARG(ctx);
	mbedtls_sha1_update(&ctx->c, data, len);
	return 0;
}

int git_hash_sha1_final(unsigned char *out, git_hash_sha1_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	mbedtls_sha1_finish(&ctx->c, out);
	return 0;
}

#endif

#ifdef GIT_SHA256_MBEDTLS

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
	if (ctx)
		mbedtls_sha256_free(&ctx->c);
}

int git_hash_sha256_init(git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	mbedtls_sha256_init(&ctx->c);
	mbedtls_sha256_starts(&ctx->c, 0);
	return 0;
}

int git_hash_sha256_update(git_hash_sha256_ctx *ctx, const void *data, size_t len)
{
	GIT_ASSERT_ARG(ctx);
	mbedtls_sha256_update(&ctx->c, data, len);
	return 0;
}

int git_hash_sha256_final(unsigned char *out, git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	mbedtls_sha256_finish(&ctx->c, out);
	return 0;
}

#endif
