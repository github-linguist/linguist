/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "builtin.h"

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
	if (SHA256Reset(&ctx->c)) {
		git_error_set(GIT_ERROR_SHA, "SHA256 error");
		return -1;
	}
	return 0;
}

int git_hash_sha256_update(git_hash_sha256_ctx *ctx, const void *data, size_t len)
{
	GIT_ASSERT_ARG(ctx);
	if (SHA256Input(&ctx->c, data, len)) {
		git_error_set(GIT_ERROR_SHA, "SHA256 error");
		return -1;
	}
	return 0;
}

int git_hash_sha256_final(unsigned char *out, git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	if (SHA256Result(&ctx->c, out)) {
		git_error_set(GIT_ERROR_SHA, "SHA256 error");
		return -1;
	}
	return 0;
}
