/*
 * Copyright (C) 2009-2012 the libgit2 contributors
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "hash.h"

#if defined(PPC_SHA1)
# include "ppc/sha1.h"
#else
# include "sha1.h"
#endif

struct git_hash_ctx {
	SHA_CTX c;
};

git_hash_ctx *git_hash_new_ctx(void)
{
	git_hash_ctx *ctx = git__malloc(sizeof(*ctx));

	if (!ctx)
		return NULL;

	SHA1_Init(&ctx->c);

	return ctx;
}

void git_hash_free_ctx(git_hash_ctx *ctx)
{
	git__free(ctx);
}

void git_hash_init(git_hash_ctx *ctx)
{
	assert(ctx);
	SHA1_Init(&ctx->c);
}

void git_hash_update(git_hash_ctx *ctx, const void *data, size_t len)
{
	assert(ctx);
	SHA1_Update(&ctx->c, data, len);
}

void git_hash_final(git_oid *out, git_hash_ctx *ctx)
{
	assert(ctx);
	SHA1_Final(out->id, &ctx->c);
}

void git_hash_buf(git_oid *out, const void *data, size_t len)
{
	SHA_CTX c;

	SHA1_Init(&c);
	SHA1_Update(&c, data, len);
	SHA1_Final(out->id, &c);
}

void git_hash_vec(git_oid *out, git_buf_vec *vec, size_t n)
{
	SHA_CTX c;
	size_t i;

	SHA1_Init(&c);
	for (i = 0; i < n; i++)
		SHA1_Update(&c, vec[i].data, vec[i].len);
	SHA1_Final(out->id, &c);
}
