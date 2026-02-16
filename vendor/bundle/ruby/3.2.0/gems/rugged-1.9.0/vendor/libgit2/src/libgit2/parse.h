/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_parse_h__
#define INCLUDE_parse_h__

#include "common.h"

typedef struct {
	/* Original content buffer */
	const char *content;
	size_t content_len;

	/* The remaining (unparsed) buffer */
	const char *remain;
	size_t remain_len;

	const char *line;
	size_t line_len;
	size_t line_num;
} git_parse_ctx;

#define GIT_PARSE_CTX_INIT { 0 }

int git_parse_ctx_init(git_parse_ctx *ctx, const char *content, size_t content_len);
void git_parse_ctx_clear(git_parse_ctx *ctx);

#define git_parse_ctx_contains_s(ctx, str) \
	git_parse_ctx_contains(ctx, str, sizeof(str) - 1)

GIT_INLINE(bool) git_parse_ctx_contains(
	git_parse_ctx *ctx, const char *str, size_t len)
{
	return (ctx->line_len >= len && memcmp(ctx->line, str, len) == 0);
}

void git_parse_advance_line(git_parse_ctx *ctx);
void git_parse_advance_chars(git_parse_ctx *ctx, size_t char_cnt);
int git_parse_advance_expected(
	git_parse_ctx *ctx,
	const char *expected,
	size_t expected_len);

#define git_parse_advance_expected_str(ctx, str) \
	git_parse_advance_expected(ctx, str, strlen(str))

int git_parse_advance_ws(git_parse_ctx *ctx);
int git_parse_advance_nl(git_parse_ctx *ctx);
int git_parse_advance_digit(int64_t *out, git_parse_ctx *ctx, int base);
int git_parse_advance_oid(git_oid *out, git_parse_ctx *ctx, git_oid_t oid_type);

enum GIT_PARSE_PEEK_FLAGS {
	GIT_PARSE_PEEK_SKIP_WHITESPACE = (1 << 0)
};

int git_parse_peek(char *out, git_parse_ctx *ctx, int flags);

#endif
