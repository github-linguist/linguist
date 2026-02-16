/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "parse.h"
#include "oid.h"

int git_parse_ctx_init(git_parse_ctx *ctx, const char *content, size_t content_len)
{
	if (content && content_len) {
		ctx->content = content;
		ctx->content_len = content_len;
	} else {
		ctx->content = "";
		ctx->content_len = 0;
	}

	ctx->remain = ctx->content;
	ctx->remain_len = ctx->content_len;
	ctx->line = ctx->remain;
	ctx->line_len = git__linenlen(ctx->line, ctx->remain_len);
	ctx->line_num = 1;

	return 0;
}

void git_parse_ctx_clear(git_parse_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));
	ctx->content = "";
}

void git_parse_advance_line(git_parse_ctx *ctx)
{
	ctx->line += ctx->line_len;
	ctx->remain_len -= ctx->line_len;
	ctx->line_len = git__linenlen(ctx->line, ctx->remain_len);
	ctx->line_num++;
}

void git_parse_advance_chars(git_parse_ctx *ctx, size_t char_cnt)
{
	ctx->line += char_cnt;
	ctx->remain_len -= char_cnt;
	ctx->line_len -= char_cnt;
}

int git_parse_advance_expected(
	git_parse_ctx *ctx,
	const char *expected,
	size_t expected_len)
{
	if (ctx->line_len < expected_len)
		return -1;

	if (memcmp(ctx->line, expected, expected_len) != 0)
		return -1;

	git_parse_advance_chars(ctx, expected_len);
	return 0;
}

int git_parse_advance_ws(git_parse_ctx *ctx)
{
	int ret = -1;

	while (ctx->line_len > 0 &&
		ctx->line[0] != '\n' &&
		git__isspace(ctx->line[0])) {
		ctx->line++;
		ctx->line_len--;
		ctx->remain_len--;
		ret = 0;
	}

	return ret;
}

int git_parse_advance_nl(git_parse_ctx *ctx)
{
	if (ctx->line_len != 1 || ctx->line[0] != '\n')
		return -1;

	git_parse_advance_line(ctx);
	return 0;
}

int git_parse_advance_digit(int64_t *out, git_parse_ctx *ctx, int base)
{
	const char *end;
	int ret;

	if (ctx->line_len < 1 || !git__isdigit(ctx->line[0]))
		return -1;

	if ((ret = git__strntol64(out, ctx->line, ctx->line_len, &end, base)) < 0)
		return -1;

	git_parse_advance_chars(ctx, (end - ctx->line));
	return 0;
}

int git_parse_advance_oid(git_oid *out, git_parse_ctx *ctx, git_oid_t oid_type)
{
	size_t oid_hexsize = git_oid_hexsize(oid_type);
	GIT_ASSERT(oid_hexsize);

	if (ctx->line_len < oid_hexsize)
		return -1;
	if ((git_oid__fromstrn(out, ctx->line, oid_hexsize, oid_type)) < 0)
		return -1;
	git_parse_advance_chars(ctx, oid_hexsize);
	return 0;
}

int git_parse_peek(char *out, git_parse_ctx *ctx, int flags)
{
	size_t remain = ctx->line_len;
	const char *ptr = ctx->line;

	while (remain) {
		char c = *ptr;

		if ((flags & GIT_PARSE_PEEK_SKIP_WHITESPACE) &&
		    git__isspace(c)) {
			remain--;
			ptr++;
			continue;
		}

		*out = c;
		return 0;
	}

	return -1;
}
