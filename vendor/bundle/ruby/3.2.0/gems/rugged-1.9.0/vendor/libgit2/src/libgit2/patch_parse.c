/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "patch_parse.h"

#include "git2/patch.h"
#include "patch.h"
#include "diff_parse.h"
#include "fs_path.h"

typedef struct {
	git_patch base;

	git_patch_parse_ctx *ctx;

	/* the paths from the `diff --git` header, these will be used if this is not
	 * a rename (and rename paths are specified) or if no `+++`/`---` line specify
	 * the paths.
	 */
	char *header_old_path, *header_new_path;

	/* renamed paths are precise and are not prefixed */
	char *rename_old_path, *rename_new_path;

	/* the paths given in `---` and `+++` lines */
	char *old_path, *new_path;

	/* the prefixes from the old/new paths */
	char *old_prefix, *new_prefix;
} git_patch_parsed;

static int git_parse_err(const char *fmt, ...) GIT_FORMAT_PRINTF(1, 2);
static int git_parse_err(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	git_error_vset(GIT_ERROR_PATCH, fmt, ap);
	va_end(ap);

	return -1;
}

static size_t header_path_len(git_patch_parse_ctx *ctx)
{
	bool inquote = 0;
	bool quoted = git_parse_ctx_contains_s(&ctx->parse_ctx, "\"");
	size_t len;

	for (len = quoted; len < ctx->parse_ctx.line_len; len++) {
		if (!quoted && git__isspace(ctx->parse_ctx.line[len]))
			break;
		else if (quoted && !inquote && ctx->parse_ctx.line[len] == '"') {
			len++;
			break;
		}

		inquote = (!inquote && ctx->parse_ctx.line[len] == '\\');
	}

	return len;
}

static int parse_header_path_buf(git_str *path, git_patch_parse_ctx *ctx, size_t path_len)
{
	int error;

	if ((error = git_str_put(path, ctx->parse_ctx.line, path_len)) < 0)
		return error;

	git_parse_advance_chars(&ctx->parse_ctx, path_len);

	git_str_rtrim(path);

	if (path->size > 0 && path->ptr[0] == '"' &&
	    (error = git_str_unquote(path)) < 0)
		return error;

	git_fs_path_squash_slashes(path);

	if (!path->size)
		return git_parse_err("patch contains empty path at line %"PRIuZ,
				     ctx->parse_ctx.line_num);

	return 0;
}

static int parse_header_path(char **out, git_patch_parse_ctx *ctx)
{
	git_str path = GIT_STR_INIT;
	int error;

	if ((error = parse_header_path_buf(&path, ctx, header_path_len(ctx))) < 0)
		goto out;
	*out = git_str_detach(&path);

out:
	git_str_dispose(&path);
	return error;
}

static int parse_header_git_oldpath(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	git_str old_path = GIT_STR_INIT;
	int error;

	if (patch->old_path) {
		error = git_parse_err("patch contains duplicate old path at line %"PRIuZ,
				      ctx->parse_ctx.line_num);
		goto out;
	}

	if ((error = parse_header_path_buf(&old_path, ctx, ctx->parse_ctx.line_len - 1)) <  0)
		goto out;

	patch->old_path = git_str_detach(&old_path);

out:
	git_str_dispose(&old_path);
	return error;
}

static int parse_header_git_newpath(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	git_str new_path = GIT_STR_INIT;
	int error;

	if (patch->new_path) {
		error = git_parse_err("patch contains duplicate new path at line %"PRIuZ,
				      ctx->parse_ctx.line_num);
		goto out;
	}

	if ((error = parse_header_path_buf(&new_path, ctx, ctx->parse_ctx.line_len - 1)) <  0)
		goto out;
	patch->new_path = git_str_detach(&new_path);

out:
	git_str_dispose(&new_path);
	return error;
}

static int parse_header_mode(uint16_t *mode, git_patch_parse_ctx *ctx)
{
	int64_t m;

	if ((git_parse_advance_digit(&m, &ctx->parse_ctx, 8)) < 0)
		return git_parse_err("invalid file mode at line %"PRIuZ, ctx->parse_ctx.line_num);

	if (m > UINT16_MAX)
		return -1;

	*mode = (uint16_t)m;

	return 0;
}

static int parse_header_oid(
	git_oid *oid,
	uint16_t *oid_len,
	git_patch_parse_ctx *ctx)
{
	size_t hexsize, len;

	hexsize = git_oid_hexsize(ctx->opts.oid_type);

	for (len = 0;
	     len < ctx->parse_ctx.line_len && len < hexsize;
	     len++) {
		if (!git__isxdigit(ctx->parse_ctx.line[len]))
			break;
	}

	if (len < GIT_OID_MINPREFIXLEN || len > hexsize ||
		git_oid__fromstrn(oid, ctx->parse_ctx.line, len, ctx->opts.oid_type) < 0)
		return git_parse_err("invalid hex formatted object id at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	git_parse_advance_chars(&ctx->parse_ctx, len);

	*oid_len = (uint16_t)len;

	return 0;
}

static int parse_header_git_index(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	char c;

	if (parse_header_oid(&patch->base.delta->old_file.id,
			&patch->base.delta->old_file.id_abbrev, ctx) < 0 ||
		git_parse_advance_expected_str(&ctx->parse_ctx, "..") < 0 ||
		parse_header_oid(&patch->base.delta->new_file.id,
			&patch->base.delta->new_file.id_abbrev, ctx) < 0)
		return -1;

	if (git_parse_peek(&c, &ctx->parse_ctx, 0) == 0 && c == ' ') {
		uint16_t mode = 0;

		git_parse_advance_chars(&ctx->parse_ctx, 1);

		if (parse_header_mode(&mode, ctx) < 0)
			return -1;

		if (!patch->base.delta->new_file.mode)
			patch->base.delta->new_file.mode = mode;

		if (!patch->base.delta->old_file.mode)
			patch->base.delta->old_file.mode = mode;
	}

	return 0;
}

static int parse_header_git_oldmode(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	return parse_header_mode(&patch->base.delta->old_file.mode, ctx);
}

static int parse_header_git_newmode(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	return parse_header_mode(&patch->base.delta->new_file.mode, ctx);
}

static int parse_header_git_deletedfilemode(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	git__free((char *)patch->base.delta->new_file.path);

	patch->base.delta->new_file.path = NULL;
	patch->base.delta->status = GIT_DELTA_DELETED;
	patch->base.delta->nfiles = 1;

	return parse_header_mode(&patch->base.delta->old_file.mode, ctx);
}

static int parse_header_git_newfilemode(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	git__free((char *)patch->base.delta->old_file.path);

	patch->base.delta->old_file.path = NULL;
	patch->base.delta->status = GIT_DELTA_ADDED;
	patch->base.delta->nfiles = 1;

	return parse_header_mode(&patch->base.delta->new_file.mode, ctx);
}

static int parse_header_rename(
	char **out,
	git_patch_parse_ctx *ctx)
{
	git_str path = GIT_STR_INIT;

	if (parse_header_path_buf(&path, ctx, header_path_len(ctx)) < 0)
		return -1;

	/* Note: the `rename from` and `rename to` lines include the literal
	 * filename.  They do *not* include the prefix.  (Who needs consistency?)
	 */
	*out = git_str_detach(&path);
	return 0;
}

static int parse_header_renamefrom(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_RENAMED;
	return parse_header_rename(&patch->rename_old_path, ctx);
}

static int parse_header_renameto(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_RENAMED;
	return parse_header_rename(&patch->rename_new_path, ctx);
}

static int parse_header_copyfrom(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_COPIED;
	return parse_header_rename(&patch->rename_old_path, ctx);
}

static int parse_header_copyto(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_COPIED;
	return parse_header_rename(&patch->rename_new_path, ctx);
}

static int parse_header_percent(uint16_t *out, git_patch_parse_ctx *ctx)
{
	int64_t val;

	if (git_parse_advance_digit(&val, &ctx->parse_ctx, 10) < 0)
		return -1;

	if (git_parse_advance_expected_str(&ctx->parse_ctx, "%") < 0)
		return -1;

	if (val < 0 || val > 100)
		return -1;

	*out = (uint16_t)val;
	return 0;
}

static int parse_header_similarity(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	if (parse_header_percent(&patch->base.delta->similarity, ctx) < 0)
		return git_parse_err("invalid similarity percentage at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	return 0;
}

static int parse_header_dissimilarity(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	uint16_t dissimilarity;

	if (parse_header_percent(&dissimilarity, ctx) < 0)
		return git_parse_err("invalid similarity percentage at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	patch->base.delta->similarity = 100 - dissimilarity;

	return 0;
}

static int parse_header_start(git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	if (parse_header_path(&patch->header_old_path, ctx) < 0)
		return git_parse_err("corrupt old path in git diff header at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	if (git_parse_advance_ws(&ctx->parse_ctx) < 0 ||
		parse_header_path(&patch->header_new_path, ctx) < 0)
		return git_parse_err("corrupt new path in git diff header at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	/*
	 * We cannot expect to be able to always parse paths correctly at this
	 * point. Due to the possibility of unquoted names, whitespaces in
	 * filenames and custom prefixes we have to allow that, though, and just
	 * proceed here. We then hope for the "---" and "+++" lines to fix that
	 * for us.
	 */
	if (!git_parse_ctx_contains(&ctx->parse_ctx, "\n", 1) &&
	    !git_parse_ctx_contains(&ctx->parse_ctx, "\r\n", 2)) {
		git_parse_advance_chars(&ctx->parse_ctx, ctx->parse_ctx.line_len - 1);

		git__free(patch->header_old_path);
		patch->header_old_path = NULL;
		git__free(patch->header_new_path);
		patch->header_new_path = NULL;
	}

	return 0;
}

typedef enum {
	STATE_START,

	STATE_DIFF,
	STATE_FILEMODE,
	STATE_MODE,
	STATE_INDEX,
	STATE_PATH,

	STATE_SIMILARITY,
	STATE_RENAME,
	STATE_COPY,

	STATE_END
} parse_header_state;

typedef struct {
	const char *str;
	parse_header_state expected_state;
	parse_header_state next_state;
	int(*fn)(git_patch_parsed *, git_patch_parse_ctx *);
} parse_header_transition;

static const parse_header_transition transitions[] = {
	/* Start */
	{ "diff --git "         , STATE_START,      STATE_DIFF,       parse_header_start },

	{ "deleted file mode "  , STATE_DIFF,       STATE_FILEMODE,   parse_header_git_deletedfilemode },
	{ "new file mode "      , STATE_DIFF,       STATE_FILEMODE,   parse_header_git_newfilemode },
	{ "old mode "           , STATE_DIFF,       STATE_MODE,       parse_header_git_oldmode },
	{ "new mode "           , STATE_MODE,       STATE_END,        parse_header_git_newmode },

	{ "index "              , STATE_FILEMODE,   STATE_INDEX,      parse_header_git_index },
	{ "index "              , STATE_DIFF,       STATE_INDEX,      parse_header_git_index },
	{ "index "              , STATE_END,        STATE_INDEX,      parse_header_git_index },

	{ "--- "                , STATE_DIFF,       STATE_PATH,       parse_header_git_oldpath },
	{ "--- "                , STATE_INDEX,      STATE_PATH,       parse_header_git_oldpath },
	{ "--- "                , STATE_FILEMODE,   STATE_PATH,       parse_header_git_oldpath },
	{ "+++ "                , STATE_PATH,       STATE_END,        parse_header_git_newpath },
	{ "GIT binary patch"    , STATE_INDEX,      STATE_END,        NULL },
	{ "Binary files "       , STATE_INDEX,      STATE_END,        NULL },

	{ "similarity index "   , STATE_END,        STATE_SIMILARITY, parse_header_similarity },
	{ "similarity index "   , STATE_DIFF,       STATE_SIMILARITY, parse_header_similarity },
	{ "dissimilarity index ", STATE_DIFF,       STATE_SIMILARITY, parse_header_dissimilarity },
	{ "rename from "        , STATE_SIMILARITY, STATE_RENAME,     parse_header_renamefrom },
	{ "rename old "         , STATE_SIMILARITY, STATE_RENAME,     parse_header_renamefrom },
	{ "copy from "          , STATE_SIMILARITY, STATE_COPY,       parse_header_copyfrom },
	{ "rename to "          , STATE_RENAME,     STATE_END,        parse_header_renameto },
	{ "rename new "         , STATE_RENAME,     STATE_END,        parse_header_renameto },
	{ "copy to "            , STATE_COPY,       STATE_END,        parse_header_copyto },

	/* Next patch */
	{ "diff --git "         , STATE_END,        0,                NULL },
	{ "@@ -"                , STATE_END,        0,                NULL },
	{ "-- "                 , STATE_INDEX,      0,                NULL },
	{ "-- "                 , STATE_END,        0,                NULL },
};

static int parse_header_git(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	size_t i;
	int error = 0;
	parse_header_state state = STATE_START;

	/* Parse remaining header lines */
	for (; ctx->parse_ctx.remain_len > 0; git_parse_advance_line(&ctx->parse_ctx)) {
		bool found = false;

		if (ctx->parse_ctx.line_len == 0 || ctx->parse_ctx.line[ctx->parse_ctx.line_len - 1] != '\n')
			break;

		for (i = 0; i < ARRAY_SIZE(transitions); i++) {
			const parse_header_transition *transition = &transitions[i];
			size_t op_len = strlen(transition->str);

			if (transition->expected_state != state ||
			    git__prefixcmp(ctx->parse_ctx.line, transition->str) != 0)
				continue;

			state = transition->next_state;

			/* Do not advance if this is the patch separator */
			if (transition->fn == NULL)
				goto done;

			git_parse_advance_chars(&ctx->parse_ctx, op_len);

			if ((error = transition->fn(patch, ctx)) < 0)
				goto done;

			git_parse_advance_ws(&ctx->parse_ctx);

			if (git_parse_advance_expected_str(&ctx->parse_ctx, "\n") < 0 ||
			    ctx->parse_ctx.line_len > 0) {
				error = git_parse_err("trailing data at line %"PRIuZ, ctx->parse_ctx.line_num);
				goto done;
			}

			found = true;
			break;
		}

		if (!found) {
			error = git_parse_err("invalid patch header at line %"PRIuZ,
				ctx->parse_ctx.line_num);
			goto done;
		}
	}

	if (state != STATE_END) {
		error = git_parse_err("unexpected header line %"PRIuZ, ctx->parse_ctx.line_num);
		goto done;
	}

done:
	return error;
}

static int parse_int(int *out, git_patch_parse_ctx *ctx)
{
	int64_t num;

	if (git_parse_advance_digit(&num, &ctx->parse_ctx, 10) < 0 || !git__is_int(num))
		return -1;

	*out = (int)num;
	return 0;
}

static int parse_hunk_header(
	git_patch_hunk *hunk,
	git_patch_parse_ctx *ctx)
{
	const char *header_start = ctx->parse_ctx.line;
	char c;

	hunk->hunk.old_lines = 1;
	hunk->hunk.new_lines = 1;

	if (git_parse_advance_expected_str(&ctx->parse_ctx, "@@ -") < 0 ||
		parse_int(&hunk->hunk.old_start, ctx) < 0)
		goto fail;

	if (git_parse_peek(&c, &ctx->parse_ctx, 0) == 0 && c == ',') {
		if (git_parse_advance_expected_str(&ctx->parse_ctx, ",") < 0 ||
			parse_int(&hunk->hunk.old_lines, ctx) < 0)
			goto fail;
	}

	if (git_parse_advance_expected_str(&ctx->parse_ctx, " +") < 0 ||
		parse_int(&hunk->hunk.new_start, ctx) < 0)
		goto fail;

	if (git_parse_peek(&c, &ctx->parse_ctx, 0) == 0 && c == ',') {
		if (git_parse_advance_expected_str(&ctx->parse_ctx, ",") < 0 ||
			parse_int(&hunk->hunk.new_lines, ctx) < 0)
			goto fail;
	}

	if (git_parse_advance_expected_str(&ctx->parse_ctx, " @@") < 0)
		goto fail;

	git_parse_advance_line(&ctx->parse_ctx);

	if (!hunk->hunk.old_lines && !hunk->hunk.new_lines)
		goto fail;

	hunk->hunk.header_len = ctx->parse_ctx.line - header_start;
	if (hunk->hunk.header_len > (GIT_DIFF_HUNK_HEADER_SIZE - 1))
		return git_parse_err("oversized patch hunk header at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	memcpy(hunk->hunk.header, header_start, hunk->hunk.header_len);
	hunk->hunk.header[hunk->hunk.header_len] = '\0';

	return 0;

fail:
	git_error_set(GIT_ERROR_PATCH, "invalid patch hunk header at line %"PRIuZ,
		ctx->parse_ctx.line_num);
	return -1;
}

static int eof_for_origin(int origin) {
	if (origin == GIT_DIFF_LINE_ADDITION)
		return GIT_DIFF_LINE_DEL_EOFNL;
	if (origin == GIT_DIFF_LINE_DELETION)
		return GIT_DIFF_LINE_ADD_EOFNL;
	return GIT_DIFF_LINE_CONTEXT_EOFNL;
}

static int parse_hunk_body(
	git_patch_parsed *patch,
	git_patch_hunk *hunk,
	git_patch_parse_ctx *ctx)
{
	git_diff_line *line;
	int error = 0;

	int oldlines = hunk->hunk.old_lines;
	int newlines = hunk->hunk.new_lines;
	int last_origin = 0;

	for (;
		ctx->parse_ctx.remain_len > 1 &&
		(oldlines || newlines) &&
		!git_parse_ctx_contains_s(&ctx->parse_ctx, "@@ -");
		git_parse_advance_line(&ctx->parse_ctx)) {

		int old_lineno, new_lineno, origin, prefix = 1;
		char c;

		if (git__add_int_overflow(&old_lineno, hunk->hunk.old_start, hunk->hunk.old_lines) ||
		    git__sub_int_overflow(&old_lineno, old_lineno, oldlines) ||
		    git__add_int_overflow(&new_lineno, hunk->hunk.new_start, hunk->hunk.new_lines) ||
		    git__sub_int_overflow(&new_lineno, new_lineno, newlines)) {
			error = git_parse_err("unrepresentable line count at line %"PRIuZ,
					      ctx->parse_ctx.line_num);
			goto done;
		}

		if (ctx->parse_ctx.line_len == 0 || ctx->parse_ctx.line[ctx->parse_ctx.line_len - 1] != '\n') {
			error = git_parse_err("invalid patch instruction at line %"PRIuZ,
				ctx->parse_ctx.line_num);
			goto done;
		}

		git_parse_peek(&c, &ctx->parse_ctx, 0);

		switch (c) {
		case '\n':
			prefix = 0;
			/* fall through */

		case ' ':
			origin = GIT_DIFF_LINE_CONTEXT;
			oldlines--;
			newlines--;
			break;

		case '-':
			origin = GIT_DIFF_LINE_DELETION;
			oldlines--;
			new_lineno = -1;
			break;

		case '+':
			origin = GIT_DIFF_LINE_ADDITION;
			newlines--;
			old_lineno = -1;
			break;

		case '\\':
			/*
			 * If there are no oldlines left, then this is probably
			 * the "\ No newline at end of file" marker. Do not
			 * verify its format, as it may be localized.
			 */
			if (!oldlines) {
				prefix = 0;
				origin = eof_for_origin(last_origin);
				old_lineno = -1;
				new_lineno = -1;
				break;
			}
			/* fall through */

		default:
			error = git_parse_err("invalid patch hunk at line %"PRIuZ, ctx->parse_ctx.line_num);
			goto done;
		}

		line = git_array_alloc(patch->base.lines);
		GIT_ERROR_CHECK_ALLOC(line);

		memset(line, 0x0, sizeof(git_diff_line));

		line->content_len = ctx->parse_ctx.line_len - prefix;
		line->content = git__strndup(ctx->parse_ctx.line + prefix, line->content_len);
		GIT_ERROR_CHECK_ALLOC(line->content);
		line->content_offset = ctx->parse_ctx.content_len - ctx->parse_ctx.remain_len;
		line->origin = origin;
		line->num_lines = 1;
		line->old_lineno = old_lineno;
		line->new_lineno = new_lineno;

		hunk->line_count++;

		last_origin = origin;
	}

	if (oldlines || newlines) {
		error = git_parse_err(
			"invalid patch hunk, expected %d old lines and %d new lines",
			hunk->hunk.old_lines, hunk->hunk.new_lines);
		goto done;
	}

	/*
	 * Handle "\ No newline at end of file". Only expect the leading
	 * backslash, though, because the rest of the string could be
	 * localized.  Because `diff` optimizes for the case where you
	 * want to apply the patch by hand.
	 */
	if (git_parse_ctx_contains_s(&ctx->parse_ctx, "\\ ") &&
		git_array_size(patch->base.lines) > 0) {

		line = git_array_get(patch->base.lines, git_array_size(patch->base.lines) - 1);

		if (line->content_len < 1) {
			error = git_parse_err("last line has no trailing newline");
			goto done;
		}

		line = git_array_alloc(patch->base.lines);
		GIT_ERROR_CHECK_ALLOC(line);

		memset(line, 0x0, sizeof(git_diff_line));

		line->content_len = ctx->parse_ctx.line_len;
		line->content = git__strndup(ctx->parse_ctx.line, line->content_len);
		GIT_ERROR_CHECK_ALLOC(line->content);
		line->content_offset = ctx->parse_ctx.content_len - ctx->parse_ctx.remain_len;
		line->origin = eof_for_origin(last_origin);
		line->num_lines = 1;
		line->old_lineno = -1;
		line->new_lineno = -1;

		hunk->line_count++;

		git_parse_advance_line(&ctx->parse_ctx);
	}

done:
	return error;
}

static int parse_patch_header(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	int error = 0;

	for (; ctx->parse_ctx.remain_len > 0; git_parse_advance_line(&ctx->parse_ctx)) {
		/* This line is too short to be a patch header. */
		if (ctx->parse_ctx.line_len < 6)
			continue;

		/* This might be a hunk header without a patch header, provide a
		 * sensible error message. */
		if (git_parse_ctx_contains_s(&ctx->parse_ctx, "@@ -")) {
			size_t line_num = ctx->parse_ctx.line_num;
			git_patch_hunk hunk;

			/* If this cannot be parsed as a hunk header, it's just leading
			* noise, continue.
			*/
			if (parse_hunk_header(&hunk, ctx) < 0) {
				git_error_clear();
				continue;
			}

			error = git_parse_err("invalid hunk header outside patch at line %"PRIuZ,
				line_num);
			goto done;
		}

		/* This buffer is too short to contain a patch. */
		if (ctx->parse_ctx.remain_len < ctx->parse_ctx.line_len + 6)
			break;

		/* A proper git patch */
		if (git_parse_ctx_contains_s(&ctx->parse_ctx, "diff --git ")) {
			error = parse_header_git(patch, ctx);
			goto done;
		}

		error = 0;
		continue;
	}

	git_error_set(GIT_ERROR_PATCH, "no patch found");
	error = GIT_ENOTFOUND;

done:
	return error;
}

static int parse_patch_binary_side(
	git_diff_binary_file *binary,
	git_patch_parse_ctx *ctx)
{
	git_diff_binary_t type = GIT_DIFF_BINARY_NONE;
	git_str base85 = GIT_STR_INIT, decoded = GIT_STR_INIT;
	int64_t len;
	int error = 0;

	if (git_parse_ctx_contains_s(&ctx->parse_ctx, "literal ")) {
		type = GIT_DIFF_BINARY_LITERAL;
		git_parse_advance_chars(&ctx->parse_ctx, 8);
	} else if (git_parse_ctx_contains_s(&ctx->parse_ctx, "delta ")) {
		type = GIT_DIFF_BINARY_DELTA;
		git_parse_advance_chars(&ctx->parse_ctx, 6);
	} else {
		error = git_parse_err(
			"unknown binary delta type at line %"PRIuZ, ctx->parse_ctx.line_num);
		goto done;
	}

	if (git_parse_advance_digit(&len, &ctx->parse_ctx, 10) < 0 ||
	    git_parse_advance_nl(&ctx->parse_ctx) < 0 || len < 0) {
		error = git_parse_err("invalid binary size at line %"PRIuZ, ctx->parse_ctx.line_num);
		goto done;
	}

	while (ctx->parse_ctx.line_len) {
		char c;
		size_t encoded_len, decoded_len = 0, decoded_orig = decoded.size;

		git_parse_peek(&c, &ctx->parse_ctx, 0);

		if (c == '\n')
			break;
		else if (c >= 'A' && c <= 'Z')
			decoded_len = c - 'A' + 1;
		else if (c >= 'a' && c <= 'z')
			decoded_len = c - 'a' + (('z' - 'a') + 1) + 1;

		if (!decoded_len) {
			error = git_parse_err("invalid binary length at line %"PRIuZ, ctx->parse_ctx.line_num);
			goto done;
		}

		git_parse_advance_chars(&ctx->parse_ctx, 1);

		encoded_len = ((decoded_len / 4) + !!(decoded_len % 4)) * 5;

		if (!encoded_len || !ctx->parse_ctx.line_len || encoded_len > ctx->parse_ctx.line_len - 1) {
			error = git_parse_err("truncated binary data at line %"PRIuZ, ctx->parse_ctx.line_num);
			goto done;
		}

		if ((error = git_str_decode_base85(
			&decoded, ctx->parse_ctx.line, encoded_len, decoded_len)) < 0)
			goto done;

		if (decoded.size - decoded_orig != decoded_len) {
			error = git_parse_err("truncated binary data at line %"PRIuZ, ctx->parse_ctx.line_num);
			goto done;
		}

		git_parse_advance_chars(&ctx->parse_ctx, encoded_len);

		if (git_parse_advance_nl(&ctx->parse_ctx) < 0) {
			error = git_parse_err("trailing data at line %"PRIuZ, ctx->parse_ctx.line_num);
			goto done;
		}
	}

	binary->type = type;
	binary->inflatedlen = (size_t)len;
	binary->datalen = decoded.size;
	binary->data = git_str_detach(&decoded);

done:
	git_str_dispose(&base85);
	git_str_dispose(&decoded);
	return error;
}

static int parse_patch_binary(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	int error;

	if (git_parse_advance_expected_str(&ctx->parse_ctx, "GIT binary patch") < 0 ||
		git_parse_advance_nl(&ctx->parse_ctx) < 0)
		return git_parse_err("corrupt git binary header at line %"PRIuZ, ctx->parse_ctx.line_num);

	/* parse old->new binary diff */
	if ((error = parse_patch_binary_side(
			&patch->base.binary.new_file, ctx)) < 0)
		return error;

	if (git_parse_advance_nl(&ctx->parse_ctx) < 0)
		return git_parse_err("corrupt git binary separator at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	/* parse new->old binary diff */
	if ((error = parse_patch_binary_side(
			&patch->base.binary.old_file, ctx)) < 0)
		return error;

	if (git_parse_advance_nl(&ctx->parse_ctx) < 0)
		return git_parse_err("corrupt git binary patch separator at line %"PRIuZ,
			ctx->parse_ctx.line_num);

	patch->base.binary.contains_data = 1;
	patch->base.delta->flags |= GIT_DIFF_FLAG_BINARY;
	return 0;
}

static int parse_patch_binary_nodata(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	const char *old = patch->old_path ? patch->old_path : patch->header_old_path;
	const char *new = patch->new_path ? patch->new_path : patch->header_new_path;

	if (!old || !new)
		return git_parse_err("corrupt binary data without paths at line %"PRIuZ, ctx->parse_ctx.line_num);

	if (patch->base.delta->status == GIT_DELTA_ADDED)
		old = "/dev/null";
	else if (patch->base.delta->status == GIT_DELTA_DELETED)
		new = "/dev/null";

	if (git_parse_advance_expected_str(&ctx->parse_ctx, "Binary files ") < 0 ||
	    git_parse_advance_expected_str(&ctx->parse_ctx, old) < 0 ||
	    git_parse_advance_expected_str(&ctx->parse_ctx, " and ") < 0 ||
	    git_parse_advance_expected_str(&ctx->parse_ctx, new) < 0 ||
	    git_parse_advance_expected_str(&ctx->parse_ctx, " differ") < 0 ||
	    git_parse_advance_nl(&ctx->parse_ctx) < 0)
		return git_parse_err("corrupt git binary header at line %"PRIuZ, ctx->parse_ctx.line_num);

	patch->base.binary.contains_data = 0;
	patch->base.delta->flags |= GIT_DIFF_FLAG_BINARY;
	return 0;
}

static int parse_patch_hunks(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	git_patch_hunk *hunk;
	int error = 0;

	while (git_parse_ctx_contains_s(&ctx->parse_ctx, "@@ -")) {
		hunk = git_array_alloc(patch->base.hunks);
		GIT_ERROR_CHECK_ALLOC(hunk);

		memset(hunk, 0, sizeof(git_patch_hunk));

		hunk->line_start = git_array_size(patch->base.lines);
		hunk->line_count = 0;

		if ((error = parse_hunk_header(hunk, ctx)) < 0 ||
			(error = parse_hunk_body(patch, hunk, ctx)) < 0)
			goto done;
	}

	patch->base.delta->flags |= GIT_DIFF_FLAG_NOT_BINARY;

done:
	return error;
}

static int parse_patch_body(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	if (git_parse_ctx_contains_s(&ctx->parse_ctx, "GIT binary patch"))
		return parse_patch_binary(patch, ctx);
	else if (git_parse_ctx_contains_s(&ctx->parse_ctx, "Binary files "))
		return parse_patch_binary_nodata(patch, ctx);
	else
		return parse_patch_hunks(patch, ctx);
}

static int check_header_names(
	const char *one,
	const char *two,
	const char *old_or_new,
	bool two_null)
{
	if (!one || !two)
		return 0;

	if (two_null && strcmp(two, "/dev/null") != 0)
		return git_parse_err("expected %s path of '/dev/null'", old_or_new);

	else if (!two_null && strcmp(one, two) != 0)
		return git_parse_err("mismatched %s path names", old_or_new);

	return 0;
}

static int check_prefix(
	char **out,
	size_t *out_len,
	git_patch_parsed *patch,
	const char *path_start)
{
	const char *path = path_start;
	size_t prefix_len = patch->ctx->opts.prefix_len;
	size_t remain_len = prefix_len;

	*out = NULL;
	*out_len = 0;

	if (prefix_len == 0)
		goto done;

	/* leading slashes do not count as part of the prefix in git apply */
	while (*path == '/')
		path++;

	while (*path && remain_len) {
		if (*path == '/')
			remain_len--;

		path++;
	}

	if (remain_len || !*path)
		return git_parse_err(
			"header filename does not contain %"PRIuZ" path components",
			prefix_len);

done:
	*out_len = (path - path_start);
	*out = git__strndup(path_start, *out_len);

	return (*out == NULL) ? -1 : 0;
}

static int check_filenames(git_patch_parsed *patch)
{
	const char *prefixed_new, *prefixed_old;
	size_t old_prefixlen = 0, new_prefixlen = 0;
	bool added = (patch->base.delta->status == GIT_DELTA_ADDED);
	bool deleted = (patch->base.delta->status == GIT_DELTA_DELETED);

	if (patch->old_path && !patch->new_path)
		return git_parse_err("missing new path");

	if (!patch->old_path && patch->new_path)
		return git_parse_err("missing old path");

	/* Ensure (non-renamed) paths match */
	if (check_header_names(patch->header_old_path, patch->old_path, "old", added) < 0 ||
	    check_header_names(patch->header_new_path, patch->new_path, "new", deleted) < 0)
		return -1;

	prefixed_old = (!added && patch->old_path) ? patch->old_path : patch->header_old_path;
	prefixed_new = (!deleted && patch->new_path) ? patch->new_path : patch->header_new_path;

	if ((prefixed_old && check_prefix(&patch->old_prefix, &old_prefixlen, patch, prefixed_old) < 0) ||
	    (prefixed_new && check_prefix(&patch->new_prefix, &new_prefixlen, patch, prefixed_new) < 0))
		return -1;

	/* Prefer the rename filenames as they are unambiguous and unprefixed */
	if (patch->rename_old_path)
		patch->base.delta->old_file.path = patch->rename_old_path;
	else if (prefixed_old)
		patch->base.delta->old_file.path = prefixed_old + old_prefixlen;
	else
		patch->base.delta->old_file.path = NULL;

	if (patch->rename_new_path)
		patch->base.delta->new_file.path = patch->rename_new_path;
	else if (prefixed_new)
		patch->base.delta->new_file.path = prefixed_new + new_prefixlen;
	else
		patch->base.delta->new_file.path = NULL;

	if (!patch->base.delta->old_file.path &&
	    !patch->base.delta->new_file.path)
		return git_parse_err("git diff header lacks old / new paths");

	return 0;
}

static int check_patch(git_patch_parsed *patch)
{
	git_diff_delta *delta = patch->base.delta;

	if (check_filenames(patch) < 0)
		return -1;

	if (delta->old_file.path &&
	    delta->status != GIT_DELTA_DELETED &&
	    !delta->new_file.mode)
		delta->new_file.mode = delta->old_file.mode;

	if (delta->status == GIT_DELTA_MODIFIED &&
	    !(delta->flags & GIT_DIFF_FLAG_BINARY) &&
	    delta->new_file.mode == delta->old_file.mode &&
	    git_array_size(patch->base.hunks) == 0)
		return git_parse_err("patch with no hunks");

	if (delta->status == GIT_DELTA_ADDED) {
		git_oid_clear(&delta->old_file.id,
			patch->base.diff_opts.oid_type);
		delta->old_file.id_abbrev = 0;
	}

	if (delta->status == GIT_DELTA_DELETED) {
		git_oid_clear(&delta->new_file.id,
			patch->base.diff_opts.oid_type);
		delta->new_file.id_abbrev = 0;
	}

	return 0;
}

git_patch_parse_ctx *git_patch_parse_ctx_init(
	const char *content,
	size_t content_len,
	const git_patch_options *opts)
{
	git_patch_parse_ctx *ctx;
	git_patch_options default_opts = GIT_PATCH_OPTIONS_INIT;

	if ((ctx = git__calloc(1, sizeof(git_patch_parse_ctx))) == NULL)
		return NULL;

	if ((git_parse_ctx_init(&ctx->parse_ctx, content, content_len)) < 0) {
		git__free(ctx);
		return NULL;
	}

	if (opts)
		memcpy(&ctx->opts, opts, sizeof(git_patch_options));
	else
		memcpy(&ctx->opts, &default_opts, sizeof(git_patch_options));

	GIT_REFCOUNT_INC(ctx);
	return ctx;
}

static void patch_parse_ctx_free(git_patch_parse_ctx *ctx)
{
	if (!ctx)
		return;

	git_parse_ctx_clear(&ctx->parse_ctx);
	git__free(ctx);
}

void git_patch_parse_ctx_free(git_patch_parse_ctx *ctx)
{
	GIT_REFCOUNT_DEC(ctx, patch_parse_ctx_free);
}

int git_patch_parsed_from_diff(git_patch **out, git_diff *d, size_t idx)
{
	git_diff_parsed *diff = (git_diff_parsed *)d;
	git_patch *p;

	if ((p = git_vector_get(&diff->patches, idx)) == NULL)
		return -1;

	GIT_REFCOUNT_INC(p);
	*out = p;

	return 0;
}

static void patch_parsed__free(git_patch *p)
{
	git_patch_parsed *patch = (git_patch_parsed *)p;
	git_diff_line *line;
	size_t i;

	if (!patch)
		return;

	git_patch_parse_ctx_free(patch->ctx);

	git__free((char *)patch->base.binary.old_file.data);
	git__free((char *)patch->base.binary.new_file.data);
	git_array_clear(patch->base.hunks);
	git_array_foreach(patch->base.lines, i, line)
		git__free((char *) line->content);
	git_array_clear(patch->base.lines);
	git__free(patch->base.delta);

	git__free(patch->old_prefix);
	git__free(patch->new_prefix);
	git__free(patch->header_old_path);
	git__free(patch->header_new_path);
	git__free(patch->rename_old_path);
	git__free(patch->rename_new_path);
	git__free(patch->old_path);
	git__free(patch->new_path);
	git__free(patch);
}

int git_patch_parse(
	git_patch **out,
	git_patch_parse_ctx *ctx)
{
	git_patch_parsed *patch;
	size_t start, used;
	int error = 0;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(ctx);

	*out = NULL;

	patch = git__calloc(1, sizeof(git_patch_parsed));
	GIT_ERROR_CHECK_ALLOC(patch);

	patch->ctx = ctx;
	GIT_REFCOUNT_INC(patch->ctx);

	patch->base.free_fn = patch_parsed__free;

	patch->base.delta = git__calloc(1, sizeof(git_diff_delta));
	GIT_ERROR_CHECK_ALLOC(patch->base.delta);

	patch->base.delta->status = GIT_DELTA_MODIFIED;
	patch->base.delta->nfiles = 2;

	patch->base.diff_opts.oid_type = ctx->opts.oid_type;

	start = ctx->parse_ctx.remain_len;

	if ((error = parse_patch_header(patch, ctx)) < 0 ||
	    (error = parse_patch_body(patch, ctx)) < 0 ||
	    (error = check_patch(patch)) < 0)
		goto done;

	used = start - ctx->parse_ctx.remain_len;
	ctx->parse_ctx.remain += used;

	patch->base.diff_opts.old_prefix = patch->old_prefix;
	patch->base.diff_opts.new_prefix = patch->new_prefix;
	patch->base.diff_opts.flags |= GIT_DIFF_SHOW_BINARY;

	GIT_REFCOUNT_INC(&patch->base);
	*out = &patch->base;

done:
	if (error < 0)
		patch_parsed__free(&patch->base);

	return error;
}

int git_patch_from_buffer(
	git_patch **out,
	const char *content,
	size_t content_len,
	const git_patch_options *opts)
{
	git_patch_parse_ctx *ctx;
	int error;

	ctx = git_patch_parse_ctx_init(content, content_len, opts);
	GIT_ERROR_CHECK_ALLOC(ctx);

	error = git_patch_parse(out, ctx);

	git_patch_parse_ctx_free(ctx);
	return error;
}

