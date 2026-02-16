/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "diff_xdiff.h"

#include "git2/errors.h"
#include "diff.h"
#include "diff_driver.h"
#include "patch_generate.h"
#include "utf8.h"

static int git_xdiff_scan_int(const char **str, int *value)
{
	const char *scan = *str;
	int v = 0, digits = 0;
	/* find next digit */
	for (scan = *str; *scan && !git__isdigit(*scan); scan++);
	/* parse next number */
	for (; git__isdigit(*scan); scan++, digits++)
		v = (v * 10) + (*scan - '0');
	*str = scan;
	*value = v;
	return (digits > 0) ? 0 : -1;
}

static int git_xdiff_parse_hunk(git_diff_hunk *hunk, const char *header)
{
	/* expect something of the form "@@ -%d[,%d] +%d[,%d] @@" */
	if (*header != '@')
		goto fail;
	if (git_xdiff_scan_int(&header, &hunk->old_start) < 0)
		goto fail;
	if (*header == ',') {
		if (git_xdiff_scan_int(&header, &hunk->old_lines) < 0)
			goto fail;
	} else
		hunk->old_lines = 1;
	if (git_xdiff_scan_int(&header, &hunk->new_start) < 0)
		goto fail;
	if (*header == ',') {
		if (git_xdiff_scan_int(&header, &hunk->new_lines) < 0)
			goto fail;
	} else
		hunk->new_lines = 1;
	if (hunk->old_start < 0 || hunk->new_start < 0)
		goto fail;

	return 0;

fail:
	git_error_set(GIT_ERROR_INVALID, "malformed hunk header from xdiff");
	return -1;
}

typedef struct {
	git_xdiff_output *xo;
	git_patch_generated *patch;
	git_diff_hunk hunk;
	int old_lineno, new_lineno;
	mmfile_t xd_old_data, xd_new_data;
} git_xdiff_info;

static int diff_update_lines(
	git_xdiff_info *info,
	git_diff_line *line,
	const char *content,
	size_t content_len)
{
	const char *scan = content, *scan_end = content + content_len;

	for (line->num_lines = 0; scan < scan_end; ++scan)
		if (*scan == '\n')
			++line->num_lines;

	line->content     = content;
	line->content_len = content_len;

	/* expect " "/"-"/"+", then data */
	switch (line->origin) {
	case GIT_DIFF_LINE_ADDITION:
	case GIT_DIFF_LINE_DEL_EOFNL:
		line->old_lineno = -1;
		line->new_lineno = info->new_lineno;
		info->new_lineno += (int)line->num_lines;
		break;
	case GIT_DIFF_LINE_DELETION:
	case GIT_DIFF_LINE_ADD_EOFNL:
		line->old_lineno = info->old_lineno;
		line->new_lineno = -1;
		info->old_lineno += (int)line->num_lines;
		break;
	case GIT_DIFF_LINE_CONTEXT:
	case GIT_DIFF_LINE_CONTEXT_EOFNL:
		line->old_lineno = info->old_lineno;
		line->new_lineno = info->new_lineno;
		info->old_lineno += (int)line->num_lines;
		info->new_lineno += (int)line->num_lines;
		break;
	default:
		git_error_set(GIT_ERROR_INVALID, "unknown diff line origin %02x",
			(unsigned int)line->origin);
		return -1;
	}

	return 0;
}

static int git_xdiff_cb(void *priv, mmbuffer_t *bufs, int len)
{
	git_xdiff_info *info = priv;
	git_patch_generated *patch = info->patch;
	const git_diff_delta *delta = patch->base.delta;
	git_patch_generated_output *output = &info->xo->output;
	git_diff_line line;
	size_t buffer_len;

	if (len == 1) {
		output->error = git_xdiff_parse_hunk(&info->hunk, bufs[0].ptr);
		if (output->error < 0)
			return output->error;

		info->hunk.header_len = bufs[0].size;
		if (info->hunk.header_len >= sizeof(info->hunk.header))
			info->hunk.header_len = sizeof(info->hunk.header) - 1;

		/* Sanitize the hunk header in case there is invalid Unicode */
		buffer_len = git_utf8_valid_buf_length(bufs[0].ptr, info->hunk.header_len);
		/* Sanitizing the hunk header may delete the newline, so add it back again if there is room */
		if (buffer_len < info->hunk.header_len) {
			bufs[0].ptr[buffer_len] = '\n';
			buffer_len += 1;
			info->hunk.header_len = buffer_len;
		}

		memcpy(info->hunk.header, bufs[0].ptr, info->hunk.header_len);
		info->hunk.header[info->hunk.header_len] = '\0';

		if (output->hunk_cb != NULL &&
			(output->error = output->hunk_cb(
				delta, &info->hunk, output->payload)))
			return output->error;

		info->old_lineno = info->hunk.old_start;
		info->new_lineno = info->hunk.new_start;
	}

	if (len == 2 || len == 3) {
		/* expect " "/"-"/"+", then data */
		line.origin =
			(*bufs[0].ptr == '+') ? GIT_DIFF_LINE_ADDITION :
			(*bufs[0].ptr == '-') ? GIT_DIFF_LINE_DELETION :
			GIT_DIFF_LINE_CONTEXT;

		if (line.origin == GIT_DIFF_LINE_ADDITION)
			line.content_offset = bufs[1].ptr - info->xd_new_data.ptr;
		else if (line.origin == GIT_DIFF_LINE_DELETION)
			line.content_offset = bufs[1].ptr - info->xd_old_data.ptr;
		else
			line.content_offset = -1;

		output->error = diff_update_lines(
			info, &line, bufs[1].ptr, bufs[1].size);

		if (!output->error && output->data_cb != NULL)
			output->error = output->data_cb(
				delta, &info->hunk, &line, output->payload);
	}

	if (len == 3 && !output->error) {
		/* If we have a '+' and a third buf, then we have added a line
		 * without a newline and the old code had one, so DEL_EOFNL.
		 * If we have a '-' and a third buf, then we have removed a line
		 * with out a newline but added a blank line, so ADD_EOFNL.
		 */
		line.origin =
			(*bufs[0].ptr == '+') ? GIT_DIFF_LINE_DEL_EOFNL :
			(*bufs[0].ptr == '-') ? GIT_DIFF_LINE_ADD_EOFNL :
			GIT_DIFF_LINE_CONTEXT_EOFNL;

		line.content_offset = -1;

		output->error = diff_update_lines(
			info, &line, bufs[2].ptr, bufs[2].size);

		if (!output->error && output->data_cb != NULL)
			output->error = output->data_cb(
				delta, &info->hunk, &line, output->payload);
	}

	return output->error;
}

static int git_xdiff(git_patch_generated_output *output, git_patch_generated *patch)
{
	git_xdiff_output *xo = (git_xdiff_output *)output;
	git_xdiff_info info;
	git_diff_find_context_payload findctxt;

	memset(&info, 0, sizeof(info));
	info.patch = patch;
	info.xo    = xo;

	xo->callback.priv = &info;

	git_diff_find_context_init(
		&xo->config.find_func, &findctxt, git_patch_generated_driver(patch));
	xo->config.find_func_priv = &findctxt;

	if (xo->config.find_func != NULL)
		xo->config.flags |= XDL_EMIT_FUNCNAMES;
	else
		xo->config.flags &= ~XDL_EMIT_FUNCNAMES;

	/* TODO: check ofile.opts_flags to see if driver-specific per-file
	 * updates are needed to xo->params.flags
	 */

	if (git_patch_generated_old_data(&info.xd_old_data.ptr, &info.xd_old_data.size, patch) < 0 ||
	    git_patch_generated_new_data(&info.xd_new_data.ptr, &info.xd_new_data.size, patch) < 0)
		return -1;

	xdl_diff(&info.xd_old_data, &info.xd_new_data,
		&xo->params, &xo->config, &xo->callback);

	git_diff_find_context_clear(&findctxt);

	return xo->output.error;
}

void git_xdiff_init(git_xdiff_output *xo, const git_diff_options *opts)
{
	uint32_t flags = opts ? opts->flags : 0;

	xo->output.diff_cb = git_xdiff;

	xo->config.ctxlen = opts ? opts->context_lines : 3;
	xo->config.interhunkctxlen = opts ? opts->interhunk_lines : 0;

	if (flags & GIT_DIFF_IGNORE_WHITESPACE)
		xo->params.flags |= XDF_WHITESPACE_FLAGS;
	if (flags & GIT_DIFF_IGNORE_WHITESPACE_CHANGE)
		xo->params.flags |= XDF_IGNORE_WHITESPACE_CHANGE;
	if (flags & GIT_DIFF_IGNORE_WHITESPACE_EOL)
		xo->params.flags |= XDF_IGNORE_WHITESPACE_AT_EOL;
	if (flags & GIT_DIFF_INDENT_HEURISTIC)
		xo->params.flags |= XDF_INDENT_HEURISTIC;

	if (flags & GIT_DIFF_PATIENCE)
		xo->params.flags |= XDF_PATIENCE_DIFF;
	if (flags & GIT_DIFF_MINIMAL)
		xo->params.flags |= XDF_NEED_MINIMAL;

	if (flags & GIT_DIFF_IGNORE_BLANK_LINES)
		xo->params.flags |= XDF_IGNORE_BLANK_LINES;

	xo->callback.out_line = git_xdiff_cb;
}
