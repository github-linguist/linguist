/*
* Copyright (C) the libgit2 contributors. All rights reserved.
*
* This file is part of libgit2, distributed under the GNU GPL v2 with
* a Linking Exception. For full terms see the included COPYING file.
*/

#include "patch.h"

#include "git2/patch.h"
#include "diff.h"

int git_patch__invoke_callbacks(
	git_patch *patch,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb line_cb,
	void *payload)
{
	int error = 0;
	uint32_t i, j;

	if (file_cb)
		error = file_cb(patch->delta, 0, payload);

	if (error)
		return error;

	if ((patch->delta->flags & GIT_DIFF_FLAG_BINARY) != 0) {
		if (binary_cb)
			error = binary_cb(patch->delta, &patch->binary, payload);

		return error;
	}

	if (!hunk_cb && !line_cb)
		return error;

	for (i = 0; !error && i < git_array_size(patch->hunks); ++i) {
		git_patch_hunk *h = git_array_get(patch->hunks, i);

		if (hunk_cb)
			error = hunk_cb(patch->delta, &h->hunk, payload);

		if (!line_cb)
			continue;

		for (j = 0; !error && j < h->line_count; ++j) {
			git_diff_line *l =
				git_array_get(patch->lines, h->line_start + j);

			error = line_cb(patch->delta, &h->hunk, l, payload);
		}
	}

	return error;
}

size_t git_patch_size(
	git_patch *patch,
	int include_context,
	int include_hunk_headers,
	int include_file_headers)
{
	size_t out;

	GIT_ASSERT_ARG(patch);

	out = patch->content_size;

	if (!include_context)
		out -= patch->context_size;

	if (include_hunk_headers)
		out += patch->header_size;

	if (include_file_headers) {
		git_str file_header = GIT_STR_INIT;

		if (git_diff_delta__format_file_header(
			&file_header, patch->delta, NULL, NULL, 0, true) < 0)
			git_error_clear();
		else
			out += git_str_len(&file_header);

		git_str_dispose(&file_header);
	}

	return out;
}

int git_patch_line_stats(
	size_t *total_ctxt,
	size_t *total_adds,
	size_t *total_dels,
	const git_patch *patch)
{
	size_t totals[3], idx;

	memset(totals, 0, sizeof(totals));

	for (idx = 0; idx < git_array_size(patch->lines); ++idx) {
		git_diff_line *line = git_array_get(patch->lines, idx);
		if (!line)
			continue;

		switch (line->origin) {
		case GIT_DIFF_LINE_CONTEXT:  totals[0]++; break;
		case GIT_DIFF_LINE_ADDITION: totals[1]++; break;
		case GIT_DIFF_LINE_DELETION: totals[2]++; break;
		default:
			/* diff --stat and --numstat don't count EOFNL marks because
			* they will always be paired with a ADDITION or DELETION line.
			*/
			break;
		}
	}

	if (total_ctxt)
		*total_ctxt = totals[0];
	if (total_adds)
		*total_adds = totals[1];
	if (total_dels)
		*total_dels = totals[2];

	return 0;
}

const git_diff_delta *git_patch_get_delta(const git_patch *patch)
{
	GIT_ASSERT_ARG_WITH_RETVAL(patch, NULL);
	return patch->delta;
}

size_t git_patch_num_hunks(const git_patch *patch)
{
	GIT_ASSERT_ARG(patch);
	return git_array_size(patch->hunks);
}

static int patch_error_outofrange(const char *thing)
{
	git_error_set(GIT_ERROR_INVALID, "patch %s index out of range", thing);
	return GIT_ENOTFOUND;
}

int git_patch_get_hunk(
	const git_diff_hunk **out,
	size_t *lines_in_hunk,
	git_patch *patch,
	size_t hunk_idx)
{
	git_patch_hunk *hunk;
	GIT_ASSERT_ARG(patch);

	hunk = git_array_get(patch->hunks, hunk_idx);

	if (!hunk) {
		if (out) *out = NULL;
		if (lines_in_hunk) *lines_in_hunk = 0;
		return patch_error_outofrange("hunk");
	}

	if (out) *out = &hunk->hunk;
	if (lines_in_hunk) *lines_in_hunk = hunk->line_count;
	return 0;
}

int git_patch_num_lines_in_hunk(const git_patch *patch, size_t hunk_idx)
{
	git_patch_hunk *hunk;
	GIT_ASSERT_ARG(patch);

	if (!(hunk = git_array_get(patch->hunks, hunk_idx)))
		return patch_error_outofrange("hunk");
	return (int)hunk->line_count;
}

int git_patch_get_line_in_hunk(
	const git_diff_line **out,
	git_patch *patch,
	size_t hunk_idx,
	size_t line_of_hunk)
{
	git_patch_hunk *hunk;
	git_diff_line *line;

	GIT_ASSERT_ARG(patch);

	if (!(hunk = git_array_get(patch->hunks, hunk_idx))) {
		if (out) *out = NULL;
		return patch_error_outofrange("hunk");
	}

	if (line_of_hunk >= hunk->line_count ||
		!(line = git_array_get(
			patch->lines, hunk->line_start + line_of_hunk))) {
		if (out) *out = NULL;
		return patch_error_outofrange("line");
	}

	if (out) *out = line;
	return 0;
}

git_repository *git_patch_owner(const git_patch *patch)
{
	return patch->repo;
}

int git_patch_from_diff(git_patch **out, git_diff *diff, size_t idx)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(diff);
	GIT_ASSERT_ARG(diff->patch_fn);
	return diff->patch_fn(out, diff, idx);
}

static void git_patch__free(git_patch *patch)
{
	if (patch->free_fn)
		patch->free_fn(patch);
}

void git_patch_free(git_patch *patch)
{
	if (patch)
		GIT_REFCOUNT_DEC(patch, git_patch__free);
}
