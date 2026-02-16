/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "buf.h"
#include "diff.h"
#include "diff_file.h"
#include "patch_generate.h"
#include "futils.h"
#include "zstream.h"
#include "blob.h"
#include "delta.h"
#include "repository.h"
#include "git2/sys/diff.h"

typedef struct {
	git_diff_format_t format;
	git_diff_line_cb print_cb;
	void *payload;

	git_str *buf;
	git_diff_line line;

	const char *old_prefix;
	const char *new_prefix;
	uint32_t flags;
	int id_strlen;
	unsigned int sent_file_header;
	git_oid_t oid_type;

	int (*strcomp)(const char *, const char *);
} diff_print_info;

static int diff_print_info_init__common(
	diff_print_info *pi,
	git_str *out,
	git_repository *repo,
	git_diff_format_t format,
	git_diff_line_cb cb,
	void *payload)
{
	pi->format = format;
	pi->print_cb = cb;
	pi->payload = payload;
	pi->buf = out;

	GIT_ASSERT(pi->oid_type);

	if (!pi->id_strlen) {
		if (!repo)
			pi->id_strlen = GIT_ABBREV_DEFAULT;
		else if (git_repository__abbrev_length(&pi->id_strlen, repo) < 0)
			return -1;
	}

	memset(&pi->line, 0, sizeof(pi->line));
	pi->line.old_lineno = -1;
	pi->line.new_lineno = -1;
	pi->line.num_lines = 1;

	return 0;
}

static int diff_print_info_init_fromdiff(
	diff_print_info *pi,
	git_str *out,
	git_diff *diff,
	git_diff_format_t format,
	git_diff_line_cb cb,
	void *payload)
{
	git_repository *repo = diff ? diff->repo : NULL;

	memset(pi, 0, sizeof(diff_print_info));

	if (diff) {
		pi->flags = diff->opts.flags;
		pi->oid_type = diff->opts.oid_type;
		pi->id_strlen = diff->opts.id_abbrev;
		pi->old_prefix = diff->opts.old_prefix;
		pi->new_prefix = diff->opts.new_prefix;

		pi->strcomp = diff->strcomp;
	}

	return diff_print_info_init__common(pi, out, repo, format, cb, payload);
}

static int diff_print_info_init_frompatch(
	diff_print_info *pi,
	git_str *out,
	git_patch *patch,
	git_diff_format_t format,
	git_diff_line_cb cb,
	void *payload)
{
	GIT_ASSERT_ARG(patch);

	memset(pi, 0, sizeof(diff_print_info));

	pi->flags = patch->diff_opts.flags;
	pi->oid_type = patch->diff_opts.oid_type;
	pi->id_strlen = patch->diff_opts.id_abbrev;
	pi->old_prefix = patch->diff_opts.old_prefix;
	pi->new_prefix = patch->diff_opts.new_prefix;

	return diff_print_info_init__common(pi, out, patch->repo, format, cb, payload);
}

static char diff_pick_suffix(int mode)
{
	if (S_ISDIR(mode))
		return '/';
	else if (GIT_PERMS_IS_EXEC(mode)) /* -V536 */
		/* in git, modes are very regular, so we must have 0100755 mode */
		return '*';
	else
		return ' ';
}

char git_diff_status_char(git_delta_t status)
{
	char code;

	switch (status) {
	case GIT_DELTA_ADDED:      code = 'A'; break;
	case GIT_DELTA_DELETED:    code = 'D'; break;
	case GIT_DELTA_MODIFIED:   code = 'M'; break;
	case GIT_DELTA_RENAMED:    code = 'R'; break;
	case GIT_DELTA_COPIED:     code = 'C'; break;
	case GIT_DELTA_IGNORED:    code = 'I'; break;
	case GIT_DELTA_UNTRACKED:  code = '?'; break;
	case GIT_DELTA_TYPECHANGE: code = 'T'; break;
	case GIT_DELTA_UNREADABLE: code = 'X'; break;
	default:                   code = ' '; break;
	}

	return code;
}

static int diff_print_one_name_only(
	const git_diff_delta *delta, float progress, void *data)
{
	diff_print_info *pi = data;
	git_str *out = pi->buf;

	GIT_UNUSED(progress);

	if ((pi->flags & GIT_DIFF_SHOW_UNMODIFIED) == 0 &&
		delta->status == GIT_DELTA_UNMODIFIED)
		return 0;

	git_str_clear(out);
	git_str_puts(out, delta->new_file.path);
	git_str_putc(out, '\n');
	if (git_str_oom(out))
		return -1;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_str_cstr(out);
	pi->line.content_len = git_str_len(out);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_one_name_status(
	const git_diff_delta *delta, float progress, void *data)
{
	diff_print_info *pi = data;
	git_str *out = pi->buf;
	char old_suffix, new_suffix, code = git_diff_status_char(delta->status);
	int(*strcomp)(const char *, const char *) = pi->strcomp ?
		pi->strcomp : git__strcmp;

	GIT_UNUSED(progress);

	if ((pi->flags & GIT_DIFF_SHOW_UNMODIFIED) == 0 && code == ' ')
		return 0;

	old_suffix = diff_pick_suffix(delta->old_file.mode);
	new_suffix = diff_pick_suffix(delta->new_file.mode);

	git_str_clear(out);

	if (delta->old_file.path != delta->new_file.path &&
		strcomp(delta->old_file.path,delta->new_file.path) != 0)
		git_str_printf(out, "%c\t%s%c %s%c\n", code,
			delta->old_file.path, old_suffix, delta->new_file.path, new_suffix);
	else if (delta->old_file.mode != delta->new_file.mode &&
		delta->old_file.mode != 0 && delta->new_file.mode != 0)
		git_str_printf(out, "%c\t%s%c %s%c\n", code,
			delta->old_file.path, old_suffix, delta->new_file.path, new_suffix);
	else if (old_suffix != ' ')
		git_str_printf(out, "%c\t%s%c\n", code, delta->old_file.path, old_suffix);
	else
		git_str_printf(out, "%c\t%s\n", code, delta->old_file.path);
	if (git_str_oom(out))
		return -1;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_str_cstr(out);
	pi->line.content_len = git_str_len(out);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_one_raw(
	const git_diff_delta *delta, float progress, void *data)
{
	diff_print_info *pi = data;
	git_str *out = pi->buf;
	int id_abbrev;
	char code = git_diff_status_char(delta->status);
	char start_oid[GIT_OID_MAX_HEXSIZE + 1],
	     end_oid[GIT_OID_MAX_HEXSIZE + 1];
	size_t oid_hexsize;
	bool id_is_abbrev;

	GIT_UNUSED(progress);

	if ((pi->flags & GIT_DIFF_SHOW_UNMODIFIED) == 0 && code == ' ')
		return 0;

	git_str_clear(out);

	id_abbrev = delta->old_file.mode ? delta->old_file.id_abbrev :
		delta->new_file.id_abbrev;

	if (pi->id_strlen > id_abbrev) {
		git_error_set(GIT_ERROR_PATCH,
			"the patch input contains %d id characters (cannot print %d)",
			id_abbrev, pi->id_strlen);
		return -1;
	}

#ifdef GIT_EXPERIMENTAL_SHA256
	GIT_ASSERT(delta->old_file.id.type == delta->new_file.id.type);
	oid_hexsize = git_oid_hexsize(delta->old_file.id.type);
#else
	oid_hexsize = GIT_OID_SHA1_HEXSIZE;
#endif

	id_is_abbrev = (pi->id_strlen > 0 &&
	                (size_t)pi->id_strlen <= oid_hexsize);

	git_oid_tostr(start_oid, pi->id_strlen + 1, &delta->old_file.id);
	git_oid_tostr(end_oid, pi->id_strlen + 1, &delta->new_file.id);

	git_str_printf(out,
		id_is_abbrev ? ":%06o %06o %s... %s... %c" : ":%06o %06o %s %s %c",
		delta->old_file.mode, delta->new_file.mode, start_oid, end_oid, code);

	if (delta->similarity > 0)
		git_str_printf(out, "%03u", delta->similarity);

	if (delta->old_file.path != delta->new_file.path)
		git_str_printf(
			out, "\t%s %s\n", delta->old_file.path, delta->new_file.path);
	else
		git_str_printf(
			out, "\t%s\n", delta->old_file.path ?
			delta->old_file.path : delta->new_file.path);

	if (git_str_oom(out))
		return -1;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_str_cstr(out);
	pi->line.content_len = git_str_len(out);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_modes(
	git_str *out, const git_diff_delta *delta)
{
	git_str_printf(out, "old mode %o\n", delta->old_file.mode);
	git_str_printf(out, "new mode %o\n", delta->new_file.mode);

	return git_str_oom(out) ? -1 : 0;
}

static int diff_print_oid_range(
	git_str *out, const git_diff_delta *delta, int id_strlen,
	bool print_index)
{
	char start_oid[GIT_OID_MAX_HEXSIZE + 1],
	     end_oid[GIT_OID_MAX_HEXSIZE + 1];

	if (delta->old_file.mode &&
			id_strlen > delta->old_file.id_abbrev) {
		git_error_set(GIT_ERROR_PATCH,
			"the patch input contains %d id characters (cannot print %d)",
			delta->old_file.id_abbrev, id_strlen);
		return -1;
	}

	if ((delta->new_file.mode &&
			id_strlen > delta->new_file.id_abbrev)) {
		git_error_set(GIT_ERROR_PATCH,
			"the patch input contains %d id characters (cannot print %d)",
			delta->new_file.id_abbrev, id_strlen);
		return -1;
	}

	git_oid_tostr(start_oid, id_strlen + 1, &delta->old_file.id);
	git_oid_tostr(end_oid, id_strlen + 1, &delta->new_file.id);

	if (delta->old_file.mode == delta->new_file.mode) {
		if (print_index)
			git_str_printf(out, "index %s..%s %o\n",
				start_oid, end_oid, delta->old_file.mode);
	} else {
		if (delta->old_file.mode == 0)
			git_str_printf(out, "new file mode %o\n", delta->new_file.mode);
		else if (delta->new_file.mode == 0)
			git_str_printf(out, "deleted file mode %o\n", delta->old_file.mode);
		else
			diff_print_modes(out, delta);

		if (print_index)
			git_str_printf(out, "index %s..%s\n", start_oid, end_oid);
	}

	return git_str_oom(out) ? -1 : 0;
}

static int diff_delta_format_path(
	git_str *out, const char *prefix, const char *filename)
{
	if (!filename) {
		/* don't prefix "/dev/null" */
 		return git_str_puts(out, "/dev/null");
	}

	if (git_str_joinpath(out, prefix, filename) < 0)
		return -1;

	return git_str_quote(out);
}

static int diff_delta_format_with_paths(
	git_str *out,
	const git_diff_delta *delta,
	const char *template,
	const char *oldpath,
	const char *newpath)
{
	if (git_oid_is_zero(&delta->old_file.id))
		oldpath = "/dev/null";

	if (git_oid_is_zero(&delta->new_file.id))
		newpath = "/dev/null";

	return git_str_printf(out, template, oldpath, newpath);
}

static int diff_delta_format_similarity_header(
	git_str *out,
	const git_diff_delta *delta)
{
	git_str old_path = GIT_STR_INIT, new_path = GIT_STR_INIT;
	const char *type;
	int error = 0;

	if (delta->similarity > 100) {
		git_error_set(GIT_ERROR_PATCH, "invalid similarity %d", delta->similarity);
		error = -1;
		goto done;
	}

	GIT_ASSERT(delta->status == GIT_DELTA_RENAMED || delta->status == GIT_DELTA_COPIED);
	if (delta->status == GIT_DELTA_RENAMED)
		type = "rename";
	else
		type = "copy";

	if ((error = git_str_puts(&old_path, delta->old_file.path)) < 0 ||
	    (error = git_str_puts(&new_path, delta->new_file.path)) < 0 ||
	    (error = git_str_quote(&old_path)) < 0 ||
	    (error = git_str_quote(&new_path)) < 0)
		goto done;

	git_str_printf(out,
		"similarity index %d%%\n"
		"%s from %s\n"
		"%s to %s\n",
		delta->similarity,
		type, old_path.ptr,
		type, new_path.ptr);

	if (git_str_oom(out))
		error = -1;

done:
	git_str_dispose(&old_path);
	git_str_dispose(&new_path);

	return error;
}

static bool delta_is_unchanged(const git_diff_delta *delta)
{
	if (git_oid_is_zero(&delta->old_file.id) &&
		git_oid_is_zero(&delta->new_file.id))
		return true;

	if (delta->old_file.mode == GIT_FILEMODE_COMMIT ||
		delta->new_file.mode == GIT_FILEMODE_COMMIT)
		return false;

	if (git_oid_equal(&delta->old_file.id, &delta->new_file.id))
		return true;

	return false;
}

int git_diff_delta__format_file_header(
	git_str *out,
	const git_diff_delta *delta,
	const char *oldpfx,
	const char *newpfx,
	int id_strlen,
	bool print_index)
{
	git_str old_path = GIT_STR_INIT, new_path = GIT_STR_INIT;
	bool unchanged = delta_is_unchanged(delta);
	int error = 0;

	if (!oldpfx)
		oldpfx = DIFF_OLD_PREFIX_DEFAULT;
	if (!newpfx)
		newpfx = DIFF_NEW_PREFIX_DEFAULT;
	if (!id_strlen)
		id_strlen = GIT_ABBREV_DEFAULT;

	if ((error = diff_delta_format_path(
			&old_path, oldpfx, delta->old_file.path)) < 0 ||
		(error = diff_delta_format_path(
			&new_path, newpfx, delta->new_file.path)) < 0)
		goto done;

	git_str_clear(out);

	git_str_printf(out, "diff --git %s %s\n",
		old_path.ptr, new_path.ptr);

	if (unchanged && delta->old_file.mode != delta->new_file.mode)
		diff_print_modes(out, delta);

	if (delta->status == GIT_DELTA_RENAMED ||
	    (delta->status == GIT_DELTA_COPIED && unchanged)) {
		if ((error = diff_delta_format_similarity_header(out, delta)) < 0)
			goto done;
	}

	if (!unchanged) {
		if ((error = diff_print_oid_range(out, delta,
						  id_strlen, print_index)) < 0)
			goto done;

		if ((delta->flags & GIT_DIFF_FLAG_BINARY) == 0)
			diff_delta_format_with_paths(out, delta,
				"--- %s\n+++ %s\n", old_path.ptr, new_path.ptr);
	}

	if (git_str_oom(out))
		error = -1;

done:
	git_str_dispose(&old_path);
	git_str_dispose(&new_path);

	return error;
}

static int format_binary(
	diff_print_info *pi,
	git_diff_binary_t type,
	const char *data,
	size_t datalen,
	size_t inflatedlen)
{
	const char *typename = type == GIT_DIFF_BINARY_DELTA ?
		"delta" : "literal";
	const char *scan, *end;

	git_str_printf(pi->buf, "%s %" PRIuZ "\n", typename, inflatedlen);
	pi->line.num_lines++;

	for (scan = data, end = data + datalen; scan < end; ) {
		size_t chunk_len = end - scan;
		if (chunk_len > 52)
			chunk_len = 52;

		if (chunk_len <= 26)
			git_str_putc(pi->buf, (char)chunk_len + 'A' - 1);
		else
			git_str_putc(pi->buf, (char)chunk_len - 26 + 'a' - 1);

		git_str_encode_base85(pi->buf, scan, chunk_len);
		git_str_putc(pi->buf, '\n');

		if (git_str_oom(pi->buf))
			return -1;

		scan += chunk_len;
		pi->line.num_lines++;
	}
	git_str_putc(pi->buf, '\n');

	if (git_str_oom(pi->buf))
		return -1;

	return 0;
}

static int diff_print_patch_file_binary_noshow(
	diff_print_info *pi, git_diff_delta *delta,
	const char *old_pfx, const char *new_pfx)
{
	git_str old_path = GIT_STR_INIT, new_path = GIT_STR_INIT;
	int error;

	if ((error = diff_delta_format_path(&old_path, old_pfx, delta->old_file.path)) < 0 ||
	    (error = diff_delta_format_path(&new_path, new_pfx, delta->new_file.path)) < 0 ||
	    (error = diff_delta_format_with_paths(pi->buf, delta, "Binary files %s and %s differ\n",
						  old_path.ptr, new_path.ptr)) < 0)
		goto done;

	pi->line.num_lines = 1;

done:
	git_str_dispose(&old_path);
	git_str_dispose(&new_path);
	return error;
}

static int diff_print_patch_file_binary(
	diff_print_info *pi, git_diff_delta *delta,
	const char *old_pfx, const char *new_pfx,
	const git_diff_binary *binary)
{
	size_t pre_binary_size;
	int error;

	if (delta->status == GIT_DELTA_UNMODIFIED)
		return 0;

	if ((pi->flags & GIT_DIFF_SHOW_BINARY) == 0 || !binary->contains_data)
		return diff_print_patch_file_binary_noshow(
			pi, delta, old_pfx, new_pfx);

	pre_binary_size = pi->buf->size;
	git_str_printf(pi->buf, "GIT binary patch\n");
	pi->line.num_lines++;

	if ((error = format_binary(pi, binary->new_file.type, binary->new_file.data,
				   binary->new_file.datalen, binary->new_file.inflatedlen)) < 0 ||
	    (error = format_binary(pi, binary->old_file.type, binary->old_file.data,
				   binary->old_file.datalen, binary->old_file.inflatedlen)) < 0) {
		if (error == GIT_EBUFS) {
			git_error_clear();
			git_str_truncate(pi->buf, pre_binary_size);

			return diff_print_patch_file_binary_noshow(
				pi, delta, old_pfx, new_pfx);
		}
	}

	pi->line.num_lines++;
	return error;
}

GIT_INLINE(int) should_force_header(const git_diff_delta *delta)
{
	if (delta->old_file.mode != delta->new_file.mode)
		return 1;

	if (delta->status == GIT_DELTA_RENAMED || delta->status == GIT_DELTA_COPIED)
		return 1;

	return 0;
}

GIT_INLINE(int) flush_file_header(const git_diff_delta *delta, diff_print_info *pi)
{
	if (pi->sent_file_header)
		return 0;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_str_cstr(pi->buf);
	pi->line.content_len = git_str_len(pi->buf);
	pi->sent_file_header = 1;

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_patch_file(
	const git_diff_delta *delta, float progress, void *data)
{
	int error;
	diff_print_info *pi = data;
	const char *oldpfx =
		pi->old_prefix ? pi->old_prefix : DIFF_OLD_PREFIX_DEFAULT;
	const char *newpfx =
		pi->new_prefix ? pi->new_prefix : DIFF_NEW_PREFIX_DEFAULT;

	bool binary = (delta->flags & GIT_DIFF_FLAG_BINARY) ||
		(pi->flags & GIT_DIFF_FORCE_BINARY);
	bool show_binary = !!(pi->flags & GIT_DIFF_SHOW_BINARY);
	int id_strlen = pi->id_strlen;
	bool print_index = (pi->format != GIT_DIFF_FORMAT_PATCH_ID);

	if (binary && show_binary)
		id_strlen = delta->old_file.id_abbrev ? delta->old_file.id_abbrev :
			delta->new_file.id_abbrev;

	GIT_UNUSED(progress);

	if (S_ISDIR(delta->new_file.mode) ||
	    delta->status == GIT_DELTA_UNMODIFIED ||
	    delta->status == GIT_DELTA_IGNORED ||
	    delta->status == GIT_DELTA_UNREADABLE ||
	    (delta->status == GIT_DELTA_UNTRACKED &&
		 (pi->flags & GIT_DIFF_SHOW_UNTRACKED_CONTENT) == 0))
		return 0;

	pi->sent_file_header = 0;

	if ((error = git_diff_delta__format_file_header(pi->buf, delta, oldpfx, newpfx,
							id_strlen, print_index)) < 0)
		return error;

	/*
	 * pi->buf now contains the file header data. Go ahead and send it
	 * if there's useful data in there, like similarity. Otherwise, we
	 * should queue it to send when we see the first hunk. This prevents
	 * us from sending a header when all hunks were ignored.
	 */
	if (should_force_header(delta) && (error = flush_file_header(delta, pi)) < 0)
		return error;

	return 0;
}

static int diff_print_patch_binary(
	const git_diff_delta *delta,
	const git_diff_binary *binary,
	void *data)
{
	diff_print_info *pi = data;
	const char *old_pfx =
		pi->old_prefix ? pi->old_prefix : DIFF_OLD_PREFIX_DEFAULT;
	const char *new_pfx =
		pi->new_prefix ? pi->new_prefix : DIFF_NEW_PREFIX_DEFAULT;
	int error;

	if ((error = flush_file_header(delta, pi)) < 0)
		return error;

	/*
	 * If the caller only wants the header, we just needed to make sure to
	 * call flush_file_header
	 */
	if (pi->format == GIT_DIFF_FORMAT_PATCH_HEADER)
		return 0;

	git_str_clear(pi->buf);

	if ((error = diff_print_patch_file_binary(
		pi, (git_diff_delta *)delta, old_pfx, new_pfx, binary)) < 0)
		return error;

	pi->line.origin = GIT_DIFF_LINE_BINARY;
	pi->line.content = git_str_cstr(pi->buf);
	pi->line.content_len = git_str_len(pi->buf);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_patch_hunk(
	const git_diff_delta *d,
	const git_diff_hunk *h,
	void *data)
{
	diff_print_info *pi = data;
	int error;

	if (S_ISDIR(d->new_file.mode))
		return 0;

	if ((error = flush_file_header(d, pi)) < 0)
		return error;

	/*
	 * If the caller only wants the header, we just needed to make sure to
	 * call flush_file_header
	 */
	if (pi->format == GIT_DIFF_FORMAT_PATCH_HEADER)
		return 0;

	pi->line.origin      = GIT_DIFF_LINE_HUNK_HDR;
	pi->line.content     = h->header;
	pi->line.content_len = h->header_len;

	return pi->print_cb(d, h, &pi->line, pi->payload);
}

static int diff_print_patch_line(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *data)
{
	diff_print_info *pi = data;
	int error;

	if (S_ISDIR(delta->new_file.mode))
		return 0;

	if ((error = flush_file_header(delta, pi)) < 0)
		return error;

	return pi->print_cb(delta, hunk, line, pi->payload);
}

/* print a git_diff to an output callback */
int git_diff_print(
	git_diff *diff,
	git_diff_format_t format,
	git_diff_line_cb print_cb,
	void *payload)
{
	int error;
	git_str buf = GIT_STR_INIT;
	diff_print_info pi;
	git_diff_file_cb print_file = NULL;
	git_diff_binary_cb print_binary = NULL;
	git_diff_hunk_cb print_hunk = NULL;
	git_diff_line_cb print_line = NULL;

	switch (format) {
	case GIT_DIFF_FORMAT_PATCH:
		print_file = diff_print_patch_file;
		print_binary = diff_print_patch_binary;
		print_hunk = diff_print_patch_hunk;
		print_line = diff_print_patch_line;
		break;
	case GIT_DIFF_FORMAT_PATCH_ID:
		print_file = diff_print_patch_file;
		print_binary = diff_print_patch_binary;
		print_line = diff_print_patch_line;
		break;
	case GIT_DIFF_FORMAT_PATCH_HEADER:
		print_file = diff_print_patch_file;
		print_binary = diff_print_patch_binary;
		print_hunk = diff_print_patch_hunk;
		break;
	case GIT_DIFF_FORMAT_RAW:
		print_file = diff_print_one_raw;
		break;
	case GIT_DIFF_FORMAT_NAME_ONLY:
		print_file = diff_print_one_name_only;
		break;
	case GIT_DIFF_FORMAT_NAME_STATUS:
		print_file = diff_print_one_name_status;
		break;
	default:
		git_error_set(GIT_ERROR_INVALID, "unknown diff output format (%d)", format);
		return -1;
	}

	if ((error = diff_print_info_init_fromdiff(&pi, &buf, diff, format, print_cb, payload)) < 0)
		goto out;

	if ((error = git_diff_foreach(diff, print_file, print_binary, print_hunk, print_line, &pi)) != 0) {
		git_error_set_after_callback_function(error, "git_diff_print");
		goto out;
	}

out:
	git_str_dispose(&buf);
	return error;
}

int git_diff_print_callback__to_buf(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	git_str *output = payload;
	GIT_UNUSED(delta); GIT_UNUSED(hunk);

	if (!output) {
		git_error_set(GIT_ERROR_INVALID, "buffer pointer must be provided");
		return -1;
	}

	if (line->origin == GIT_DIFF_LINE_ADDITION ||
	    line->origin == GIT_DIFF_LINE_DELETION ||
	    line->origin == GIT_DIFF_LINE_CONTEXT)
		git_str_putc(output, line->origin);

	return git_str_put(output, line->content, line->content_len);
}

int git_diff_print_callback__to_file_handle(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	FILE *fp = payload ? payload : stdout;
	int error;

	GIT_UNUSED(delta);
	GIT_UNUSED(hunk);

	if (line->origin == GIT_DIFF_LINE_CONTEXT ||
	    line->origin == GIT_DIFF_LINE_ADDITION ||
	    line->origin == GIT_DIFF_LINE_DELETION) {
		while ((error = fputc(line->origin, fp)) == EINTR)
			continue;
		if (error) {
			git_error_set(GIT_ERROR_OS, "could not write status");
			return -1;
		}
	}

	if (fwrite(line->content, line->content_len, 1, fp) != 1) {
		git_error_set(GIT_ERROR_OS, "could not write line");
		return -1;
	}

	return 0;
}

/* print a git_diff to a git_str */
int git_diff_to_buf(git_buf *out, git_diff *diff, git_diff_format_t format)
{
	git_str str = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(diff);

	if ((error = git_buf_tostr(&str, out)) < 0 ||
	    (error = git_diff_print(diff, format, git_diff_print_callback__to_buf, &str)) < 0)
		goto done;

	error = git_buf_fromstr(out, &str);

done:
	git_str_dispose(&str);
	return error;
}

/* print a git_patch to an output callback */
int git_patch_print(
	git_patch *patch,
	git_diff_line_cb print_cb,
	void *payload)
{
	git_str temp = GIT_STR_INIT;
	diff_print_info pi;
	int error;

	GIT_ASSERT_ARG(patch);
	GIT_ASSERT_ARG(print_cb);

	if ((error = diff_print_info_init_frompatch(&pi, &temp, patch,
						    GIT_DIFF_FORMAT_PATCH, print_cb, payload)) < 0)
		goto out;

	if ((error = git_patch__invoke_callbacks(patch, diff_print_patch_file, diff_print_patch_binary,
						 diff_print_patch_hunk, diff_print_patch_line, &pi)) < 0) {
		git_error_set_after_callback_function(error, "git_patch_print");
		goto out;
	}

out:
	git_str_dispose(&temp);
	return error;
}

/* print a git_patch to a git_str */
int git_patch_to_buf(git_buf *out, git_patch *patch)
{
	GIT_BUF_WRAP_PRIVATE(out, git_patch__to_buf, patch);
}

int git_patch__to_buf(git_str *out, git_patch *patch)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(patch);

	return git_patch_print(patch, git_diff_print_callback__to_buf, out);
}
