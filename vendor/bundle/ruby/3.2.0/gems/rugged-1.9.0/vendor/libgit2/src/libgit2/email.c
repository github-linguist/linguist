/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "email.h"

#include "common.h"
#include "buf.h"
#include "diff_generate.h"
#include "diff_stats.h"
#include "patch.h"
#include "date.h"

#include "git2/email.h"
#include "git2/patch.h"
#include "git2/sys/email.h"
#include "git2/version.h"

/*
 * Git uses a "magic" timestamp to indicate that an email message
 * is from `git format-patch` (or our equivalent).
 */
#define EMAIL_TIMESTAMP "Mon Sep 17 00:00:00 2001"

GIT_INLINE(int) include_prefix(
	size_t patch_count,
	git_email_create_options *opts)
{
	return ((!opts->subject_prefix || *opts->subject_prefix) ||
	        (opts->flags & GIT_EMAIL_CREATE_ALWAYS_NUMBER) != 0 ||
	        opts->reroll_number ||
		(patch_count > 1 && !(opts->flags & GIT_EMAIL_CREATE_OMIT_NUMBERS)));
}

static int append_prefix(
	git_str *out,
	size_t patch_idx,
	size_t patch_count,
	git_email_create_options *opts)
{
	const char *subject_prefix = opts->subject_prefix ?
		opts->subject_prefix : "PATCH";

	git_str_putc(out, '[');

	if (*subject_prefix)
		git_str_puts(out, subject_prefix);

	if (opts->reroll_number) {
		if (*subject_prefix)
			git_str_putc(out, ' ');

		git_str_printf(out, "v%" PRIuZ, opts->reroll_number);
	}

	if ((opts->flags & GIT_EMAIL_CREATE_ALWAYS_NUMBER) != 0 ||
	    (patch_count > 1 && !(opts->flags & GIT_EMAIL_CREATE_OMIT_NUMBERS))) {
		size_t start_number = opts->start_number ?
			opts->start_number : 1;

		if (*subject_prefix || opts->reroll_number)
			git_str_putc(out, ' ');

		git_str_printf(out, "%" PRIuZ "/%" PRIuZ,
		               patch_idx + (start_number - 1),
		               patch_count + (start_number - 1));
	}

	git_str_puts(out, "]");

	return git_str_oom(out) ? -1 : 0;
}

static int append_date(
	git_str *out,
	const git_time *date)
{
	int error;

	if ((error = git_str_printf(out, "Date: ")) == 0 &&
	    (error = git_date_rfc2822_fmt(out, date->time, date->offset)) == 0)
	    error = git_str_putc(out, '\n');

	return error;
}

static int append_subject(
	git_str *out,
	size_t patch_idx,
	size_t patch_count,
	const char *summary,
	git_email_create_options *opts)
{
	bool prefix = include_prefix(patch_count, opts);
	size_t summary_len = summary ? strlen(summary) : 0;
	int error;

	if (summary_len) {
		const char *nl = strchr(summary, '\n');

		if (nl)
			summary_len = (nl - summary);
	}

	if ((error = git_str_puts(out, "Subject: ")) < 0)
		return error;

	if (prefix &&
	    (error = append_prefix(out, patch_idx, patch_count, opts)) < 0)
		return error;

	if (prefix && summary_len && (error = git_str_putc(out, ' ')) < 0)
		return error;

	if (summary_len &&
	    (error = git_str_put(out, summary, summary_len)) < 0)
		return error;

	return git_str_putc(out, '\n');
}

static int append_header(
	git_str *out,
	size_t patch_idx,
	size_t patch_count,
	const git_oid *commit_id,
	const char *summary,
	const git_signature *author,
	git_email_create_options *opts)
{
	char id[GIT_OID_MAX_HEXSIZE + 1];
	int error;

	git_oid_tostr(id, GIT_OID_MAX_HEXSIZE + 1, commit_id);

	if ((error = git_str_printf(out, "From %s %s\n", id, EMAIL_TIMESTAMP)) < 0 ||
	    (error = git_str_printf(out, "From: %s <%s>\n", author->name, author->email)) < 0 ||
	    (error = append_date(out, &author->when)) < 0 ||
	    (error = append_subject(out, patch_idx, patch_count, summary, opts)) < 0)
		return error;

	if ((error = git_str_putc(out, '\n')) < 0)
		return error;

	return 0;
}

static int append_body(git_str *out, const char *body)
{
	size_t body_len;
	int error;

	if (!body)
		return 0;

	body_len = strlen(body);

	if ((error = git_str_puts(out, body)) < 0)
		return error;

	if (body_len && body[body_len - 1] != '\n')
		error = git_str_putc(out, '\n');

	return error;
}

static int append_diffstat(git_str *out, git_diff *diff)
{
	git_diff_stats *stats = NULL;
	unsigned int format_flags;
	int error;

	format_flags = GIT_DIFF_STATS_FULL | GIT_DIFF_STATS_INCLUDE_SUMMARY;

	if ((error = git_diff_get_stats(&stats, diff)) == 0 &&
	    (error = git_diff__stats_to_buf(out, stats, format_flags, 0)) == 0)
		error = git_str_putc(out, '\n');

	git_diff_stats_free(stats);
	return error;
}

static int append_patches(git_str *out, git_diff *diff)
{
	size_t i, deltas;
	int error = 0;

	deltas = git_diff_num_deltas(diff);

	for (i = 0; i < deltas; ++i) {
		git_patch *patch = NULL;

		if ((error = git_patch_from_diff(&patch, diff, i)) >= 0)
			error = git_patch__to_buf(out, patch);

		git_patch_free(patch);

		if (error < 0)
			break;
	}

	return error;
}

int git_email__append_from_diff(
	git_str *out,
	git_diff *diff,
	size_t patch_idx,
	size_t patch_count,
	const git_oid *commit_id,
	const char *summary,
	const char *body,
	const git_signature *author,
	const git_email_create_options *given_opts)
{
	git_email_create_options opts = GIT_EMAIL_CREATE_OPTIONS_INIT;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(diff);
	GIT_ASSERT_ARG(!patch_idx || patch_idx <= patch_count);
	GIT_ASSERT_ARG(commit_id);
	GIT_ASSERT_ARG(author);

	GIT_ERROR_CHECK_VERSION(given_opts,
		GIT_EMAIL_CREATE_OPTIONS_VERSION,
		"git_email_create_options");

	if (given_opts)
		memcpy(&opts, given_opts, sizeof(git_email_create_options));

	if ((error = append_header(out, patch_idx, patch_count, commit_id, summary, author, &opts)) == 0 &&
	    (error = append_body(out, body)) == 0 &&
	    (error = git_str_puts(out, "---\n")) == 0 &&
	    (error = append_diffstat(out, diff)) == 0 &&
	    (error = append_patches(out, diff)) == 0)
		error = git_str_puts(out, "--\nlibgit2 " LIBGIT2_VERSION "\n\n");

	return error;
}

int git_email_create_from_diff(
	git_buf *out,
	git_diff *diff,
	size_t patch_idx,
	size_t patch_count,
	const git_oid *commit_id,
	const char *summary,
	const char *body,
	const git_signature *author,
	const git_email_create_options *given_opts)
{
	git_str email = GIT_STR_INIT;
	int error;

	git_buf_tostr(&email, out);

	error = git_email__append_from_diff(&email, diff, patch_idx,
		patch_count, commit_id, summary, body, author,
		given_opts);

	if (error == 0)
		error = git_buf_fromstr(out, &email);

	git_str_dispose(&email);
	return error;
}

int git_email_create_from_commit(
	git_buf *out,
	git_commit *commit,
	const git_email_create_options *given_opts)
{
	git_email_create_options opts = GIT_EMAIL_CREATE_OPTIONS_INIT;
	git_diff *diff = NULL;
	git_repository *repo;
	git_diff_options *diff_opts;
	git_diff_find_options *find_opts;
	const git_signature *author;
	const char *summary, *body;
	const git_oid *commit_id;
	int error = -1;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(commit);

	GIT_ERROR_CHECK_VERSION(given_opts,
		GIT_EMAIL_CREATE_OPTIONS_VERSION,
		"git_email_create_options");

	if (given_opts)
		memcpy(&opts, given_opts, sizeof(git_email_create_options));

	repo = git_commit_owner(commit);
	author = git_commit_author(commit);
	summary = git_commit_summary(commit);
	body = git_commit_body(commit);
	commit_id = git_commit_id(commit);
	diff_opts = &opts.diff_opts;
	find_opts = &opts.diff_find_opts;

	if ((error = git_diff__commit(&diff, repo, commit, diff_opts)) < 0)
		goto done;

	if ((opts.flags & GIT_EMAIL_CREATE_NO_RENAMES) == 0 &&
	    (error = git_diff_find_similar(diff, find_opts)) < 0)
		goto done;

	error = git_email_create_from_diff(out, diff, 1, 1, commit_id, summary, body, author, &opts);

done:
	git_diff_free(diff);
	return error;
}
