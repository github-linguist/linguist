/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stdio.h>
#include <git2.h>
#include "common.h"
#include "cmd.h"
#include "error.h"
#include "sighandler.h"
#include "progress.h"

#include "fs_path.h"
#include "futils.h"
#include "date.h"
#include "hashmap.h"

#define COMMAND_NAME "blame"

static char *file;
static int porcelain, line_porcelain;

static const cli_opt_spec opts[] = {
	CLI_COMMON_OPT,

	{ CLI_OPT_TYPE_SWITCH,    "porcelain",      'p', &porcelain, 1,
	  CLI_OPT_USAGE_DEFAULT,   NULL,            "show machine readable output" },
	{ CLI_OPT_TYPE_SWITCH,    "line-porcelain",  0,  &line_porcelain, 1,
	  CLI_OPT_USAGE_DEFAULT,   NULL,            "show individual lines in machine readable output" },
	{ CLI_OPT_TYPE_LITERAL },
	{ CLI_OPT_TYPE_ARG,       "file",            0,  &file, 0,
	  CLI_OPT_USAGE_REQUIRED, "file",           "file to blame" },

	{ 0 }
};

static void print_help(void)
{
	cli_opt_usage_fprint(stdout, PROGRAM_NAME, COMMAND_NAME, opts, 0);
	printf("\n");

	printf("Show the origin of each line of a file.\n");
	printf("\n");

	printf("Options:\n");

	cli_opt_help_fprint(stdout, opts);
}

static int strintlen(size_t n)
{
	int len = 1;

	while (n > 10) {
		n /= 10;
		len++;

		if (len == INT_MAX)
			break;
	}

	return len;
}

static int fmt_date(git_str *out, git_time_t time, int offset)
{
	time_t t;
	struct tm gmt;

	GIT_ASSERT_ARG(out);

	t = (time_t)(time + offset * 60);

	if (p_gmtime_r(&t, &gmt) == NULL)
		return -1;

	return git_str_printf(out, "%.4u-%02u-%02u %02u:%02u:%02u %+03d%02d",
		gmt.tm_year + 1900, gmt.tm_mon + 1, gmt.tm_mday,
		gmt.tm_hour, gmt.tm_min, gmt.tm_sec,
		offset / 60, offset % 60);
}

static int print_standard(git_blame *blame)
{
	size_t max_line_number = 0;
	int max_lineno_len, max_line_len, max_author_len = 0, max_path_len = 0;
	const char *last_path = NULL;
	const git_blame_line *line;
	bool paths_differ = false;
	git_str date_str = GIT_STR_INIT;
	size_t i;
	int ret = 0;

	/* Compute the maximum size of things */
	for (i = 0; i < git_blame_hunkcount(blame); i++) {
		const git_blame_hunk *hunk = git_blame_hunk_byindex(blame, i);
		size_t hunk_author_len = strlen(hunk->orig_signature->name);
		size_t hunk_path_len = strlen(hunk->orig_path);
		size_t hunk_max_line_number =
			hunk->orig_start_line_number + hunk->lines_in_hunk;

		if (hunk_max_line_number > max_line_number)
			max_line_number = hunk_max_line_number;

		if (hunk_author_len > INT_MAX)
			max_author_len = INT_MAX;
		else if ((int)hunk_author_len > max_author_len)
			max_author_len = (int)hunk_author_len;

		if (hunk_path_len > INT_MAX)
			hunk_path_len = INT_MAX;
		else if ((int)hunk_path_len > max_path_len)
			max_path_len = (int)hunk_path_len;

		if (!paths_differ && last_path != NULL &&
		    strcmp(last_path, hunk->orig_path) != 0) {
			paths_differ = true;
		}

		last_path = hunk->orig_path;
	}

	max_lineno_len = strintlen(max_line_number);

	max_author_len--;

	for (i = 1; i < git_blame_linecount(blame); i++) {
		const git_blame_hunk *hunk = git_blame_hunk_byline(blame, i);
		int oid_abbrev;

		if (!hunk)
			break;

		oid_abbrev = hunk->boundary ? 7 : 8;
		printf("%s%.*s ", hunk->boundary ? "^" : "",
			oid_abbrev, git_oid_tostr_s(&hunk->orig_commit_id));

		if (paths_differ)
			printf("%-*.*s ", max_path_len, max_path_len, hunk->orig_path);

		git_str_clear(&date_str);
		if (fmt_date(&date_str,
				hunk->orig_signature->when.time,
				hunk->orig_signature->when.offset) < 0) {
			ret = cli_error_git();
			goto done;
		}

		if ((line = git_blame_line_byindex(blame, i)) == NULL) {
			ret = cli_error_git();
			goto done;
		}

		max_line_len = (int)min(line->len, INT_MAX);

		printf("(%-*.*s %s %*" PRIuZ ") %.*s" ,
			max_author_len, max_author_len, hunk->orig_signature->name,
			date_str.ptr,
			max_lineno_len, i,
			max_line_len, line->ptr);
		printf("\n");
	}

done:
	git_str_dispose(&date_str);
	return ret;
}

GIT_INLINE(uint32_t) oid_hashcode(const git_oid *oid)
{
	uint32_t hash;
	memcpy(&hash, oid->id, sizeof(uint32_t));
	return hash;
}

GIT_HASHSET_SETUP(git_blame_commitmap, const git_oid *, oid_hashcode, git_oid_equal);

static int print_porcelain(git_blame *blame)
{
	git_blame_commitmap seen_ids = GIT_HASHSET_INIT;
	size_t i, j;

	for (i = 0; i < git_blame_hunkcount(blame); i++) {
		const git_blame_line *line;
		const git_blame_hunk *hunk = git_blame_hunk_byindex(blame, i);

		for (j = 0; j < hunk->lines_in_hunk; j++) {
			size_t line_number = hunk->final_start_line_number + j;
			bool seen = git_blame_commitmap_contains(&seen_ids, &hunk->orig_commit_id);

			printf("%s %" PRIuZ " %" PRIuZ,
				git_oid_tostr_s(&hunk->orig_commit_id),
				hunk->orig_start_line_number + j,
				hunk->final_start_line_number + j);

			if (!j)
				printf(" %" PRIuZ, hunk->lines_in_hunk);

			printf("\n");

			if ((!j && !seen) || line_porcelain) {
				printf("author %s\n", hunk->orig_signature->name);
				printf("author-mail <%s>\n", hunk->orig_signature->email);
				printf("author-time %" PRId64 "\n", hunk->orig_signature->when.time);
				printf("author-tz %+03d%02d\n",
					hunk->orig_signature->when.offset / 60,
					hunk->orig_signature->when.offset % 60);

				printf("committer %s\n", hunk->orig_committer->name);
				printf("committer-mail <%s>\n", hunk->orig_committer->email);
				printf("committer-time %" PRId64 "\n", hunk->orig_committer->when.time);
				printf("committer-tz %+03d%02d\n",
					hunk->orig_committer->when.offset / 60,
					hunk->orig_committer->when.offset % 60);

				printf("summary %s\n", hunk->summary);

				/* TODO: previous */

				printf("filename %s\n", hunk->orig_path);
			}

			if ((line = git_blame_line_byindex(blame, line_number)) == NULL)
				return cli_error_git();

			printf("\t%.*s\n", (int)min(line->len, INT_MAX),
				line->ptr);

			if (!seen)
				git_blame_commitmap_add(&seen_ids, &hunk->orig_commit_id);
		}
	}

	git_blame_commitmap_dispose(&seen_ids);
	return 0;
}

int cmd_blame(int argc, char **argv)
{
	cli_repository_open_options open_opts = { argv + 1, argc - 1 };
	git_blame_options blame_opts = GIT_BLAME_OPTIONS_INIT;
	git_repository *repo = NULL;
	git_str workdir_path = GIT_STR_INIT;
	git_blame *blame = NULL;
	cli_opt invalid_opt;
	int ret = 0;

	blame_opts.flags |= GIT_BLAME_USE_MAILMAP;

	if (cli_opt_parse(&invalid_opt, opts, argv + 1, argc - 1, CLI_OPT_PARSE_GNU))
		return cli_opt_usage_error(COMMAND_NAME, opts, &invalid_opt);

	if (cli_opt__show_help) {
		print_help();
		return 0;
	}

	if (!file) {
		ret = cli_error_usage("you must specify a file to blame");
		goto done;
	}

	if (cli_repository_open(&repo, &open_opts) < 0)
		return cli_error_git();

	if ((ret = cli_resolve_path(&workdir_path, repo, file)) != 0)
		goto done;

	if (git_blame_file(&blame, repo, workdir_path.ptr, &blame_opts) < 0) {
		ret = cli_error_git();
		goto done;
	}

	if (porcelain || line_porcelain)
		ret = print_porcelain(blame);
	else
		ret = print_standard(blame);

done:
	git_str_dispose(&workdir_path);
	git_blame_free(blame);
	git_repository_free(repo);
	return ret;
}
