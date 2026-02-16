/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <git2.h>
#include "common.h"
#include "cmd.h"
#include "progress.h"

#define COMMAND_NAME "index-pack"

#define BUFFER_SIZE (1024 * 1024)

static int verbose, read_stdin;
static char *filename;
static cli_progress progress = CLI_PROGRESS_INIT;

static const cli_opt_spec opts[] = {
	CLI_COMMON_OPT,

	{ CLI_OPT_TYPE_SWITCH,    "verbose", 'v', &verbose,    1,
	  CLI_OPT_USAGE_DEFAULT,   NULL,    "display progress output" },

	{ CLI_OPT_TYPE_LITERAL },

	{ CLI_OPT_TYPE_SWITCH,    "stdin",    0,   &read_stdin, 1,
	  CLI_OPT_USAGE_REQUIRED,  NULL,     "read from stdin" },
	{ CLI_OPT_TYPE_ARG,       "pack-file", 0,  &filename, 0,
	  CLI_OPT_USAGE_CHOICE,   "pack-file", "packfile path" },

	{ 0 },
};

static void print_help(void)
{
	cli_opt_usage_fprint(stdout, PROGRAM_NAME, COMMAND_NAME, opts, 0);
	printf("\n");

	printf("Indexes a packfile and writes the index to disk.\n");
	printf("\n");

	printf("Options:\n");

	cli_opt_help_fprint(stdout, opts);
}

int cmd_index_pack(int argc, char **argv)
{
	cli_opt invalid_opt;
	git_indexer *idx = NULL;
	git_indexer_options idx_opts = GIT_INDEXER_OPTIONS_INIT;
	git_indexer_progress stats = {0};
	char buf[BUFFER_SIZE];
	ssize_t read_len;
	int fd, ret;

	if (cli_opt_parse(&invalid_opt, opts, argv + 1, argc - 1, CLI_OPT_PARSE_GNU))
		return cli_opt_usage_error(COMMAND_NAME, opts, &invalid_opt);

	if (cli_opt__show_help) {
		print_help();
		return 0;
	}

	if (verbose) {
		idx_opts.progress_cb = cli_progress_indexer;
		idx_opts.progress_cb_payload = &progress;
	}

	if (read_stdin) {
		fd = fileno(stdin);
	} else if ((fd = p_open(filename, O_RDONLY)) < 0) {
		ret = cli_error_git();
		goto done;
	}

#ifdef GIT_EXPERIMENTAL_SHA256
	idx_opts.oid_type = GIT_OID_SHA1;

	ret = git_indexer_new(&idx, ".", &idx_opts);
#else
	ret = git_indexer_new(&idx, ".", 0, NULL, &idx_opts);
#endif

	if (ret < 0) {
		ret = cli_error_git();
		goto done;
	}

	while ((read_len = p_read(fd, buf, sizeof(buf))) > 0) {
		if (git_indexer_append(idx, buf, (size_t)read_len, &stats) < 0) {
			ret = cli_error_git();
			goto done;
		}
	}

	if (git_indexer_commit(idx, &stats) < 0) {
		ret = cli_error_git();
		goto done;
	}

	cli_progress_finish(&progress);

done:
	if (!read_stdin && fd >= 0)
		p_close(fd);

	cli_progress_dispose(&progress);
	git_indexer_free(idx);
	return ret;
}
