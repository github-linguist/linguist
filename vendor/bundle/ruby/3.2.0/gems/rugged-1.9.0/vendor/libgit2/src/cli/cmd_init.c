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

#define COMMAND_NAME "init"

static char *branch, *git_dir, *template_dir, *path;
static int quiet, bare;

static const cli_opt_spec opts[] = {
	CLI_COMMON_OPT,

	{ CLI_OPT_TYPE_SWITCH,    "quiet",           'q', &quiet,       1,
	  CLI_OPT_USAGE_DEFAULT,   NULL,             "quiet mode; don't display informational messages" },
	{ CLI_OPT_TYPE_SWITCH,    "bare",             0,  &bare,        1,
	  CLI_OPT_USAGE_DEFAULT,   NULL,             "don't create a working directory" },
	{ CLI_OPT_TYPE_VALUE,     "initial-branch",  'b', &branch,      0,
	  CLI_OPT_USAGE_DEFAULT,  "name",            "initial branch name" },
	{ CLI_OPT_TYPE_VALUE,     "separate-git-dir", 0, &git_dir,      0,
	  CLI_OPT_USAGE_DEFAULT,  "git-dir",         "path to separate git directory" },
	{ CLI_OPT_TYPE_VALUE,     "template",         0, &template_dir, 0,
	  CLI_OPT_USAGE_DEFAULT,  "template-dir",    "path to git directory templates" },
	{ CLI_OPT_TYPE_LITERAL },
	{ CLI_OPT_TYPE_ARG,       "directory",        0,  &path,        0,
	  CLI_OPT_USAGE_DEFAULT,  "directory",       "directory to create repository in" },
	{ 0 }
};

static void print_help(void)
{
	cli_opt_usage_fprint(stdout, PROGRAM_NAME, COMMAND_NAME, opts, 0);
	printf("\n");

	printf("Create a new git repository.\n");
	printf("\n");

	printf("Options:\n");

	cli_opt_help_fprint(stdout, opts);
}

int cmd_init(int argc, char **argv)
{
	git_repository *repo = NULL;
	git_repository_init_options init_opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	cli_opt invalid_opt;
	const char *repo_path;
	int ret = 0;

	if (cli_opt_parse(&invalid_opt, opts, argv + 1, argc - 1, CLI_OPT_PARSE_GNU))
		return cli_opt_usage_error(COMMAND_NAME, opts, &invalid_opt);

	if (cli_opt__show_help) {
		print_help();
		return 0;
	}

	init_opts.flags |= GIT_REPOSITORY_INIT_MKPATH |
	                   GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;

	if (bare && git_dir)
		return cli_error_usage("the '--bare' and '--separate-git-dir' options cannot be used together");

	if (bare)
		init_opts.flags |= GIT_REPOSITORY_INIT_BARE;

	init_opts.template_path = template_dir;
	init_opts.initial_head = branch;

	if (git_dir) {
		init_opts.flags |= GIT_REPOSITORY_INIT_NO_DOTGIT_DIR;
		init_opts.workdir_path = path;

		repo_path = git_dir;
	} else {
		repo_path = path;
	}

	if (git_repository_init_ext(&repo, repo_path, &init_opts) < 0) {
		ret = cli_error_git();
	} else if (!quiet) {
		printf("Initialized empty Git repository in %s\n",
			git_repository_path(repo));
	}

	git_repository_free(repo);
	return ret;
}
