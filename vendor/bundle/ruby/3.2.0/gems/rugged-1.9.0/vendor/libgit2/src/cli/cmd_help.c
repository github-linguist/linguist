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

#define COMMAND_NAME "help"

static char *command;

static const cli_opt_spec opts[] = {
	CLI_COMMON_OPT,

	{ CLI_OPT_TYPE_ARG,      "command",  0, &command,   0,
	  CLI_OPT_USAGE_DEFAULT, "command", "the command to show help for" },
	{ 0 },
};

static int print_help(void)
{
	cli_opt_usage_fprint(stdout, PROGRAM_NAME, COMMAND_NAME, opts, CLI_OPT_USAGE_SHOW_HIDDEN);
	printf("\n");

	printf("Display help information about %s.  If a command is specified, help\n", PROGRAM_NAME);
	printf("about that command will be shown.  Otherwise, general information about\n");
	printf("%s will be shown, including the commands available.\n", PROGRAM_NAME);

	return 0;
}

static int print_commands(void)
{
	const cli_cmd_spec *cmd;

	cli_opt_usage_fprint(stdout, PROGRAM_NAME, NULL, cli_common_opts, CLI_OPT_USAGE_SHOW_HIDDEN);
	printf("\n");

	printf("These are the %s commands available:\n\n", PROGRAM_NAME);

	for (cmd = cli_cmds; cmd->name; cmd++)
		printf("   %-11s  %s\n", cmd->name, cmd->desc);

	printf("\nSee '%s help <command>' for more information on a specific command.\n", PROGRAM_NAME);

	return 0;
}

int cmd_help(int argc, char **argv)
{
	char *fake_args[2];
	const cli_cmd_spec *cmd;
	cli_opt invalid_opt;

	if (cli_opt_parse(&invalid_opt, opts, argv + 1, argc - 1, CLI_OPT_PARSE_GNU))
		return cli_opt_usage_error(COMMAND_NAME, opts, &invalid_opt);

	/* Show the meta-help */
	if (cli_opt__show_help)
		return print_help();

	/* We were not asked to show help for a specific command. */
	if (!command)
		return print_commands();

	/*
	 * If we were asked for help for a command (eg, `help <command>`),
	 * delegate back to that command's `--help` option.  This lets
	 * commands own their help.  Emulate the command-line arguments
	 * that would invoke `<command> --help` and invoke that command.
	 */
	fake_args[0] = command;
	fake_args[1] = "--help";

	if ((cmd = cli_cmd_spec_byname(command)) == NULL)
		return cli_error("'%s' is not a %s command. See '%s help'.",
		                command, PROGRAM_NAME, PROGRAM_NAME);

	return cmd->fn(2, fake_args);
}
