/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef CLI_opt_usage_h__
#define CLI_opt_usage_h__

typedef enum {
	CLI_OPT_USAGE_SHOW_HIDDEN = (1 << 0),
} cli_opt_usage_flags;

/**
 * Prints usage information to the given file handle.
 *
 * @param file The file to print information to
 * @param command The name of the command to use when printing
 * @param subcommand The name of the subcommand (eg "checkout") to use when printing, or NULL to skip
 * @param specs The specifications allowed by the command
 * @return 0 on success, -1 on failure
 */
int cli_opt_usage_fprint(
	FILE *file,
	const char *command,
	const char *subcommand,
	const cli_opt_spec specs[],
	unsigned int print_flags);

int cli_opt_usage_error(
	const char *subcommand,
	const cli_opt_spec specs[],
	const cli_opt *invalid_opt);

int cli_opt_help_fprint(
	FILE *file,
	const cli_opt_spec specs[]);

#endif /* CLI_opt_usage_h__ */
