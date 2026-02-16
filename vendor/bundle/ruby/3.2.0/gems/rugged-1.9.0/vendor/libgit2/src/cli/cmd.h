/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef CLI_cmd_h__
#define CLI_cmd_h__

/* Command definitions */
typedef struct {
	const char *name;
	int (*fn)(int argc, char **argv);
	const char *desc;
} cli_cmd_spec;

/* Options that are common to all commands (eg --help, --git-dir) */
extern const cli_opt_spec cli_common_opts[];

/* All the commands supported by the CLI */
extern const cli_cmd_spec cli_cmds[];

/* Find a command by name */
extern const cli_cmd_spec *cli_cmd_spec_byname(const char *name);

/* Commands */
extern int cmd_blame(int argc, char **argv);
extern int cmd_cat_file(int argc, char **argv);
extern int cmd_clone(int argc, char **argv);
extern int cmd_config(int argc, char **argv);
extern int cmd_hash_object(int argc, char **argv);
extern int cmd_help(int argc, char **argv);
extern int cmd_index_pack(int argc, char **argv);
extern int cmd_init(int argc, char **argv);

#endif /* CLI_cmd_h__ */
