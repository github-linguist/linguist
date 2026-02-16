/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "cmd.h"

const cli_cmd_spec *cli_cmd_spec_byname(const char *name)
{
	const cli_cmd_spec *cmd;

	for (cmd = cli_cmds; cmd->name; cmd++) {
		if (!strcmp(cmd->name, name))
			return cmd;
	}

	return NULL;
}
