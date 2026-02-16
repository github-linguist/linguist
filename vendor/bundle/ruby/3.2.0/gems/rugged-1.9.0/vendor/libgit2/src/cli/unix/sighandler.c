/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stdint.h>
#include <signal.h>
#include "git2_util.h"
#include "common.h"
#include "sighandler.h"

static void (*interrupt_handler)(void) = NULL;

static void interrupt_proxy(int signal)
{
	GIT_UNUSED(signal);
	interrupt_handler();
}

int cli_sighandler_set_interrupt(void (*handler)(void))
{
	void (*result)(int);

	if ((interrupt_handler = handler) != NULL)
		result = signal(SIGINT, interrupt_proxy);
	else
		result = signal(SIGINT, SIG_DFL);

	if (result == SIG_ERR) {
		git_error_set(GIT_ERROR_OS, "could not set signal handler");
		return -1;
	}

	return 0;
}
