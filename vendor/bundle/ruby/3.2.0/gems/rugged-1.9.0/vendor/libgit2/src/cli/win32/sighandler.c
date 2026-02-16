/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"
#include <windows.h>

#include "sighandler.h"

static void (*interrupt_handler)(void) = NULL;

static BOOL WINAPI interrupt_proxy(DWORD signal)
{
	GIT_UNUSED(signal);
	interrupt_handler();
	return TRUE;
}

int cli_sighandler_set_interrupt(void (*handler)(void))
{
	BOOL result;

	if ((interrupt_handler = handler) != NULL)
		result = SetConsoleCtrlHandler(interrupt_proxy, FALSE);
	else
		result = SetConsoleCtrlHandler(NULL, FALSE);

	if (!result) {
		git_error_set(GIT_ERROR_OS, "could not set control control handler");
		return -1;
	}

	return 0;
}
