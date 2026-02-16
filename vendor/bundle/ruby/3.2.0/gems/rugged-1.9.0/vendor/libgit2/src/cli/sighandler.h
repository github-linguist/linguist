/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef CLI_sighandler_h__
#define CLI_sighandler_h__

/**
 * Sets up a signal handler that will run when the process is interrupted
 * (via SIGINT on POSIX or Control-C or Control-Break on Windows).
 *
 * @param handler The function to run on interrupt
 * @return 0 on success, -1 on failure
 */
int cli_sighandler_set_interrupt(void (*handler)(void));

#endif /* CLI_sighandler_h__ */
