/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef CLI_error_h__
#define CLI_error_h__

#include "common.h"
#include <stdio.h>

#define CLI_EXIT_OK      0
#define CLI_EXIT_ERROR   1
#define CLI_EXIT_OS    128
#define CLI_EXIT_GIT   128
#define CLI_EXIT_USAGE 129

#define cli_error__print(fmt) do { \
		va_list ap; \
		va_start(ap, fmt); \
		fprintf(stderr, "%s: ", PROGRAM_NAME); \
		vfprintf(stderr, fmt, ap); \
		fprintf(stderr, "\n"); \
		va_end(ap); \
	} while(0)

GIT_INLINE(int) cli_error(const char *fmt, ...)
{
	cli_error__print(fmt);
	return CLI_EXIT_ERROR;
}

GIT_INLINE(int) cli_error_usage(const char *fmt, ...)
{
	cli_error__print(fmt);
	return CLI_EXIT_USAGE;
}

GIT_INLINE(int) cli_error_git(void)
{
	const git_error *err = git_error_last();
	fprintf(stderr, "%s: %s\n", PROGRAM_NAME,
	        err ? err->message : "unknown error");
	return CLI_EXIT_GIT;
}

#define cli_error_os() (perror(PROGRAM_NAME), CLI_EXIT_OS)

#endif /* CLI_error_h__ */
