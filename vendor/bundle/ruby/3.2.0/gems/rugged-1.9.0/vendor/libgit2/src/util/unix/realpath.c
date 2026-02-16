/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"

#ifndef GIT_WIN32

#include <limits.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

char *p_realpath(const char *pathname, char *resolved)
{
	char *result;

	if ((result = realpath(pathname, resolved)) == NULL)
		return NULL;

#ifdef __OpenBSD__
	/* The OpenBSD realpath function behaves differently,
	 * figure out if the file exists */
	if (access(result, F_OK) < 0) {
		if (!resolved)
			free(result);

		return NULL;
	}
#endif

	/*
	 * If resolved == NULL, the system has allocated the result
	 * string. We need to strdup this into _our_ allocator pool
	 * so that callers can free it with git__free.
	 */
	if (!resolved) {
		char *dup = git__strdup(result);
		free(result);

		result = dup;
	}

	return result;
}

#endif
