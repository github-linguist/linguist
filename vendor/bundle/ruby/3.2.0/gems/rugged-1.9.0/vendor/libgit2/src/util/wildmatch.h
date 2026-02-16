/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_wildmatch_h__
#define INCLUDE_wildmatch_h__

#include "git2_util.h"

#define WM_CASEFOLD 1
#define WM_PATHNAME 2

#define WM_NOMATCH 1
#define WM_MATCH 0
#define WM_ABORT_ALL -1
#define WM_ABORT_TO_STARSTAR -2

int wildmatch(const char *pattern, const char *text, unsigned int flags);

#endif
