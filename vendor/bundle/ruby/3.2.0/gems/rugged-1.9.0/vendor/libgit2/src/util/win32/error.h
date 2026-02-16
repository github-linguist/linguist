/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_win32_error_h__
#define INCLUDE_win32_error_h__

#include "git2_util.h"

extern char *git_win32_get_error_message(DWORD error_code);

#endif
