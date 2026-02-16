/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_allocators_win32_leakcheck_h
#define INCLUDE_allocators_win32_leakcheck_h

#include "git2_util.h"

#include "alloc.h"

int git_win32_leakcheck_init_allocator(git_allocator *allocator);

#endif
