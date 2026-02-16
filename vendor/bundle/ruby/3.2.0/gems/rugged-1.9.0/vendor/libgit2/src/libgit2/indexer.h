/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_indexer_h__
#define INCLUDE_indexer_h__

#include "common.h"

#include "git2/indexer.h"

extern void git_indexer__set_fsync(git_indexer *idx, int do_fsync);

#endif
