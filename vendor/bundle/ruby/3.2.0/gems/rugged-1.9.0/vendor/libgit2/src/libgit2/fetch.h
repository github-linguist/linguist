/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_fetch_h__
#define INCLUDE_fetch_h__

#include "common.h"

#include "git2/remote.h"

int git_fetch_negotiate(git_remote *remote, const git_fetch_options *opts);

int git_fetch_download_pack(git_remote *remote);

int git_fetch_setup_walk(git_revwalk **out, git_repository *repo);

#endif
