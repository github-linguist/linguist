/*
* Copyright (C) the libgit2 contributors. All rights reserved.
*
* This file is part of libgit2, distributed under the GNU GPL v2 with
* a Linking Exception. For full terms see the included COPYING file.
*/
#ifndef INCLUDE_proxy_h__
#define INCLUDE_proxy_h__

#include "common.h"

#include "git2/proxy.h"

extern int git_proxy_options_dup(git_proxy_options *tgt, const git_proxy_options *src);
extern void git_proxy_options_dispose(git_proxy_options *opts);

#endif
