/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_signature_h__
#define INCLUDE_signature_h__

#include "common.h"

#include "git2/common.h"
#include "git2/signature.h"
#include "repository.h"
#include <time.h>

int git_signature__parse(git_signature *sig, const char **buffer_out, const char *buffer_end, const char *header, char ender);
void git_signature__writebuf(git_str *buf, const char *header, const git_signature *sig);
bool git_signature__equal(const git_signature *one, const git_signature *two);
int git_signature__pdup(git_signature **dest, const git_signature *source, git_pool *pool);

#endif
