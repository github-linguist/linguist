/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_builtin_h__
#define INCLUDE_hash_builtin_h__

#include "hash/sha.h"

#include "rfc6234/sha.h"

struct git_hash_sha256_ctx {
	SHA256Context c;
};

#endif
