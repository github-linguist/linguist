/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_common_crypto_h__
#define INCLUDE_hash_common_crypto_h__

#include "hash/sha.h"

#include <CommonCrypto/CommonDigest.h>

#ifdef GIT_SHA1_COMMON_CRYPTO
struct git_hash_sha1_ctx {
	CC_SHA1_CTX c;
};
#endif

#ifdef GIT_SHA256_COMMON_CRYPTO
struct git_hash_sha256_ctx {
	CC_SHA256_CTX c;
};
#endif

#endif
