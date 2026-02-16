/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_sha_h__
#define INCLUDE_hash_sha_h__

#include "git2_util.h"

typedef struct git_hash_sha1_ctx git_hash_sha1_ctx;
typedef struct git_hash_sha256_ctx git_hash_sha256_ctx;

#if defined(GIT_SHA1_COMMON_CRYPTO) || defined(GIT_SHA256_COMMON_CRYPTO)
# include "common_crypto.h"
#endif

#if defined(GIT_SHA1_OPENSSL) || \
    defined(GIT_SHA1_OPENSSL_FIPS) || \
    defined(GIT_SHA256_OPENSSL) || \
    defined(GIT_SHA256_OPENSSL_FIPS)
# include "openssl.h"
#endif

#if defined(GIT_SHA1_WIN32) || defined(GIT_SHA256_WIN32)
# include "win32.h"
#endif

#if defined(GIT_SHA1_MBEDTLS) || defined(GIT_SHA256_MBEDTLS)
# include "mbedtls.h"
#endif

#if defined(GIT_SHA1_COLLISIONDETECT)
# include "collisiondetect.h"
#endif

#if defined(GIT_SHA256_BUILTIN)
# include "builtin.h"
#endif

/*
 * SHA1
 */

#define GIT_HASH_SHA1_SIZE 20

int git_hash_sha1_global_init(void);

int git_hash_sha1_ctx_init(git_hash_sha1_ctx *ctx);
void git_hash_sha1_ctx_cleanup(git_hash_sha1_ctx *ctx);

int git_hash_sha1_init(git_hash_sha1_ctx *c);
int git_hash_sha1_update(git_hash_sha1_ctx *c, const void *data, size_t len);
int git_hash_sha1_final(unsigned char *out, git_hash_sha1_ctx *c);

/*
 * SHA256
 */

#define GIT_HASH_SHA256_SIZE 32

int git_hash_sha256_global_init(void);

int git_hash_sha256_ctx_init(git_hash_sha256_ctx *ctx);
void git_hash_sha256_ctx_cleanup(git_hash_sha256_ctx *ctx);

int git_hash_sha256_init(git_hash_sha256_ctx *c);
int git_hash_sha256_update(git_hash_sha256_ctx *c, const void *data, size_t len);
int git_hash_sha256_final(unsigned char *out, git_hash_sha256_ctx *c);

#endif
