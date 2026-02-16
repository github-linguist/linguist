/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_CRYPT_MBEDTLS_H__
#define PRIVATE_CRYPT_MBEDTLS_H__

#include "mbedtls/md.h"

struct ntlm_crypt_ctx {
	mbedtls_md_context_t hmac;
	unsigned int hmac_initialized : 1;
};

#endif /* PRIVATE_CRYPT_MBEDTLS_H__ */
