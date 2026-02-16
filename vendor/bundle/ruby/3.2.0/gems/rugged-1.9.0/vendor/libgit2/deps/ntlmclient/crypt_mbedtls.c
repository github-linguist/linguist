/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <string.h>

#include "mbedtls/ctr_drbg.h"
#include "mbedtls/des.h"
#include "mbedtls/entropy.h"

#include "ntlm.h"
#include "crypt.h"

bool ntlm_crypt_init(ntlm_client *ntlm)
{
	const mbedtls_md_info_t *info = mbedtls_md_info_from_type(MBEDTLS_MD_MD5);

	mbedtls_md_init(&ntlm->crypt_ctx.hmac);

	if (mbedtls_md_setup(&ntlm->crypt_ctx.hmac, info, 1) != 0) {
		ntlm_client_set_errmsg(ntlm, "could not setup mbedtls digest");
		return false;
	}

	return true;
}


bool ntlm_random_bytes(
	unsigned char *out,
	ntlm_client *ntlm,
	size_t len)
{
	mbedtls_ctr_drbg_context ctr_drbg;
	mbedtls_entropy_context entropy;
	bool ret = true;

	const unsigned char personalization[] = {
		0xec, 0xb5, 0xd1, 0x0b, 0x8f, 0x15, 0x1f, 0xc2,
		0xe4, 0x8e, 0xec, 0x36, 0xf7, 0x0a, 0x45, 0x9a,
		0x1f, 0xe1, 0x35, 0x58, 0xb1, 0xcb, 0xfd, 0x8a,
		0x57, 0x5c, 0x75, 0x7d, 0x2f, 0xc9, 0x70, 0xac
	};

	mbedtls_ctr_drbg_init(&ctr_drbg);
	mbedtls_entropy_init(&entropy);

	if (mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func,
		&entropy, personalization, sizeof(personalization)) ||
		mbedtls_ctr_drbg_random(&ctr_drbg, out, len)) {
		ntlm_client_set_errmsg(ntlm, "random generation failed");
		ret = false;
	}

	mbedtls_entropy_free(&entropy);
	mbedtls_ctr_drbg_free(&ctr_drbg);

	return ret;
}

bool ntlm_des_encrypt(
	ntlm_des_block *out,
	ntlm_client *ntlm,
	ntlm_des_block *plaintext,
	ntlm_des_block *key)
{
	mbedtls_des_context ctx;
	bool success = false;

	mbedtls_des_init(&ctx);

	if (mbedtls_des_setkey_enc(&ctx, *key) ||
	    mbedtls_des_crypt_ecb(&ctx, *plaintext, *out)) {
		ntlm_client_set_errmsg(ntlm, "DES encryption failed");
		goto done;
	}

	success = true;

done:
	mbedtls_des_free(&ctx);
	return success;
}

bool ntlm_hmac_md5_init(
	ntlm_client *ntlm,
	const unsigned char *key,
	size_t key_len)
{
	if (ntlm->crypt_ctx.hmac_initialized) {
		if (mbedtls_md_hmac_reset(&ntlm->crypt_ctx.hmac))
			return false;
	}

	ntlm->crypt_ctx.hmac_initialized = !mbedtls_md_hmac_starts(&ntlm->crypt_ctx.hmac, key, key_len);
	return ntlm->crypt_ctx.hmac_initialized;
}

bool ntlm_hmac_md5_update(
	ntlm_client *ntlm,
	const unsigned char *in,
	size_t in_len)
{
	return !mbedtls_md_hmac_update(&ntlm->crypt_ctx.hmac, in, in_len);
}

bool ntlm_hmac_md5_final(
	unsigned char *out,
	size_t *out_len,
	ntlm_client *ntlm)
{
	if (*out_len < CRYPT_MD5_DIGESTSIZE)
		return false;

	return !mbedtls_md_hmac_finish(&ntlm->crypt_ctx.hmac, out);
}

void ntlm_crypt_shutdown(ntlm_client *ntlm)
{
	mbedtls_md_free(&ntlm->crypt_ctx.hmac);
}
