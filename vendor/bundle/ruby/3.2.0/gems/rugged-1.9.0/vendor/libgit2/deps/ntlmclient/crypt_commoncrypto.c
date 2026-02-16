/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include <CommonCrypto/CommonCrypto.h>

#include "ntlm.h"
#include "crypt.h"

bool ntlm_crypt_init(ntlm_client *ntlm)
{
	memset(&ntlm->crypt_ctx, 0, sizeof(ntlm_crypt_ctx));
	return true;
}

bool ntlm_random_bytes(
	unsigned char *out,
	ntlm_client *ntlm,
	size_t len)
{
	int fd, ret;
	size_t total = 0;

	if ((fd = open("/dev/urandom", O_RDONLY)) < 0) {
		ntlm_client_set_errmsg(ntlm, strerror(errno));
		return false;
	}

	while (total < len) {
		if ((ret = read(fd, out, (len - total))) < 0) {
			ntlm_client_set_errmsg(ntlm, strerror(errno));
			return false;
		} else if (ret == 0) {
			ntlm_client_set_errmsg(ntlm, "unexpected eof on random device");
			return false;
		}

		total += ret;
	}

	close(fd);
	return true;
}

bool ntlm_des_encrypt(
	ntlm_des_block *out,
	ntlm_client *ntlm,
	ntlm_des_block *plaintext,
	ntlm_des_block *key)
{
	CCCryptorStatus result;
	size_t written;

	NTLM_UNUSED(ntlm);

	result = CCCrypt(kCCEncrypt,
		kCCAlgorithmDES, kCCOptionECBMode,
		key, sizeof(ntlm_des_block), NULL,
		plaintext, sizeof(ntlm_des_block),
		out, sizeof(ntlm_des_block), &written);

	return (result == kCCSuccess) ? true : false;
}

bool ntlm_md4_digest(
	unsigned char out[CRYPT_MD4_DIGESTSIZE],
	ntlm_client *ntlm,
	const unsigned char *in,
	size_t in_len)
{
	NTLM_UNUSED(ntlm);
	return !!CC_MD4(in, in_len, out);
}

bool ntlm_hmac_md5_init(
	ntlm_client *ntlm,
	const unsigned char *key,
	size_t key_len)
{
	CCHmacInit(&ntlm->crypt_ctx.hmac, kCCHmacAlgMD5, key, key_len);
	return true;
}

bool ntlm_hmac_md5_update(
	ntlm_client *ntlm,
	const unsigned char *data,
	size_t data_len)
{
	CCHmacUpdate(&ntlm->crypt_ctx.hmac, data, data_len);
	return true;
}

bool ntlm_hmac_md5_final(
	unsigned char *out,
	size_t *out_len,
	ntlm_client *ntlm)
{
	if (*out_len < CRYPT_MD5_DIGESTSIZE)
		return false;

	CCHmacFinal(&ntlm->crypt_ctx.hmac, out);

	*out_len = CRYPT_MD5_DIGESTSIZE;
	return true;
}

void ntlm_crypt_shutdown(ntlm_client *ntlm)
{
	NTLM_UNUSED(ntlm);
}
