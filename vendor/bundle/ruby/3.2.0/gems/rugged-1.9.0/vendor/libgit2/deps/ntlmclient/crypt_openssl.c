/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <string.h>

#ifdef CRYPT_OPENSSL_DYNAMIC
# include <dlfcn.h>
#else
# include <openssl/rand.h>
# include <openssl/des.h>
# include <openssl/md4.h>
# include <openssl/hmac.h>
# include <openssl/err.h>
#endif

#include "ntlm.h"
#include "compat.h"
#include "util.h"
#include "crypt.h"

#if OPENSSL_VERSION_NUMBER < 0x10100000L || defined(CRYPT_OPENSSL_DYNAMIC)

NTLM_INLINE(HMAC_CTX *) HMAC_CTX_new(void)
{
	return calloc(1, sizeof(HMAC_CTX));
}

NTLM_INLINE(int) HMAC_CTX_reset(HMAC_CTX *ctx)
{
	ntlm_memzero(ctx, sizeof(HMAC_CTX));
	return 1;
}

NTLM_INLINE(void) HMAC_CTX_free(HMAC_CTX *ctx)
{
	free(ctx);
}

#endif

#if (OPENSSL_VERSION_NUMBER >= 0x10100000L && !defined(LIBRESSL_VERSION_NUMBER)) || \
	(defined(LIBRESSL_VERSION_NUMBER) && LIBRESSL_VERSION_NUMBER >= 0x03050000fL) || \
	defined(CRYPT_OPENSSL_DYNAMIC)

NTLM_INLINE(void) HMAC_CTX_cleanup(HMAC_CTX *ctx)
{
	NTLM_UNUSED(ctx);
}

#endif


#ifdef CRYPT_OPENSSL_DYNAMIC

static bool ntlm_crypt_init_functions(ntlm_client *ntlm)
{
	void *handle;

	if ((handle = dlopen("libssl.so.1.1", RTLD_NOW)) == NULL &&
	    (handle = dlopen("libssl.1.1.dylib", RTLD_NOW)) == NULL &&
	    (handle = dlopen("libssl.so.1.0.0", RTLD_NOW)) == NULL &&
	    (handle = dlopen("libssl.1.0.0.dylib", RTLD_NOW)) == NULL &&
	    (handle = dlopen("libssl.so.10", RTLD_NOW)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "could not open libssl");
		return false;
	}

	ntlm->crypt_ctx.des_set_key_fn = dlsym(handle, "DES_set_key");
	ntlm->crypt_ctx.des_ecb_encrypt_fn = dlsym(handle, "DES_ecb_encrypt");
	ntlm->crypt_ctx.err_get_error_fn = dlsym(handle, "ERR_get_error");
	ntlm->crypt_ctx.err_lib_error_string_fn = dlsym(handle, "ERR_lib_error_string");
	ntlm->crypt_ctx.evp_md5_fn = dlsym(handle, "EVP_md5");
	ntlm->crypt_ctx.hmac_ctx_new_fn = dlsym(handle, "HMAC_CTX_new");
	ntlm->crypt_ctx.hmac_ctx_free_fn = dlsym(handle, "HMAC_CTX_free");
	ntlm->crypt_ctx.hmac_ctx_reset_fn = dlsym(handle, "HMAC_CTX_reset");
	ntlm->crypt_ctx.hmac_init_ex_fn = dlsym(handle, "HMAC_Init_ex");
	ntlm->crypt_ctx.hmac_update_fn = dlsym(handle, "HMAC_Update");
	ntlm->crypt_ctx.hmac_final_fn = dlsym(handle, "HMAC_Final");
	ntlm->crypt_ctx.md4_fn = dlsym(handle, "MD4");
	ntlm->crypt_ctx.rand_bytes_fn = dlsym(handle, "RAND_bytes");

	if (!ntlm->crypt_ctx.des_set_key_fn ||
	    !ntlm->crypt_ctx.des_ecb_encrypt_fn ||
	    !ntlm->crypt_ctx.err_get_error_fn ||
	    !ntlm->crypt_ctx.err_lib_error_string_fn ||
	    !ntlm->crypt_ctx.evp_md5_fn ||
	    !ntlm->crypt_ctx.hmac_init_ex_fn ||
	    !ntlm->crypt_ctx.hmac_update_fn ||
	    !ntlm->crypt_ctx.hmac_final_fn ||
	    !ntlm->crypt_ctx.md4_fn ||
	    !ntlm->crypt_ctx.rand_bytes_fn) {
		ntlm_client_set_errmsg(ntlm, "could not load libssl functions");
		dlclose(handle);
		return false;
	}

	/* Toggle legacy HMAC context functions */
	if (ntlm->crypt_ctx.hmac_ctx_new_fn &&
	    ntlm->crypt_ctx.hmac_ctx_free_fn &&
	    ntlm->crypt_ctx.hmac_ctx_reset_fn) {
		ntlm->crypt_ctx.hmac_ctx_cleanup_fn = HMAC_CTX_cleanup;
	} else {
		ntlm->crypt_ctx.hmac_ctx_cleanup_fn = dlsym(handle, "HMAC_CTX_cleanup");

		if (!ntlm->crypt_ctx.hmac_ctx_cleanup_fn) {
			ntlm_client_set_errmsg(ntlm, "could not load legacy libssl functions");
			dlclose(handle);
			return false;
		}

		ntlm->crypt_ctx.hmac_ctx_new_fn = HMAC_CTX_new;
		ntlm->crypt_ctx.hmac_ctx_free_fn = HMAC_CTX_free;
		ntlm->crypt_ctx.hmac_ctx_reset_fn = HMAC_CTX_reset;
	}

	ntlm->crypt_ctx.openssl_handle = handle;
	return true;
}

#else /* CRYPT_OPENSSL_DYNAMIC */

static bool ntlm_crypt_init_functions(ntlm_client *ntlm)
{
	ntlm->crypt_ctx.des_set_key_fn = DES_set_key;
	ntlm->crypt_ctx.des_ecb_encrypt_fn = DES_ecb_encrypt;
	ntlm->crypt_ctx.err_get_error_fn = ERR_get_error;
	ntlm->crypt_ctx.err_lib_error_string_fn = ERR_lib_error_string;
	ntlm->crypt_ctx.evp_md5_fn = EVP_md5;
	ntlm->crypt_ctx.hmac_ctx_new_fn = HMAC_CTX_new;
	ntlm->crypt_ctx.hmac_ctx_free_fn = HMAC_CTX_free;
	ntlm->crypt_ctx.hmac_ctx_reset_fn = HMAC_CTX_reset;
	ntlm->crypt_ctx.hmac_ctx_cleanup_fn = HMAC_CTX_cleanup;
	ntlm->crypt_ctx.hmac_init_ex_fn = HMAC_Init_ex;
	ntlm->crypt_ctx.hmac_update_fn = HMAC_Update;
	ntlm->crypt_ctx.hmac_final_fn = HMAC_Final;
	ntlm->crypt_ctx.md4_fn = MD4;
	ntlm->crypt_ctx.rand_bytes_fn = RAND_bytes;

	return true;
}

#endif /* CRYPT_OPENSSL_DYNAMIC */

bool ntlm_crypt_init(ntlm_client *ntlm)
{
	if (!ntlm_crypt_init_functions(ntlm))
		return false;

	ntlm->crypt_ctx.hmac = ntlm->crypt_ctx.hmac_ctx_new_fn();

	if (ntlm->crypt_ctx.hmac == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return false;
	}

	return true;
}

bool ntlm_random_bytes(
	unsigned char *out,
	ntlm_client *ntlm,
	size_t len)
{
	int rc = ntlm->crypt_ctx.rand_bytes_fn(out, len);

	if (rc != 1) {
		ntlm_client_set_errmsg(ntlm, ntlm->crypt_ctx.err_lib_error_string_fn(ntlm->crypt_ctx.err_get_error_fn()));
		return false;
	}

	return true;
}

bool ntlm_des_encrypt(
	ntlm_des_block *out,
	ntlm_client *ntlm,
	ntlm_des_block *plaintext,
	ntlm_des_block *key)
{
	DES_key_schedule keysched;

	NTLM_UNUSED(ntlm);

	memset(out, 0, sizeof(ntlm_des_block));

	ntlm->crypt_ctx.des_set_key_fn(key, &keysched);
	ntlm->crypt_ctx.des_ecb_encrypt_fn(plaintext, out, &keysched, DES_ENCRYPT);

	return true;
}

bool ntlm_md4_digest(
	unsigned char out[CRYPT_MD4_DIGESTSIZE],
	ntlm_client *ntlm,
	const unsigned char *in,
	size_t in_len)
{
	ntlm->crypt_ctx.md4_fn(in, in_len, out);
	return true;
}

bool ntlm_hmac_md5_init(
	ntlm_client *ntlm,
	const unsigned char *key,
	size_t key_len)
{
	const EVP_MD *md5 = ntlm->crypt_ctx.evp_md5_fn();

	ntlm->crypt_ctx.hmac_ctx_cleanup_fn(ntlm->crypt_ctx.hmac);

	return ntlm->crypt_ctx.hmac_ctx_reset_fn(ntlm->crypt_ctx.hmac) &&
	       ntlm->crypt_ctx.hmac_init_ex_fn(ntlm->crypt_ctx.hmac, key, key_len, md5, NULL);
}

bool ntlm_hmac_md5_update(
	ntlm_client *ntlm,
	const unsigned char *in,
	size_t in_len)
{
	return ntlm->crypt_ctx.hmac_update_fn(ntlm->crypt_ctx.hmac, in, in_len);
}

bool ntlm_hmac_md5_final(
	unsigned char *out,
	size_t *out_len,
	ntlm_client *ntlm)
{
	unsigned int len;

	if (*out_len < CRYPT_MD5_DIGESTSIZE)
		return false;

	if (!ntlm->crypt_ctx.hmac_final_fn(ntlm->crypt_ctx.hmac, out, &len))
		return false;

	*out_len = len;
	return true;
}

void ntlm_crypt_shutdown(ntlm_client *ntlm)
{
	if (ntlm->crypt_ctx.hmac) {
		ntlm->crypt_ctx.hmac_ctx_cleanup_fn(ntlm->crypt_ctx.hmac);
		ntlm->crypt_ctx.hmac_ctx_free_fn(ntlm->crypt_ctx.hmac);
	}

#ifdef CRYPT_OPENSSL_DYNAMIC
	if (ntlm->crypt_ctx.openssl_handle)
		dlclose(ntlm->crypt_ctx.openssl_handle);
#endif

	memset(&ntlm->crypt_ctx, 0, sizeof(ntlm_crypt_ctx));
}
