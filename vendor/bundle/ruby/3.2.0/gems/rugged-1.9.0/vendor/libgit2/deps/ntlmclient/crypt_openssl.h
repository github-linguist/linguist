/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_CRYPT_OPENSSL_H__
#define PRIVATE_CRYPT_OPENSSL_H__

#ifndef CRYPT_OPENSSL_DYNAMIC
# include <openssl/des.h>
# include <openssl/hmac.h>
#endif

/* OpenSSL 1.1.0 uses opaque structs, we'll reuse these. */
#if defined(OPENSSL_VERSION_NUMBER) && OPENSSL_VERSION_NUMBER < 0x10100000L
# define HMAC_CTX struct hmac_ctx_st
#endif

#ifdef CRYPT_OPENSSL_DYNAMIC
typedef unsigned char DES_cblock[8];
typedef unsigned char const_DES_cblock[8];

typedef unsigned long DES_LONG;

typedef struct DES_ks {
    union {
        DES_cblock cblock;
        DES_LONG deslong[2];
    } ks[16];
} DES_key_schedule;

#define DES_ENCRYPT 1

typedef void EVP_MD;
typedef void ENGINE;
typedef void EVP_PKEY_CTX;

#define HMAC_MAX_MD_CBLOCK 128

typedef struct env_md_ctx_st EVP_MD_CTX;
struct env_md_ctx_st {
    const EVP_MD *digest;
    ENGINE *engine;
    unsigned long flags;
    void *md_data;
    EVP_PKEY_CTX *pctx;
    int (*update) (EVP_MD_CTX *ctx, const void *data, size_t count);
};

typedef struct hmac_ctx_st {
    const EVP_MD *md;
    EVP_MD_CTX md_ctx;
    EVP_MD_CTX i_ctx;
    EVP_MD_CTX o_ctx;
    unsigned int key_length;
    unsigned char key[HMAC_MAX_MD_CBLOCK];
} HMAC_CTX;
#endif

struct ntlm_crypt_ctx {
	HMAC_CTX *hmac;

	void *openssl_handle;

	void (*des_ecb_encrypt_fn)(const_DES_cblock *input, DES_cblock *output, DES_key_schedule *ks, int enc);
	int (*des_set_key_fn)(const_DES_cblock *key, DES_key_schedule *schedule);

	unsigned long (*err_get_error_fn)(void);
	const char *(*err_lib_error_string_fn)(unsigned long e);

	const EVP_MD *(*evp_md5_fn)(void);

	HMAC_CTX *(*hmac_ctx_new_fn)(void);
	int (*hmac_ctx_reset_fn)(HMAC_CTX *ctx);
	void (*hmac_ctx_free_fn)(HMAC_CTX *ctx);
	void (*hmac_ctx_cleanup_fn)(HMAC_CTX *ctx);

	int (*hmac_init_ex_fn)(HMAC_CTX *ctx, const void *key, int key_len, const EVP_MD *md, ENGINE *impl);
	int (*hmac_update_fn)(HMAC_CTX *ctx, const unsigned char *data, size_t len);
	int (*hmac_final_fn)(HMAC_CTX *ctx, unsigned char *md, unsigned int *len);

	unsigned char *(*md4_fn)(const unsigned char *d, size_t n, unsigned char *md);

	int (*rand_bytes_fn)(unsigned char *buf, int num);
};

#endif /* PRIVATE_CRYPT_OPENSSL_H__ */
