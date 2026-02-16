/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_streams_openssl_legacy_h__
#define INCLUDE_streams_openssl_legacy_h__

#include "streams/openssl_dynamic.h"

#if defined(GIT_OPENSSL) && !defined(GIT_OPENSSL_DYNAMIC)
# include <openssl/ssl.h>
# include <openssl/err.h>
# include <openssl/x509v3.h>
# include <openssl/bio.h>

# if (defined(OPENSSL_VERSION_NUMBER) && OPENSSL_VERSION_NUMBER < 0x10100000L) || \
     (defined(LIBRESSL_VERSION_NUMBER) && LIBRESSL_VERSION_NUMBER < 0x20700000L)
#  define GIT_OPENSSL_LEGACY
# endif
#endif

#if defined(GIT_OPENSSL_LEGACY) && !defined(GIT_OPENSSL_DYNAMIC)
# define OPENSSL_init_ssl OPENSSL_init_ssl__legacy
# define BIO_meth_new BIO_meth_new__legacy
# define BIO_meth_free BIO_meth_free__legacy
# define BIO_meth_set_write BIO_meth_set_write__legacy
# define BIO_meth_set_read BIO_meth_set_read__legacy
# define BIO_meth_set_puts BIO_meth_set_puts__legacy
# define BIO_meth_set_gets BIO_meth_set_gets__legacy
# define BIO_meth_set_ctrl BIO_meth_set_ctrl__legacy
# define BIO_meth_set_create BIO_meth_set_create__legacy
# define BIO_meth_set_destroy BIO_meth_set_destroy__legacy
# define BIO_get_new_index BIO_get_new_index__legacy
# define BIO_set_data BIO_set_data__legacy
# define BIO_set_init BIO_set_init__legacy
# define BIO_get_data BIO_get_data__legacy
# define ASN1_STRING_get0_data ASN1_STRING_get0_data__legacy
#endif

#if defined(GIT_OPENSSL_LEGACY) || defined(GIT_OPENSSL_DYNAMIC)

extern int OPENSSL_init_ssl__legacy(uint64_t opts, const void *settings);
extern BIO_METHOD *BIO_meth_new__legacy(int type, const char *name);
extern void BIO_meth_free__legacy(BIO_METHOD *biom);
extern int BIO_meth_set_write__legacy(BIO_METHOD *biom, int (*write) (BIO *, const char *, int));
extern int BIO_meth_set_read__legacy(BIO_METHOD *biom, int (*read) (BIO *, char *, int));
extern int BIO_meth_set_puts__legacy(BIO_METHOD *biom, int (*puts) (BIO *, const char *));
extern int BIO_meth_set_gets__legacy(BIO_METHOD *biom, int (*gets) (BIO *, char *, int));
extern int BIO_meth_set_ctrl__legacy(BIO_METHOD *biom, long (*ctrl) (BIO *, int, long, void *));
extern int BIO_meth_set_create__legacy(BIO_METHOD *biom, int (*create) (BIO *));
extern int BIO_meth_set_destroy__legacy(BIO_METHOD *biom, int (*destroy) (BIO *));
extern int BIO_get_new_index__legacy(void);
extern void BIO_set_data__legacy(BIO *a, void *ptr);
extern void BIO_set_init__legacy(BIO *b, int init);
extern void *BIO_get_data__legacy(BIO *a);
extern const unsigned char *ASN1_STRING_get0_data__legacy(const ASN1_STRING *x);
extern long SSL_CTX_set_options__legacy(SSL_CTX *ctx, long op);

#endif

#endif
