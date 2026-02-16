/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

/* Copyright (C) 1995-1998 Eric Young (eay@cryptsoft.com)
 * All rights reserved.
 *
 * This package is an SSL implementation written
 * by Eric Young (eay@cryptsoft.com).
 * The implementation was written so as to conform with Netscapes SSL.
 *
 * This library is free for commercial and non-commercial use as long as
 * the following conditions are adhered to.  The following conditions
 * apply to all code found in this distribution, be it the RC4, RSA,
 * lhash, DES, etc., code; not just the SSL code.  The SSL documentation
 * included with this distribution is covered by the same copyright terms
 * except that the holder is Tim Hudson (tjh@cryptsoft.com).
 *
 * Copyright remains Eric Young's, and as such any Copyright notices in
 * the code are not to be removed.
 * If this package is used in a product, Eric Young should be given attribution
 * as the author of the parts of the library used.
 * This can be in the form of a textual message at program startup or
 * in documentation (online or textual) provided with the package.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    "This product includes cryptographic software written by
 *     Eric Young (eay@cryptsoft.com)"
 *    The word 'cryptographic' can be left out if the routines from the library
 *    being used are not cryptographic related :-).
 * 4. If you include any Windows specific code (or a derivative thereof) from
 *    the apps directory (application code) you must include an acknowledgement:
 *    "This product includes software written by Tim Hudson (tjh@cryptsoft.com)"
 *
 * THIS SOFTWARE IS PROVIDED BY ERIC YOUNG ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * The licence and distribution terms for any publicly available version or
 * derivative of this code cannot be changed.  i.e. this code cannot simply be
 * copied and put under another distribution licence
 * [including the GNU Public Licence.]
 */
/* ====================================================================
 * Copyright (c) 1998-2007 The OpenSSL Project.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit. (http://www.openssl.org/)"
 *
 * 4. The names "OpenSSL Toolkit" and "OpenSSL Project" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    openssl-core@openssl.org.
 *
 * 5. Products derived from this software may not be called "OpenSSL"
 *    nor may "OpenSSL" appear in their names without prior written
 *    permission of the OpenSSL Project.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the OpenSSL Project
 *    for use in the OpenSSL Toolkit (http://www.openssl.org/)"
 *
 * THIS SOFTWARE IS PROVIDED BY THE OpenSSL PROJECT ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OpenSSL PROJECT OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This product includes cryptographic software written by Eric Young
 * (eay@cryptsoft.com).  This product includes software written by Tim
 * Hudson (tjh@cryptsoft.com).
 *
 */
/* ====================================================================
 * Copyright 2002 Sun Microsystems, Inc. ALL RIGHTS RESERVED.
 * ECC cipher suite support in OpenSSL originally developed by
 * SUN MICROSYSTEMS, INC., and contributed to the OpenSSL project.
 */
/* ====================================================================
 * Copyright 2005 Nokia. All rights reserved.
 *
 * The portions of the attached software ("Contribution") is developed by
 * Nokia Corporation and is licensed pursuant to the OpenSSL open source
 * license.
 *
 * The Contribution, originally written by Mika Kousa and Pasi Eronen of
 * Nokia Corporation, consists of the "PSK" (Pre-Shared Key) ciphersuites
 * support (see RFC 4279) to OpenSSL.
 *
 * No patent licenses or other rights except those expressly stated in
 * the OpenSSL open source license shall be deemed granted or received
 * expressly, by implication, estoppel, or otherwise.
 *
 * No assurances are provided by Nokia that the Contribution does not
 * infringe the patent or other intellectual property rights of any third
 * party or that the license provides you with all the necessary rights
 * to make use of the Contribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. IN
 * ADDITION TO THE DISCLAIMERS INCLUDED IN THE LICENSE, NOKIA
 * SPECIFICALLY DISCLAIMS ANY LIABILITY FOR CLAIMS BROUGHT BY YOU OR ANY
 * OTHER ENTITY BASED ON INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS OR
 * OTHERWISE.
 */

#ifndef INCLUDE_streams_openssl_dynamic_h__
#define INCLUDE_streams_openssl_dynamic_h__

#ifdef GIT_OPENSSL_DYNAMIC

# define BIO_CTRL_FLUSH               11

# define BIO_TYPE_SOURCE_SINK         0x0400

# define CRYPTO_LOCK                  1

# define GEN_DNS                      2
# define GEN_IPADD                    7

# define NID_commonName               13
# define NID_subject_alt_name         85

# define SSL_VERIFY_NONE              0x00

# define SSL_CTRL_OPTIONS             32
# define SSL_CTRL_MODE                33
# define SSL_CTRL_SET_TLSEXT_HOSTNAME 55

# define SSL_ERROR_NONE               0
# define SSL_ERROR_SSL                1
# define SSL_ERROR_WANT_READ          2
# define SSL_ERROR_WANT_WRITE         3
# define SSL_ERROR_WANT_X509_LOOKUP   4
# define SSL_ERROR_SYSCALL            5
# define SSL_ERROR_ZERO_RETURN        6
# define SSL_ERROR_WANT_CONNECT       7
# define SSL_ERROR_WANT_ACCEPT        8

# define SSL_OP_NO_COMPRESSION        0x00020000L
# define SSL_OP_NO_SSLv2              0x01000000L
# define SSL_OP_NO_SSLv3              0x02000000L

# define SSL_MODE_AUTO_RETRY          0x00000004L

# define TLSEXT_NAMETYPE_host_name    0

# define V_ASN1_UTF8STRING            12

# define X509_V_OK 0

/* Most of the OpenSSL types are mercifully opaque, so we can treat them like `void *` */
typedef struct bio_st BIO;
typedef struct bio_method_st BIO_METHOD;
typedef void bio_info_cb;
typedef void * CRYPTO_EX_DATA;
typedef void CRYPTO_THREADID;
typedef void GENERAL_NAMES;
typedef void SSL;
typedef void SSL_CTX;
typedef void SSL_METHOD;
typedef void X509;
typedef void X509_NAME;
typedef void X509_NAME_ENTRY;
typedef void X509_STORE;
typedef void X509_STORE_CTX;

typedef struct {
    int length;
    int type;
    unsigned char *data;
    long flags;
} ASN1_STRING;

typedef struct {
    int type;
    union {
        char *ptr;
        ASN1_STRING *ia5;
    } d;
} GENERAL_NAME;

struct bio_st {
    BIO_METHOD *method;
    /* bio, mode, argp, argi, argl, ret */
    long (*callback) (struct bio_st *, int, const char *, int, long, long);
    char *cb_arg;               /* first argument for the callback */
    int init;
    int shutdown;
    int flags;                  /* extra storage */
    int retry_reason;
    int num;
    void *ptr;
    struct bio_st *next_bio;    /* used by filter BIOs */
    struct bio_st *prev_bio;    /* used by filter BIOs */
    int references;
    unsigned long num_read;
    unsigned long num_write;
    CRYPTO_EX_DATA ex_data;
};

struct bio_method_st {
    int type;
    const char *name;
    int (*bwrite) (BIO *, const char *, int);
    int (*bread) (BIO *, char *, int);
    int (*bputs) (BIO *, const char *);
    int (*bgets) (BIO *, char *, int);
    long (*ctrl) (BIO *, int, long, void *);
    int (*create) (BIO *);
    int (*destroy) (BIO *);
    long (*callback_ctrl) (BIO *, int, bio_info_cb *);
};

extern unsigned char *(*ASN1_STRING_data)(ASN1_STRING *x);
extern const unsigned char *(*ASN1_STRING_get0_data)(const ASN1_STRING *x);
extern int (*ASN1_STRING_length)(const ASN1_STRING *x);
extern int (*ASN1_STRING_to_UTF8)(unsigned char **out, const ASN1_STRING *in);
extern int (*ASN1_STRING_type)(const ASN1_STRING *x);

extern void *(*BIO_get_data)(BIO *a);
extern int (*BIO_get_new_index)(void);
extern int (*OPENSSL_init_ssl)(uint64_t opts, const void *settings);
extern void (*BIO_meth_free)(BIO_METHOD *biom);
extern int (*BIO_meth_set_create)(BIO_METHOD *biom, int (*create) (BIO *));
extern int (*BIO_meth_set_ctrl)(BIO_METHOD *biom, long (*ctrl) (BIO *, int, long, void *));
extern int (*BIO_meth_set_destroy)(BIO_METHOD *biom, int (*destroy) (BIO *));
extern int (*BIO_meth_set_gets)(BIO_METHOD *biom, int (*gets) (BIO *, char *, int));
extern int (*BIO_meth_set_puts)(BIO_METHOD *biom, int (*puts) (BIO *, const char *));
extern int (*BIO_meth_set_read)(BIO_METHOD *biom, int (*read) (BIO *, char *, int));
extern int (*BIO_meth_set_write)(BIO_METHOD *biom, int (*write) (BIO *, const char *, int));
extern BIO_METHOD *(*BIO_meth_new)(int type, const char *name);
extern BIO *(*BIO_new)(const BIO_METHOD *type);
extern void (*BIO_set_data)(BIO *a, void *ptr);
extern void (*BIO_set_init)(BIO *a, int init);

extern void (*CRYPTO_free)(void *ptr, const char *file, int line);
extern void *(*CRYPTO_malloc)(size_t num, const char *file, int line);
extern int (*CRYPTO_num_locks)(void);
extern void (*CRYPTO_set_locking_callback)(void (*func)(int mode, int type, const char *file, int line));
extern int (*CRYPTO_set_mem_functions)(void *(*m)(size_t bytes), void *(*r)(void *mem, size_t size), void (*f)(void *mem));
extern int (*CRYPTO_THREADID_set_callback)(void (*func)(CRYPTO_THREADID *id));
extern void (*CRYPTO_THREADID_set_numeric)(CRYPTO_THREADID *id, unsigned long val);

extern char *(*ERR_error_string)(unsigned long e, char *buf);
extern void (*ERR_error_string_n)(unsigned long e, char *buf, size_t len);
extern unsigned long (*ERR_get_error)(void);

# define OPENSSL_malloc(num) CRYPTO_malloc(num, __FILE__, __LINE__)
# define OPENSSL_free(addr) CRYPTO_free(addr, __FILE__, __LINE__)

extern int (*SSL_connect)(SSL *ssl);
extern long (*SSL_ctrl)(SSL *ssl, int cmd, long arg, void *parg);
extern void (*SSL_free)(SSL *ssl);
extern int (*SSL_get_error)(SSL *ssl, int ret);
extern X509 *(*SSL_get_peer_certificate)(const SSL *ssl);
extern long (*SSL_get_verify_result)(const SSL *ssl);
extern int (*SSL_library_init)(void);
extern void (*SSL_load_error_strings)(void);
extern SSL *(*SSL_new)(SSL_CTX *ctx);
extern int (*SSL_read)(SSL *ssl, const void *buf, int num);
extern void (*SSL_set_bio)(SSL *ssl, BIO *rbio, BIO *wbio);
extern int (*SSL_shutdown)(SSL *ssl);
extern int (*SSL_write)(SSL *ssl, const void *buf, int num);

# define SSL_set_tlsext_host_name(s, name) SSL_ctrl((s), SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, (char *)(name));

extern long (*SSL_CTX_ctrl)(SSL_CTX *ctx, int cmd, long larg, void *parg);
extern void (*SSL_CTX_free)(SSL_CTX *ctx);
extern SSL_CTX *(*SSL_CTX_new)(const SSL_METHOD *method);
extern X509_STORE *(*SSL_CTX_get_cert_store)(const SSL_CTX *ctx);
extern int (*SSL_CTX_set_cipher_list)(SSL_CTX *ctx, const char *str);
extern int (*SSL_CTX_set_default_verify_paths)(SSL_CTX *ctx);
extern long (*SSL_CTX_set_options)(SSL_CTX *ctx, long options);
extern void (*SSL_CTX_set_verify)(SSL_CTX *ctx, int mode, int (*verify_callback)(int, X509_STORE_CTX *));
extern int (*SSL_CTX_load_verify_locations)(SSL_CTX *ctx, const char *CAfile, const char *CApath);

# define SSL_CTX_set_mode(ctx, mode) SSL_CTX_ctrl((ctx), SSL_CTRL_MODE, (mode), NULL);

extern const SSL_METHOD *(*SSLv23_method)(void);
extern const SSL_METHOD *(*TLS_method)(void);

extern ASN1_STRING *(*X509_NAME_ENTRY_get_data)(const X509_NAME_ENTRY *ne);
extern X509_NAME_ENTRY *(*X509_NAME_get_entry)(X509_NAME *name, int loc);
extern int (*X509_NAME_get_index_by_NID)(X509_NAME *name, int nid, int lastpos);
extern void (*X509_free)(X509 *a);
extern void *(*X509_get_ext_d2i)(const X509 *x, int nid, int *crit, int *idx);
extern X509_NAME *(*X509_get_subject_name)(const X509 *x);
extern int (*X509_STORE_add_cert)(X509_STORE *ctx, X509 *x);

extern int (*i2d_X509)(X509 *a, unsigned char **ppout);

extern int (*OPENSSL_sk_num)(const void *sk);
extern void *(*OPENSSL_sk_value)(const void *sk, int i);
extern void (*OPENSSL_sk_free)(void *sk);

extern int (*sk_num)(const void *sk);
extern void *(*sk_value)(const void *sk, int i);
extern void (*sk_free)(void *sk);

extern int sk_GENERAL_NAME_num(const GENERAL_NAME *sk);
extern GENERAL_NAME *sk_GENERAL_NAME_value(const GENERAL_NAME *sk, int i);
extern void GENERAL_NAMES_free(GENERAL_NAME *sk);

extern int git_openssl_stream_dynamic_init(void);

#endif /* GIT_OPENSSL_DYNAMIC */

#endif
