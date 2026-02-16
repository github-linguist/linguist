/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "streams/openssl.h"
#include "streams/openssl_dynamic.h"

#if defined(GIT_OPENSSL) && defined(GIT_OPENSSL_DYNAMIC)

#include "runtime.h"

#include <dlfcn.h>

unsigned char *(*ASN1_STRING_data)(ASN1_STRING *x);
const unsigned char *(*ASN1_STRING_get0_data)(const ASN1_STRING *x);
int (*ASN1_STRING_length)(const ASN1_STRING *x);
int (*ASN1_STRING_to_UTF8)(unsigned char **out, const ASN1_STRING *in);
int (*ASN1_STRING_type)(const ASN1_STRING *x);

void *(*BIO_get_data)(BIO *a);
int (*BIO_get_new_index)(void);
int (*OPENSSL_init_ssl)(uint64_t opts, const void *settings);
void (*BIO_meth_free)(BIO_METHOD *biom);
int (*BIO_meth_set_create)(BIO_METHOD *biom, int (*create) (BIO *));
int (*BIO_meth_set_ctrl)(BIO_METHOD *biom, long (*ctrl) (BIO *, int, long, void *));
int (*BIO_meth_set_destroy)(BIO_METHOD *biom, int (*destroy) (BIO *));
int (*BIO_meth_set_gets)(BIO_METHOD *biom, int (*gets) (BIO *, char *, int));
int (*BIO_meth_set_puts)(BIO_METHOD *biom, int (*puts) (BIO *, const char *));
int (*BIO_meth_set_read)(BIO_METHOD *biom, int (*read) (BIO *, char *, int));
int (*BIO_meth_set_write)(BIO_METHOD *biom, int (*write) (BIO *, const char *, int));
BIO_METHOD *(*BIO_meth_new)(int type, const char *name);
BIO *(*BIO_new)(const BIO_METHOD *type);
void (*BIO_set_data)(BIO *a, void *ptr);
void (*BIO_set_init)(BIO *a, int init);

void (*CRYPTO_free)(void *ptr, const char *file, int line);
void *(*CRYPTO_malloc)(size_t num, const char *file, int line);
int (*CRYPTO_num_locks)(void);
void (*CRYPTO_set_locking_callback)(void (*func)(int mode, int type, const char *file, int line));
int (*CRYPTO_set_mem_functions)(void *(*m)(size_t bytes), void *(*r)(void *mem, size_t size), void (*f)(void *mem));
int (*CRYPTO_THREADID_set_callback)(void (*func)(CRYPTO_THREADID *id));
void (*CRYPTO_THREADID_set_numeric)(CRYPTO_THREADID *id, unsigned long val);

char *(*ERR_error_string)(unsigned long e, char *buf);
void (*ERR_error_string_n)(unsigned long e, char *buf, size_t len);
unsigned long (*ERR_get_error)(void);

int (*SSL_connect)(SSL *ssl);
long (*SSL_ctrl)(SSL *ssl, int cmd, long arg, void *parg);
void (*SSL_free)(SSL *ssl);
int (*SSL_get_error)(SSL *ssl, int ret);
X509 *(*SSL_get_peer_certificate)(const SSL *ssl);
long (*SSL_get_verify_result)(const SSL *ssl);
int (*SSL_library_init)(void);
void (*SSL_load_error_strings)(void);
SSL *(*SSL_new)(SSL_CTX *ctx);
int (*SSL_read)(SSL *ssl, const void *buf, int num);
void (*SSL_set_bio)(SSL *ssl, BIO *rbio, BIO *wbio);
int (*SSL_shutdown)(SSL *ssl);
int (*SSL_write)(SSL *ssl, const void *buf, int num);

long (*SSL_CTX_ctrl)(SSL_CTX *ctx, int cmd, long larg, void *parg);
void (*SSL_CTX_free)(SSL_CTX *ctx);
SSL_CTX *(*SSL_CTX_new)(const SSL_METHOD *method);
X509_STORE *(*SSL_CTX_get_cert_store)(const SSL_CTX *);
int (*SSL_CTX_set_cipher_list)(SSL_CTX *ctx, const char *str);
int (*SSL_CTX_set_default_verify_paths)(SSL_CTX *ctx);
long (*SSL_CTX_set_options)(SSL_CTX *ctx, long options);
void (*SSL_CTX_set_verify)(SSL_CTX *ctx, int mode, int (*verify_callback)(int, X509_STORE_CTX *));
int (*SSL_CTX_load_verify_locations)(SSL_CTX *ctx, const char *CAfile, const char *CApath);

const SSL_METHOD *(*SSLv23_method)(void);
const SSL_METHOD *(*TLS_method)(void);

ASN1_STRING *(*X509_NAME_ENTRY_get_data)(const X509_NAME_ENTRY *ne);
X509_NAME_ENTRY *(*X509_NAME_get_entry)(X509_NAME *name, int loc);
int (*X509_NAME_get_index_by_NID)(X509_NAME *name, int nid, int lastpos);
void (*X509_free)(X509 *a);
void *(*X509_get_ext_d2i)(const X509 *x, int nid, int *crit, int *idx);
X509_NAME *(*X509_get_subject_name)(const X509 *x);
int (*X509_STORE_add_cert)(X509_STORE *ctx, X509 *x);

int (*i2d_X509)(X509 *a, unsigned char **ppout);

int (*OPENSSL_sk_num)(const void *sk);
void *(*OPENSSL_sk_value)(const void *sk, int i);
void (*OPENSSL_sk_free)(void *sk);

int (*sk_num)(const void *sk);
void *(*sk_value)(const void *sk, int i);
void (*sk_free)(void *sk);

static void *openssl_handle;

GIT_INLINE(void *) openssl_sym(int *err, const char *name, bool required)
{
	void *symbol;

	/* if we've seen an err, noop to retain it */
	if (*err)
		return NULL;


	if ((symbol = dlsym(openssl_handle, name)) == NULL && required) {
		const char *msg = dlerror();
		git_error_set(GIT_ERROR_SSL, "could not load ssl function '%s': %s", name, msg ? msg : "unknown error");
		*err = -1;
	}

	return symbol;
}

static void dynamic_shutdown(void)
{
	dlclose(openssl_handle);
	openssl_handle = NULL;
}

int git_openssl_stream_dynamic_init(void)
{
	int err = 0;

	if ((openssl_handle = dlopen("libssl.so.1.1", RTLD_NOW)) == NULL &&
	    (openssl_handle = dlopen("libssl.1.1.dylib", RTLD_NOW)) == NULL &&
	    (openssl_handle = dlopen("libssl.so.1.0.0", RTLD_NOW)) == NULL &&
	    (openssl_handle = dlopen("libssl.1.0.0.dylib", RTLD_NOW)) == NULL &&
	    (openssl_handle = dlopen("libssl.so.10", RTLD_NOW)) == NULL &&
	    (openssl_handle = dlopen("libssl.so.3", RTLD_NOW)) == NULL) {
		git_error_set(GIT_ERROR_SSL, "could not load ssl libraries");
		return -1;
	}

	ASN1_STRING_data = (unsigned char *(*)(ASN1_STRING *x))openssl_sym(&err, "ASN1_STRING_data", false);
	ASN1_STRING_get0_data = (const unsigned char *(*)(const ASN1_STRING *x))openssl_sym(&err, "ASN1_STRING_get0_data", false);
	ASN1_STRING_length = (int (*)(const ASN1_STRING *))openssl_sym(&err, "ASN1_STRING_length", true);
	ASN1_STRING_to_UTF8 = (int (*)(unsigned char **, const ASN1_STRING *))openssl_sym(&err, "ASN1_STRING_to_UTF8", true);
	ASN1_STRING_type = (int (*)(const ASN1_STRING *))openssl_sym(&err, "ASN1_STRING_type", true);

	BIO_get_data = (void *(*)(BIO *))openssl_sym(&err, "BIO_get_data", false);
	BIO_get_new_index = (int (*)(void))openssl_sym(&err, "BIO_get_new_index", false);
	BIO_meth_free = (void (*)(BIO_METHOD *))openssl_sym(&err, "BIO_meth_free", false);
	BIO_meth_new = (BIO_METHOD *(*)(int, const char *))openssl_sym(&err, "BIO_meth_new", false);
	BIO_meth_set_create = (int (*)(BIO_METHOD *, int (*)(BIO *)))openssl_sym(&err, "BIO_meth_set_create", false);
	BIO_meth_set_ctrl = (int (*)(BIO_METHOD *, long (*)(BIO *, int, long, void *)))openssl_sym(&err, "BIO_meth_set_ctrl", false);
	BIO_meth_set_destroy = (int (*)(BIO_METHOD *, int (*)(BIO *)))openssl_sym(&err, "BIO_meth_set_destroy", false);
	BIO_meth_set_gets = (int (*)(BIO_METHOD *, int (*)(BIO *, char *, int)))openssl_sym(&err, "BIO_meth_set_gets", false);
	BIO_meth_set_puts = (int (*)(BIO_METHOD *, int (*)(BIO *, const char *)))openssl_sym(&err, "BIO_meth_set_puts", false);
	BIO_meth_set_read = (int (*)(BIO_METHOD *, int (*)(BIO *, char *, int)))openssl_sym(&err, "BIO_meth_set_read", false);
	BIO_meth_set_write = (int (*)(BIO_METHOD *, int (*)(BIO *, const char *, int)))openssl_sym(&err, "BIO_meth_set_write", false);
	BIO_new = (BIO *(*)(const BIO_METHOD *))openssl_sym(&err, "BIO_new", true);
	BIO_set_data = (void (*)(BIO *a, void *))openssl_sym(&err, "BIO_set_data", false);
	BIO_set_init = (void (*)(BIO *a, int))openssl_sym(&err, "BIO_set_init", false);

	CRYPTO_free = (void (*)(void *, const char *, int))openssl_sym(&err, "CRYPTO_free", true);
	CRYPTO_malloc = (void *(*)(size_t, const char *, int))openssl_sym(&err, "CRYPTO_malloc", true);
	CRYPTO_num_locks = (int (*)(void))openssl_sym(&err, "CRYPTO_num_locks", false);
	CRYPTO_set_locking_callback = (void (*)(void (*)(int, int, const char *, int)))openssl_sym(&err, "CRYPTO_set_locking_callback", false);
	CRYPTO_set_mem_functions = (int (*)(void *(*)(size_t), void *(*)(void *, size_t), void (*f)(void *)))openssl_sym(&err, "CRYPTO_set_mem_functions", true);

	CRYPTO_THREADID_set_callback = (int (*)(void (*)(CRYPTO_THREADID *)))openssl_sym(&err, "CRYPTO_THREADID_set_callback", false);
	CRYPTO_THREADID_set_numeric = (void (*)(CRYPTO_THREADID *, unsigned long))openssl_sym(&err, "CRYPTO_THREADID_set_numeric", false);

	ERR_error_string = (char *(*)(unsigned long, char *))openssl_sym(&err, "ERR_error_string", true);
	ERR_error_string_n = (void (*)(unsigned long, char *, size_t))openssl_sym(&err, "ERR_error_string_n", true);
	ERR_get_error = (unsigned long (*)(void))openssl_sym(&err, "ERR_get_error", true);

	OPENSSL_init_ssl = (int (*)(uint64_t opts, const void *settings))openssl_sym(&err, "OPENSSL_init_ssl", false);
	OPENSSL_sk_num = (int (*)(const void *))openssl_sym(&err, "OPENSSL_sk_num", false);
	OPENSSL_sk_value = (void *(*)(const void *sk, int i))openssl_sym(&err, "OPENSSL_sk_value", false);
	OPENSSL_sk_free = (void (*)(void *))openssl_sym(&err, "OPENSSL_sk_free", false);

	sk_num = (int (*)(const void *))openssl_sym(&err, "sk_num", false);
	sk_value = (void *(*)(const void *sk, int i))openssl_sym(&err, "sk_value", false);
	sk_free = (void (*)(void *))openssl_sym(&err, "sk_free", false);

	SSL_connect = (int (*)(SSL *))openssl_sym(&err, "SSL_connect", true);
	SSL_ctrl = (long (*)(SSL *, int, long, void *))openssl_sym(&err, "SSL_ctrl", true);
	SSL_library_init = (int (*)(void))openssl_sym(&err, "SSL_library_init", false);
	SSL_free = (void (*)(SSL *))openssl_sym(&err, "SSL_free", true);
	SSL_get_error = (int (*)(SSL *, int))openssl_sym(&err, "SSL_get_error", true);
	SSL_get_verify_result = (long (*)(const SSL *ssl))openssl_sym(&err, "SSL_get_verify_result", true);
	SSL_load_error_strings = (void (*)(void))openssl_sym(&err, "SSL_load_error_strings", false);
	SSL_new = (SSL *(*)(SSL_CTX *))openssl_sym(&err, "SSL_new", true);
	SSL_read = (int (*)(SSL *, const void *, int))openssl_sym(&err, "SSL_read", true);
	SSL_set_bio = (void (*)(SSL *, BIO *, BIO *))openssl_sym(&err, "SSL_set_bio", true);
	SSL_shutdown = (int (*)(SSL *ssl))openssl_sym(&err, "SSL_shutdown", true);
	SSL_write = (int (*)(SSL *, const void *, int))openssl_sym(&err, "SSL_write", true);

	if (!(SSL_get_peer_certificate = (X509 *(*)(const SSL *))openssl_sym(&err, "SSL_get_peer_certificate", false))) {
		SSL_get_peer_certificate = (X509 *(*)(const SSL *))openssl_sym(&err, "SSL_get1_peer_certificate", true);
	}

	SSL_CTX_ctrl = (long (*)(SSL_CTX *, int, long, void *))openssl_sym(&err, "SSL_CTX_ctrl", true);
	SSL_CTX_free = (void (*)(SSL_CTX *))openssl_sym(&err, "SSL_CTX_free", true);
	SSL_CTX_new = (SSL_CTX *(*)(const SSL_METHOD *))openssl_sym(&err, "SSL_CTX_new", true);
	SSL_CTX_get_cert_store = (X509_STORE *(*)(const SSL_CTX *))openssl_sym(&err, "SSL_CTX_get_cert_store", true);
	SSL_CTX_set_cipher_list = (int (*)(SSL_CTX *, const char *))openssl_sym(&err, "SSL_CTX_set_cipher_list", true);
	SSL_CTX_set_default_verify_paths = (int (*)(SSL_CTX *ctx))openssl_sym(&err, "SSL_CTX_set_default_verify_paths", true);
	SSL_CTX_set_options = (long (*)(SSL_CTX *, long))openssl_sym(&err, "SSL_CTX_set_options", false);
	SSL_CTX_set_verify = (void (*)(SSL_CTX *, int, int (*)(int, X509_STORE_CTX *)))openssl_sym(&err, "SSL_CTX_set_verify", true);
	SSL_CTX_load_verify_locations = (int (*)(SSL_CTX *, const char *, const char *))openssl_sym(&err, "SSL_CTX_load_verify_locations", true);

	SSLv23_method = (const SSL_METHOD *(*)(void))openssl_sym(&err, "SSLv23_method", false);
	TLS_method = (const SSL_METHOD *(*)(void))openssl_sym(&err, "TLS_method", false);

	X509_NAME_ENTRY_get_data = (ASN1_STRING *(*)(const X509_NAME_ENTRY *))openssl_sym(&err, "X509_NAME_ENTRY_get_data", true);
	X509_NAME_get_entry = (X509_NAME_ENTRY *(*)(X509_NAME *, int))openssl_sym(&err, "X509_NAME_get_entry", true);
	X509_NAME_get_index_by_NID = (int (*)(X509_NAME *, int, int))openssl_sym(&err, "X509_NAME_get_index_by_NID", true);
	X509_free = (void (*)(X509 *))openssl_sym(&err, "X509_free", true);
	X509_get_ext_d2i = (void *(*)(const X509 *x, int nid, int *crit, int *idx))openssl_sym(&err, "X509_get_ext_d2i", true);
	X509_get_subject_name = (X509_NAME *(*)(const X509 *))openssl_sym(&err, "X509_get_subject_name", true);
	X509_STORE_add_cert = (int (*)(X509_STORE *ctx, X509 *x))openssl_sym(&err, "X509_STORE_add_cert", true);

	i2d_X509 = (int (*)(X509 *a, unsigned char **ppout))openssl_sym(&err, "i2d_X509", true);

	if (err)
		goto on_error;

	/* Add legacy functionality */
	if (!OPENSSL_init_ssl) {
		OPENSSL_init_ssl = OPENSSL_init_ssl__legacy;

		if (!SSL_library_init ||
		    !SSL_load_error_strings ||
		    !CRYPTO_num_locks ||
		    !CRYPTO_set_locking_callback ||
		    !CRYPTO_THREADID_set_callback ||
		    !CRYPTO_THREADID_set_numeric) {
			git_error_set(GIT_ERROR_SSL, "could not load legacy openssl initialization functions");
			goto on_error;
		}
	}

	if (!SSL_CTX_set_options)
		SSL_CTX_set_options = SSL_CTX_set_options__legacy;

	if (TLS_method)
		SSLv23_method = TLS_method;

	if (!BIO_meth_new) {
		BIO_meth_new = BIO_meth_new__legacy;
		BIO_meth_new = BIO_meth_new__legacy;
		BIO_meth_free = BIO_meth_free__legacy;
		BIO_meth_set_write = BIO_meth_set_write__legacy;
		BIO_meth_set_read = BIO_meth_set_read__legacy;
		BIO_meth_set_puts = BIO_meth_set_puts__legacy;
		BIO_meth_set_gets = BIO_meth_set_gets__legacy;
		BIO_meth_set_ctrl = BIO_meth_set_ctrl__legacy;
		BIO_meth_set_create = BIO_meth_set_create__legacy;
		BIO_meth_set_destroy = BIO_meth_set_destroy__legacy;
		BIO_get_new_index = BIO_get_new_index__legacy;
		BIO_set_data = BIO_set_data__legacy;
		BIO_set_init = BIO_set_init__legacy;
		BIO_get_data = BIO_get_data__legacy;
	}

	if (!ASN1_STRING_get0_data) {
		if (!ASN1_STRING_data) {
			git_error_set(GIT_ERROR_SSL, "could not load legacy openssl string function");
			goto on_error;
		}

		ASN1_STRING_get0_data = ASN1_STRING_get0_data__legacy;
	}

	if ((!OPENSSL_sk_num && !sk_num) ||
	    (!OPENSSL_sk_value && !sk_value) ||
	    (!OPENSSL_sk_free && !sk_free)) {
		git_error_set(GIT_ERROR_SSL, "could not load legacy openssl stack functions");
		goto on_error;
	}

	if (git_runtime_shutdown_register(dynamic_shutdown) != 0)
		goto on_error;

	return 0;

on_error:
	dlclose(openssl_handle);
	return -1;
}


int sk_GENERAL_NAME_num(const GENERAL_NAME *sk)
{
	if (OPENSSL_sk_num)
		return OPENSSL_sk_num(sk);
	else if (sk_num)
		return sk_num(sk);

	GIT_ASSERT_WITH_RETVAL(false, 0);
	return 0;
}

GENERAL_NAME *sk_GENERAL_NAME_value(const GENERAL_NAME *sk, int i)
{
	if (OPENSSL_sk_value)
		return OPENSSL_sk_value(sk, i);
	else if (sk_value)
		return sk_value(sk, i);

	GIT_ASSERT_WITH_RETVAL(false, NULL);
	return NULL;
}

void GENERAL_NAMES_free(GENERAL_NAME *sk)
{
	if (OPENSSL_sk_free)
		OPENSSL_sk_free(sk);
	else if (sk_free)
		sk_free(sk);
}

#endif /* GIT_OPENSSL && GIT_OPENSSL_DYNAMIC */
