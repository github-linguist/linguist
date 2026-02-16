/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "win32.h"

#include "runtime.h"

#include <wincrypt.h>
#include <strsafe.h>

#define GIT_HASH_CNG_DLL_NAME           "bcrypt.dll"

/* BCRYPT_SHA1_ALGORITHM */
#define GIT_HASH_CNG_SHA1_TYPE          L"SHA1"
#define GIT_HASH_CNG_SHA256_TYPE        L"SHA256"

/* BCRYPT_OBJECT_LENGTH */
#define GIT_HASH_CNG_HASH_OBJECT_LEN    L"ObjectLength"

/* BCRYPT_HASH_REUSEABLE_FLAGS */
#define GIT_HASH_CNG_HASH_REUSABLE      0x00000020

/* Definitions */

/* CryptoAPI is available for hashing on Windows XP and newer.  */
struct cryptoapi_provider {
	HCRYPTPROV handle;
};

/*
 * CNG (bcrypt.dll) is significantly more performant than CryptoAPI and is
 * preferred, however it is only available on Windows 2008 and newer and
 * must therefore be dynamically loaded, and we must inline constants that
 * would not exist when building in pre-Windows 2008 environments.
 */

/* Function declarations for CNG */
typedef NTSTATUS (WINAPI *cng_open_algorithm_provider_fn)(
	HANDLE /* BCRYPT_ALG_HANDLE */ *phAlgorithm,
	LPCWSTR pszAlgId,
	LPCWSTR pszImplementation,
	DWORD dwFlags);

typedef NTSTATUS (WINAPI *cng_get_property_fn)(
	HANDLE /* BCRYPT_HANDLE */ hObject,
	LPCWSTR pszProperty,
	PUCHAR pbOutput,
	ULONG cbOutput,
	ULONG *pcbResult,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *cng_create_hash_fn)(
	HANDLE /* BCRYPT_ALG_HANDLE */ hAlgorithm,
	HANDLE /* BCRYPT_HASH_HANDLE */ *phHash,
	PUCHAR pbHashObject, ULONG cbHashObject,
	PUCHAR pbSecret,
	ULONG cbSecret,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *cng_finish_hash_fn)(
	HANDLE /* BCRYPT_HASH_HANDLE */ hHash,
	PUCHAR pbOutput,
	ULONG cbOutput,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *cng_hash_data_fn)(
	HANDLE /* BCRYPT_HASH_HANDLE */ hHash,
	PUCHAR pbInput,
	ULONG cbInput,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *cng_destroy_hash_fn)(
	HANDLE /* BCRYPT_HASH_HANDLE */ hHash);

typedef NTSTATUS (WINAPI *cng_close_algorithm_provider_fn)(
	HANDLE /* BCRYPT_ALG_HANDLE */ hAlgorithm,
	ULONG dwFlags);

struct cng_provider {
	/* DLL for CNG */
	HINSTANCE dll;

	/* Function pointers for CNG */
	cng_open_algorithm_provider_fn open_algorithm_provider;
	cng_get_property_fn get_property;
	cng_create_hash_fn create_hash;
	cng_finish_hash_fn finish_hash;
	cng_hash_data_fn hash_data;
	cng_destroy_hash_fn destroy_hash;
	cng_close_algorithm_provider_fn close_algorithm_provider;

	HANDLE /* BCRYPT_ALG_HANDLE */ sha1_handle;
	DWORD sha1_object_size;

	HANDLE /* BCRYPT_ALG_HANDLE */ sha256_handle;
	DWORD sha256_object_size;
};

typedef struct {
	git_hash_win32_provider_t type;

	union {
		struct cryptoapi_provider cryptoapi;
		struct cng_provider cng;
	} provider;
} hash_win32_provider;

/* Hash provider definition */

static hash_win32_provider hash_provider = {0};

/* Hash initialization */

/* Initialize CNG, if available */
GIT_INLINE(int) cng_provider_init(void)
{
	char dll_path[MAX_PATH];
	DWORD dll_path_len, size_len;

	/* Only use CNG on Windows 2008 / Vista SP1  or better (Windows 6.0 SP1) */
	if (!git_has_win32_version(6, 0, 1)) {
		git_error_set(GIT_ERROR_SHA, "CryptoNG is not supported on this platform");
		return -1;
	}

	/* Load bcrypt.dll explicitly from the system directory */
	if ((dll_path_len = GetSystemDirectory(dll_path, MAX_PATH)) == 0 ||
		dll_path_len > MAX_PATH ||
		StringCchCat(dll_path, MAX_PATH, "\\") < 0 ||
		StringCchCat(dll_path, MAX_PATH, GIT_HASH_CNG_DLL_NAME) < 0 ||
		(hash_provider.provider.cng.dll = LoadLibrary(dll_path)) == NULL) {
		git_error_set(GIT_ERROR_SHA, "CryptoNG library could not be loaded");
		return -1;
	}

	/* Load the function addresses */
	if ((hash_provider.provider.cng.open_algorithm_provider = (cng_open_algorithm_provider_fn)((void *)GetProcAddress(hash_provider.provider.cng.dll, "BCryptOpenAlgorithmProvider"))) == NULL ||
		(hash_provider.provider.cng.get_property = (cng_get_property_fn)((void *)GetProcAddress(hash_provider.provider.cng.dll, "BCryptGetProperty"))) == NULL ||
		(hash_provider.provider.cng.create_hash = (cng_create_hash_fn)((void *)GetProcAddress(hash_provider.provider.cng.dll, "BCryptCreateHash"))) == NULL ||
		(hash_provider.provider.cng.finish_hash = (cng_finish_hash_fn)((void *)GetProcAddress(hash_provider.provider.cng.dll, "BCryptFinishHash"))) == NULL ||
		(hash_provider.provider.cng.hash_data = (cng_hash_data_fn)((void *)GetProcAddress(hash_provider.provider.cng.dll, "BCryptHashData"))) == NULL ||
		(hash_provider.provider.cng.destroy_hash = (cng_destroy_hash_fn)((void *)GetProcAddress(hash_provider.provider.cng.dll, "BCryptDestroyHash"))) == NULL ||
		(hash_provider.provider.cng.close_algorithm_provider = (cng_close_algorithm_provider_fn)((void *)GetProcAddress(hash_provider.provider.cng.dll, "BCryptCloseAlgorithmProvider"))) == NULL) {
		FreeLibrary(hash_provider.provider.cng.dll);

		git_error_set(GIT_ERROR_OS, "CryptoNG functions could not be loaded");
		return -1;
	}

	/* Load the SHA1 algorithm */
	if (hash_provider.provider.cng.open_algorithm_provider(&hash_provider.provider.cng.sha1_handle, GIT_HASH_CNG_SHA1_TYPE, NULL, GIT_HASH_CNG_HASH_REUSABLE) < 0 ||
	    hash_provider.provider.cng.get_property(hash_provider.provider.cng.sha1_handle, GIT_HASH_CNG_HASH_OBJECT_LEN, (PBYTE)&hash_provider.provider.cng.sha1_object_size, sizeof(DWORD), &size_len, 0) < 0) {
		git_error_set(GIT_ERROR_OS, "algorithm provider could not be initialized");
		goto on_error;
	}

	/* Load the SHA256 algorithm */
	if (hash_provider.provider.cng.open_algorithm_provider(&hash_provider.provider.cng.sha256_handle, GIT_HASH_CNG_SHA256_TYPE, NULL, GIT_HASH_CNG_HASH_REUSABLE) < 0 ||
	    hash_provider.provider.cng.get_property(hash_provider.provider.cng.sha256_handle, GIT_HASH_CNG_HASH_OBJECT_LEN, (PBYTE)&hash_provider.provider.cng.sha256_object_size, sizeof(DWORD), &size_len, 0) < 0) {
		git_error_set(GIT_ERROR_OS, "algorithm provider could not be initialized");
		goto on_error;
	}

	hash_provider.type = GIT_HASH_WIN32_CNG;
	return 0;

on_error:
	if (hash_provider.provider.cng.sha1_handle)
		hash_provider.provider.cng.close_algorithm_provider(hash_provider.provider.cng.sha1_handle, 0);

	if (hash_provider.provider.cng.sha256_handle)
		hash_provider.provider.cng.close_algorithm_provider(hash_provider.provider.cng.sha256_handle, 0);

	if (hash_provider.provider.cng.dll)
		FreeLibrary(hash_provider.provider.cng.dll);

	return -1;
}

GIT_INLINE(void) cng_provider_shutdown(void)
{
	if (hash_provider.type == GIT_HASH_WIN32_INVALID)
		return;

	hash_provider.provider.cng.close_algorithm_provider(hash_provider.provider.cng.sha1_handle, 0);
	hash_provider.provider.cng.close_algorithm_provider(hash_provider.provider.cng.sha256_handle, 0);
	FreeLibrary(hash_provider.provider.cng.dll);

	hash_provider.type = GIT_HASH_WIN32_INVALID;
}

/* Initialize CryptoAPI */
GIT_INLINE(int) cryptoapi_provider_init(void)
{
	if (!CryptAcquireContext(&hash_provider.provider.cryptoapi.handle, NULL, 0, PROV_RSA_AES, CRYPT_VERIFYCONTEXT)) {
		git_error_set(GIT_ERROR_OS, "legacy hash context could not be started");
		return -1;
	}

	hash_provider.type = GIT_HASH_WIN32_CRYPTOAPI;
	return 0;
}

GIT_INLINE(void) cryptoapi_provider_shutdown(void)
{
	if (hash_provider.type == GIT_HASH_WIN32_INVALID)
		return;

	CryptReleaseContext(hash_provider.provider.cryptoapi.handle, 0);

	hash_provider.type = GIT_HASH_WIN32_INVALID;
}

static void hash_provider_shutdown(void)
{
	if (hash_provider.type == GIT_HASH_WIN32_CNG)
		cng_provider_shutdown();
	else if (hash_provider.type == GIT_HASH_WIN32_CRYPTOAPI)
		cryptoapi_provider_shutdown();
}

static int hash_provider_init(void)
{
	int error = 0;

	if (hash_provider.type != GIT_HASH_WIN32_INVALID)
		return 0;

	if ((error = cng_provider_init()) < 0)
		error = cryptoapi_provider_init();

	if (!error)
		error = git_runtime_shutdown_register(hash_provider_shutdown);

	return error;
}

git_hash_win32_provider_t git_hash_win32_provider(void)
{
	return hash_provider.type;
}

int git_hash_win32_set_provider(git_hash_win32_provider_t provider)
{
	if (provider == hash_provider.type)
		return 0;

	hash_provider_shutdown();

	if (provider == GIT_HASH_WIN32_CNG)
		return cng_provider_init();
	else if (provider == GIT_HASH_WIN32_CRYPTOAPI)
		return cryptoapi_provider_init();

	git_error_set(GIT_ERROR_SHA, "unsupported win32 provider");
	return -1;
}

/* CryptoAPI: available in Windows XP and newer */

GIT_INLINE(int) hash_cryptoapi_init(git_hash_win32_ctx *ctx)
{
	if (ctx->ctx.cryptoapi.valid)
		CryptDestroyHash(ctx->ctx.cryptoapi.hash_handle);

	if (!CryptCreateHash(hash_provider.provider.cryptoapi.handle, ctx->algorithm, 0, 0, &ctx->ctx.cryptoapi.hash_handle)) {
		ctx->ctx.cryptoapi.valid = 0;
		git_error_set(GIT_ERROR_OS, "legacy hash implementation could not be created");
		return -1;
	}

	ctx->ctx.cryptoapi.valid = 1;
	return 0;
}

GIT_INLINE(int) hash_cryptoapi_update(git_hash_win32_ctx *ctx, const void *_data, size_t len)
{
	const BYTE *data = (BYTE *)_data;

	GIT_ASSERT(ctx->ctx.cryptoapi.valid);

	while (len > 0) {
		DWORD chunk = (len > MAXDWORD) ? MAXDWORD : (DWORD)len;

		if (!CryptHashData(ctx->ctx.cryptoapi.hash_handle, data, chunk, 0)) {
			git_error_set(GIT_ERROR_OS, "legacy hash data could not be updated");
			return -1;
		}

		data += chunk;
		len -= chunk;
	}

	return 0;
}

GIT_INLINE(int) hash_cryptoapi_final(unsigned char *out, git_hash_win32_ctx *ctx)
{
	DWORD len = ctx->algorithm == CALG_SHA_256 ? GIT_HASH_SHA256_SIZE : GIT_HASH_SHA1_SIZE;
	int error = 0;

	GIT_ASSERT(ctx->ctx.cryptoapi.valid);

	if (!CryptGetHashParam(ctx->ctx.cryptoapi.hash_handle, HP_HASHVAL, out, &len, 0)) {
		git_error_set(GIT_ERROR_OS, "legacy hash data could not be finished");
		error = -1;
	}

	CryptDestroyHash(ctx->ctx.cryptoapi.hash_handle);
	ctx->ctx.cryptoapi.valid = 0;

	return error;
}

GIT_INLINE(void) hash_ctx_cryptoapi_cleanup(git_hash_win32_ctx *ctx)
{
	if (ctx->ctx.cryptoapi.valid)
		CryptDestroyHash(ctx->ctx.cryptoapi.hash_handle);
}

GIT_INLINE(int) hash_sha1_cryptoapi_ctx_init_init(git_hash_win32_ctx *ctx)
{
	ctx->algorithm = CALG_SHA1;
	return hash_cryptoapi_init(ctx);
}

GIT_INLINE(int) hash_sha256_cryptoapi_ctx_init(git_hash_win32_ctx *ctx)
{
	ctx->algorithm = CALG_SHA_256;
	return hash_cryptoapi_init(ctx);
}

/* CNG: Available in Windows Server 2008 and newer */

GIT_INLINE(int) hash_sha1_cng_ctx_init(git_hash_win32_ctx *ctx)
{
	if ((ctx->ctx.cng.hash_object = git__malloc(hash_provider.provider.cng.sha1_object_size)) == NULL)
		return -1;

	if (hash_provider.provider.cng.create_hash(hash_provider.provider.cng.sha1_handle, &ctx->ctx.cng.hash_handle, ctx->ctx.cng.hash_object, hash_provider.provider.cng.sha1_object_size, NULL, 0, 0) < 0) {
		git__free(ctx->ctx.cng.hash_object);

		git_error_set(GIT_ERROR_OS, "sha1 implementation could not be created");
		return -1;
	}

	ctx->algorithm = CALG_SHA1;
	return 0;
}

GIT_INLINE(int) hash_sha256_cng_ctx_init(git_hash_win32_ctx *ctx)
{
	if ((ctx->ctx.cng.hash_object = git__malloc(hash_provider.provider.cng.sha256_object_size)) == NULL)
		return -1;

	if (hash_provider.provider.cng.create_hash(hash_provider.provider.cng.sha256_handle, &ctx->ctx.cng.hash_handle, ctx->ctx.cng.hash_object, hash_provider.provider.cng.sha256_object_size, NULL, 0, 0) < 0) {
		git__free(ctx->ctx.cng.hash_object);

		git_error_set(GIT_ERROR_OS, "sha256 implementation could not be created");
		return -1;
	}

	ctx->algorithm = CALG_SHA_256;
	return 0;
}

GIT_INLINE(int) hash_cng_init(git_hash_win32_ctx *ctx)
{
	BYTE hash[GIT_HASH_SHA256_SIZE];
	ULONG size = ctx->algorithm == CALG_SHA_256 ? GIT_HASH_SHA256_SIZE : GIT_HASH_SHA1_SIZE;

	if (!ctx->ctx.cng.updated)
		return 0;

	/* CNG needs to be finished to restart */
	if (hash_provider.provider.cng.finish_hash(ctx->ctx.cng.hash_handle, hash, size, 0) < 0) {
		git_error_set(GIT_ERROR_OS, "hash implementation could not be finished");
		return -1;
	}

	ctx->ctx.cng.updated = 0;

	return 0;
}

GIT_INLINE(int) hash_cng_update(git_hash_win32_ctx *ctx, const void *_data, size_t len)
{
	PBYTE data = (PBYTE)_data;

	while (len > 0) {
		ULONG chunk = (len > ULONG_MAX) ? ULONG_MAX : (ULONG)len;

		if (hash_provider.provider.cng.hash_data(ctx->ctx.cng.hash_handle, data, chunk, 0) < 0) {
			git_error_set(GIT_ERROR_OS, "hash could not be updated");
			return -1;
		}

		data += chunk;
		len -= chunk;
	}

	return 0;
}

GIT_INLINE(int) hash_cng_final(unsigned char *out, git_hash_win32_ctx *ctx)
{
	ULONG size = ctx->algorithm == CALG_SHA_256 ? GIT_HASH_SHA256_SIZE : GIT_HASH_SHA1_SIZE;

	if (hash_provider.provider.cng.finish_hash(ctx->ctx.cng.hash_handle, out, size, 0) < 0) {
		git_error_set(GIT_ERROR_OS, "hash could not be finished");
		return -1;
	}

	ctx->ctx.cng.updated = 0;

	return 0;
}

GIT_INLINE(void) hash_ctx_cng_cleanup(git_hash_win32_ctx *ctx)
{
	hash_provider.provider.cng.destroy_hash(ctx->ctx.cng.hash_handle);
	git__free(ctx->ctx.cng.hash_object);
}

/* Indirection between CryptoAPI and CNG */

GIT_INLINE(int) hash_sha1_win32_ctx_init(git_hash_win32_ctx *ctx)
{
	GIT_ASSERT_ARG(hash_provider.type);

	memset(ctx, 0x0, sizeof(git_hash_win32_ctx));
	return (hash_provider.type == GIT_HASH_WIN32_CNG) ? hash_sha1_cng_ctx_init(ctx) : hash_sha1_cryptoapi_ctx_init_init(ctx);
}

GIT_INLINE(int) hash_sha256_win32_ctx_init(git_hash_win32_ctx *ctx)
{
	GIT_ASSERT_ARG(hash_provider.type);

	memset(ctx, 0x0, sizeof(git_hash_win32_ctx));
	return (hash_provider.type == GIT_HASH_WIN32_CNG) ? hash_sha256_cng_ctx_init(ctx) : hash_sha256_cryptoapi_ctx_init(ctx);
}

GIT_INLINE(int) hash_win32_init(git_hash_win32_ctx *ctx)
{
	return (hash_provider.type == GIT_HASH_WIN32_CNG) ? hash_cng_init(ctx) : hash_cryptoapi_init(ctx);
}

GIT_INLINE(int) hash_win32_update(git_hash_win32_ctx *ctx, const void *data, size_t len)
{
	return (hash_provider.type == GIT_HASH_WIN32_CNG) ? hash_cng_update(ctx, data, len) : hash_cryptoapi_update(ctx, data, len);
}

GIT_INLINE(int) hash_win32_final(unsigned char *out, git_hash_win32_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	return (hash_provider.type == GIT_HASH_WIN32_CNG) ? hash_cng_final(out, ctx) : hash_cryptoapi_final(out, ctx);
}

GIT_INLINE(void) hash_win32_cleanup(git_hash_win32_ctx *ctx)
{
	if (hash_provider.type == GIT_HASH_WIN32_CNG)
		hash_ctx_cng_cleanup(ctx);
	else if(hash_provider.type == GIT_HASH_WIN32_CRYPTOAPI)
		hash_ctx_cryptoapi_cleanup(ctx);
}

#ifdef GIT_SHA1_WIN32

int git_hash_sha1_global_init(void)
{
	return hash_provider_init();
}

int git_hash_sha1_ctx_init(git_hash_sha1_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	return hash_sha1_win32_ctx_init(&ctx->win32);
}

int git_hash_sha1_init(git_hash_sha1_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	return hash_win32_init(&ctx->win32);
}

int git_hash_sha1_update(git_hash_sha1_ctx *ctx, const void *data, size_t len)
{
	GIT_ASSERT_ARG(ctx);
	return hash_win32_update(&ctx->win32, data, len);
}

int git_hash_sha1_final(unsigned char *out, git_hash_sha1_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	return hash_win32_final(out, &ctx->win32);
}

void git_hash_sha1_ctx_cleanup(git_hash_sha1_ctx *ctx)
{
	if (!ctx)
		return;
	hash_win32_cleanup(&ctx->win32);
}

#endif

#ifdef GIT_SHA256_WIN32

int git_hash_sha256_global_init(void)
{
	return hash_provider_init();
}

int git_hash_sha256_ctx_init(git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	return hash_sha256_win32_ctx_init(&ctx->win32);
}

int git_hash_sha256_init(git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	return hash_win32_init(&ctx->win32);
}

int git_hash_sha256_update(git_hash_sha256_ctx *ctx, const void *data, size_t len)
{
	GIT_ASSERT_ARG(ctx);
	return hash_win32_update(&ctx->win32, data, len);
}

int git_hash_sha256_final(unsigned char *out, git_hash_sha256_ctx *ctx)
{
	GIT_ASSERT_ARG(ctx);
	return hash_win32_final(out, &ctx->win32);
}

void git_hash_sha256_ctx_cleanup(git_hash_sha256_ctx *ctx)
{
	if (!ctx)
		return;
	hash_win32_cleanup(&ctx->win32);
}

#endif
