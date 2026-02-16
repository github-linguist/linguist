/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_NTLM_H__
#define PRIVATE_NTLM_H__

#include "ntlmclient.h"
#include "unicode.h"
#include "crypt.h"
#include "compat.h"

#define NTLM_UNUSED(x) ((void)(x))

#define NTLM_LM_RESPONSE_LEN 24
#define NTLM_NTLM_RESPONSE_LEN 24
#define NTLM_NTLM_HASH_LEN 16
#define NTLM_NTLM2_HASH_LEN 16

#define NTLM_SIGNATURE { 'N', 'T', 'L', 'M', 'S', 'S', 'P', 0x00 }

#define NTLM_LM_PLAINTEXT { 0x4b, 0x47, 0x53, 0x21, 0x40, 0x23, 0x24, 0x25 }

typedef enum {
	NTLM_STATE_NEGOTIATE = 0,
	NTLM_STATE_CHALLENGE = 1,
	NTLM_STATE_RESPONSE = 2,
	NTLM_STATE_ERROR = 3,
	NTLM_STATE_COMPLETE = 4
} ntlm_state;

typedef struct {
	unsigned char *buf;
	size_t pos;
	size_t len;
} ntlm_buf;

typedef struct {
	uint8_t major;
	uint8_t minor;
	uint16_t build;
	uint32_t reserved;
} ntlm_version;

typedef struct {
	uint32_t flags;
	uint64_t nonce;
	ntlm_version target_version;

	/* The unparsed target information from the server */
	unsigned char *target_info;
	size_t target_info_len;

	/* The target information parsed into usable strings */
	char *target;
	char *target_server;
	char *target_domain;
	char *target_server_dns;
	char *target_domain_dns;
} ntlm_challenge;

struct ntlm_client {
	ntlm_client_flags flags;

	ntlm_state state;

	/* subsystem contexts */
	ntlm_crypt_ctx crypt_ctx;
	ntlm_unicode_ctx unicode_ctx;
	int crypt_initialized : 1,
	    unicode_initialized : 1;

	/* error message as set by the library */
	const char *errmsg;

	char *hostname;
	char *hostdomain;
	ntlm_version host_version;

	char *target;

	char *username;
	char *username_upper;
	char *userdomain;
	char *password;

	/* strings as converted to utf16 */
	char *hostname_utf16;
	char *target_utf16;
	char *username_utf16;
	char *username_upper_utf16;
	char *userdomain_utf16;
	char *password_utf16;

	size_t hostname_utf16_len;
	size_t username_utf16_len;
	size_t username_upper_utf16_len;
	size_t userdomain_utf16_len;
	size_t password_utf16_len;
	size_t target_utf16_len;

	/* timestamp and nonce; only for debugging */
	uint64_t nonce;
	uint64_t timestamp;

	unsigned char lm_response[NTLM_LM_RESPONSE_LEN];
	size_t lm_response_len;

	unsigned char ntlm_response[NTLM_NTLM_RESPONSE_LEN];
	size_t ntlm_response_len;

	unsigned char *ntlm2_response;
	size_t ntlm2_response_len;

	ntlm_buf negotiate;
	ntlm_challenge challenge;
	ntlm_buf response;
};

typedef enum {
	NTLM_ENABLE_HOSTVERSION = (1 << 31)
} ntlm_client_internal_flags;

typedef enum {
	NTLM_TARGET_INFO_END = 0,
	NTLM_TARGET_INFO_SERVER = 1,
	NTLM_TARGET_INFO_DOMAIN = 2,
	NTLM_TARGET_INFO_SERVER_DNS = 3,
	NTLM_TARGET_INFO_DOMAIN_DNS = 4
} ntlm_target_info_type_t;

typedef enum {
	/* Unicode strings are supported in security buffers */
	NTLM_NEGOTIATE_UNICODE = 0x00000001,

	/* OEM (ANSI) strings are supported in security buffers */
	NTLM_NEGOTIATE_OEM = 0x00000002,

	/* Request the target realm from the server */
	NTLM_NEGOTIATE_REQUEST_TARGET = 0x00000004,

	/* NTLM authentication is supported */
	NTLM_NEGOTIATE_NTLM = 0x00000200,

	/* Negotiate domain name */
	NTLM_NEGOTIATE_DOMAIN_SUPPLIED = 0x00001000,

	/* Negotiate workstation (client) name */
	NTLM_NEGOTIATE_WORKSTATION_SUPPLIED = 0x00002000,

	/* Indicates that a local context is available */
	NTLM_NEGOTIATE_LOCAL_CALL = 0x00004000,

	/* Request a dummy signature */
	NTLM_NEGOTIATE_ALWAYS_SIGN = 0x00008000,

	/* Target (server) is a domain */
	NTLM_NEGOTIATE_TYPE_DOMAIN = 0x00010000,

	/* NTLM2 signing and sealing is supported */
	NTLM_NEGOTIATE_NTLM2_SIGN_AND_SEAL = 0x00080000,

	/* A target information block is included */
	NTLM_NEGOTIATE_TARGET_INFO = 0x00800000,

	/* Version information should be provided */
	NTLM_NEGOTIATE_VERSION = 0x01000000
} ntlm_negotiate_t;

extern int ntlm_client_set_nonce(ntlm_client *ntlm, uint64_t nonce);
extern int ntlm_client_set_timestamp(ntlm_client *ntlm, uint64_t timestamp);
extern void ntlm_client_set_errmsg(ntlm_client *ntlm, const char *errmsg);

#endif /* PRIVATE_NTLM_H__ */
