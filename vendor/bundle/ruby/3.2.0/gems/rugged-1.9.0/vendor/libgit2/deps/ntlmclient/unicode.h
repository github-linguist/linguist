/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_UNICODE_H__
#define PRIVATE_UNICODE_H__

#include "compat.h"

#ifdef UNICODE_ICONV
# include "unicode_iconv.h"
#elif UNICODE_BUILTIN
# include "unicode_builtin.h"
#endif

#define NTLM_UNICODE_MAX_LEN	2048

typedef struct ntlm_unicode_ctx ntlm_unicode_ctx;

extern bool ntlm_unicode_init(ntlm_client *ntlm);

bool ntlm_unicode_utf8_to_16(
	char **converted,
	size_t *converted_len,
	ntlm_client *ntlm,
	const char *string,
	size_t string_len);

bool ntlm_unicode_utf16_to_8(
	char **converted,
	size_t *converted_len,
	ntlm_client *ntlm,
	const char *string,
	size_t string_len);

extern void ntlm_unicode_shutdown(ntlm_client *ntlm);

#endif /* PRIVATE_UNICODE_H__ */
