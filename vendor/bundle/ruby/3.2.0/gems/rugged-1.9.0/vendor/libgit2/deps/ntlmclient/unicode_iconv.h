/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#ifndef PRIVATE_UNICODE_ICONV_H__
#define PRIVATE_UNICODE_ICONV_H__

#include <locale.h>
#include <iconv.h>

#include "ntlmclient.h"

struct ntlm_unicode_ctx {
	iconv_t utf8_to_16;
	iconv_t utf16_to_8;
};

#endif /* PRIVATE_UNICODE_ICONV_H__ */
