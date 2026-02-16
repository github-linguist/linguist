/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>

#include "compat.h"
#include "util.h"

void ntlm_memzero(void *data, size_t size)
{
	volatile uint8_t *scan = (volatile uint8_t *)data;

	while (size--)
		*scan++ = 0x0;
}

uint64_t ntlm_htonll(uint64_t value)
{
	static union {
		uint32_t i;
		char c[8];
	} test = { 0x01020304 };

	if (test.c[0] == 0x01)
		return value;
	else
		return ((uint64_t)htonl(value) << 32) | htonl((uint64_t)value >> 32);
}
