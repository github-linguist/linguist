/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "varint.h"

uintmax_t git_decode_varint(const unsigned char *bufp, size_t *varint_len)
{
	const unsigned char *buf = bufp;
	unsigned char c = *buf++;
	uintmax_t val = c & 127;
	while (c & 128) {
		val += 1;
		if (!val || MSB(val, 7)) {
			/* This is not a valid varint_len, so it signals
			   the error */
			*varint_len = 0;
			return 0; /* overflow */
		}
		c = *buf++;
		val = (val << 7) + (c & 127);
	}
	*varint_len = buf - bufp;
	return val;
}

int git_encode_varint(unsigned char *buf, size_t bufsize, uintmax_t value)
{
	unsigned char varint[16];
	unsigned pos = sizeof(varint) - 1;
	varint[pos] = value & 127;
	while (value >>= 7)
		varint[--pos] = 128 | (--value & 127);
	if (buf) {
		if (bufsize < (sizeof(varint) - pos))
			return -1;
		memcpy(buf, varint + pos, sizeof(varint) - pos);
	}
	return sizeof(varint) - pos;
}
