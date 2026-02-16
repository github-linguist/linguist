/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "utf8.h"

#include "git2_util.h"

/*
 * git_utf8_iterate is taken from the utf8proc project,
 * http://www.public-software-group.org/utf8proc
 *
 * Copyright (c) 2009 Public Software Group e. V., Berlin, Germany
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the ""Software""),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

static const uint8_t utf8proc_utf8class[256] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0
};

static int utf8_charlen(const uint8_t *str, size_t str_len)
{
	uint8_t length;
	size_t i;

	length = utf8proc_utf8class[str[0]];
	if (!length)
		return -1;

	if (str_len > 0 && length > str_len)
		return -1;

	for (i = 1; i < length; i++) {
		if ((str[i] & 0xC0) != 0x80)
			return -1;
	}

	return (int)length;
}

int git_utf8_iterate(uint32_t *out, const char *_str, size_t str_len)
{
	const uint8_t *str = (const uint8_t *)_str;
	uint32_t uc = 0;
	int length;

	*out = 0;

	if ((length = utf8_charlen(str, str_len)) < 0)
		return -1;

	switch (length) {
		case 1:
			uc = str[0];
			break;
		case 2:
			uc = ((str[0] & 0x1F) <<  6) + (str[1] & 0x3F);
			if (uc < 0x80) uc = -1;
			break;
		case 3:
			uc = ((str[0] & 0x0F) << 12) + ((str[1] & 0x3F) <<  6)
				+ (str[2] & 0x3F);
			if (uc < 0x800 || (uc >= 0xD800 && uc < 0xE000) ||
					(uc >= 0xFDD0 && uc < 0xFDF0)) uc = -1;
			break;
		case 4:
			uc = ((str[0] & 0x07) << 18) + ((str[1] & 0x3F) << 12)
				+ ((str[2] & 0x3F) <<  6) + (str[3] & 0x3F);
			if (uc < 0x10000 || uc >= 0x110000) uc = -1;
			break;
		default:
			return -1;
	}

	if ((uc & 0xFFFF) >= 0xFFFE)
		return -1;

	*out = uc;
	return length;
}

size_t git_utf8_char_length(const char *_str, size_t str_len)
{
	const uint8_t *str = (const uint8_t *)_str;
	size_t offset = 0, count = 0;

	while (offset < str_len) {
		int length = utf8_charlen(str + offset, str_len - offset);

		if (length < 0)
			length = 1;

		offset += length;
		count++;
	}

	return count;
}

size_t git_utf8_valid_buf_length(const char *_str, size_t str_len)
{
	const uint8_t *str = (const uint8_t *)_str;
	size_t offset = 0;

	while (offset < str_len) {
		int length = utf8_charlen(str + offset, str_len - offset);

		if (length < 0)
			break;

		offset += length;
	}

	return offset;
}
