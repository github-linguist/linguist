/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "str.h"
#include "posix.h"
#include <ctype.h>

/* Used as default value for git_str->ptr so that people can always
 * assume ptr is non-NULL and zero terminated even for new git_strs.
 */
char git_str__initstr[1];

char git_str__oom[1];

#define ENSURE_SIZE(b, d) \
	if ((b)->ptr == git_str__oom || \
	    ((d) > (b)->asize && git_str_grow((b), (d)) < 0))\
		return -1;


int git_str_init(git_str *buf, size_t initial_size)
{
	buf->asize = 0;
	buf->size = 0;
	buf->ptr = git_str__initstr;

	ENSURE_SIZE(buf, initial_size);

	return 0;
}

int git_str_try_grow(
	git_str *buf, size_t target_size, bool mark_oom)
{
	char *new_ptr;
	size_t new_size;

	if (buf->ptr == git_str__oom)
		return -1;

	if (buf->asize == 0 && buf->size != 0) {
		git_error_set(GIT_ERROR_INVALID, "cannot grow a borrowed buffer");
		return GIT_EINVALID;
	}

	if (!target_size)
		target_size = buf->size;

	if (target_size <= buf->asize)
		return 0;

	if (buf->asize == 0) {
		new_size = target_size;
		new_ptr = NULL;
	} else {
		new_size = buf->asize;
		/*
		 * Grow the allocated buffer by 1.5 to allow
		 * re-use of memory holes resulting from the
		 * realloc. If this is still too small, then just
		 * use the target size.
		 */
		if ((new_size = (new_size << 1) - (new_size >> 1)) < target_size)
			new_size = target_size;
		new_ptr = buf->ptr;
	}

	/* round allocation up to multiple of 8 */
	new_size = (new_size + 7) & ~7;

	if (new_size < buf->size) {
		if (mark_oom) {
			if (buf->ptr && buf->ptr != git_str__initstr)
				git__free(buf->ptr);
			buf->ptr = git_str__oom;
		}

		git_error_set_oom();
		return -1;
	}

	new_ptr = git__realloc(new_ptr, new_size);

	if (!new_ptr) {
		if (mark_oom) {
			if (buf->ptr && (buf->ptr != git_str__initstr))
				git__free(buf->ptr);
			buf->ptr = git_str__oom;
		}
		return -1;
	}

	buf->asize = new_size;
	buf->ptr   = new_ptr;

	/* truncate the existing buffer size if necessary */
	if (buf->size >= buf->asize)
		buf->size = buf->asize - 1;
	buf->ptr[buf->size] = '\0';

	return 0;
}

int git_str_grow(git_str *buffer, size_t target_size)
{
	return git_str_try_grow(buffer, target_size, true);
}

int git_str_grow_by(git_str *buffer, size_t additional_size)
{
	size_t newsize;

	if (GIT_ADD_SIZET_OVERFLOW(&newsize, buffer->size, additional_size)) {
		buffer->ptr = git_str__oom;
		return -1;
	}

	return git_str_try_grow(buffer, newsize, true);
}

void git_str_dispose(git_str *buf)
{
	if (!buf) return;

	if (buf->asize > 0 && buf->ptr != NULL && buf->ptr != git_str__oom)
		git__free(buf->ptr);

	git_str_init(buf, 0);
}

void git_str_clear(git_str *buf)
{
	buf->size = 0;

	if (!buf->ptr) {
		buf->ptr = git_str__initstr;
		buf->asize = 0;
	}

	if (buf->asize > 0)
		buf->ptr[0] = '\0';
}

int git_str_set(git_str *buf, const void *data, size_t len)
{
	size_t alloclen;

	if (len == 0 || data == NULL) {
		git_str_clear(buf);
	} else {
		if (data != buf->ptr) {
			GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, len, 1);
			ENSURE_SIZE(buf, alloclen);
			memmove(buf->ptr, data, len);
		}

		buf->size = len;
		if (buf->asize > buf->size)
			buf->ptr[buf->size] = '\0';

	}
	return 0;
}

int git_str_sets(git_str *buf, const char *string)
{
	return git_str_set(buf, string, string ? strlen(string) : 0);
}

int git_str_putc(git_str *buf, char c)
{
	size_t new_size;
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, buf->size, 2);
	ENSURE_SIZE(buf, new_size);
	buf->ptr[buf->size++] = c;
	buf->ptr[buf->size] = '\0';
	return 0;
}

int git_str_putcn(git_str *buf, char c, size_t len)
{
	size_t new_size;
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, buf->size, len);
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);
	ENSURE_SIZE(buf, new_size);
	memset(buf->ptr + buf->size, c, len);
	buf->size += len;
	buf->ptr[buf->size] = '\0';
	return 0;
}

int git_str_put(git_str *buf, const char *data, size_t len)
{
	if (len) {
		size_t new_size;

		GIT_ASSERT_ARG(data);

		GIT_ERROR_CHECK_ALLOC_ADD(&new_size, buf->size, len);
		GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);
		ENSURE_SIZE(buf, new_size);
		memmove(buf->ptr + buf->size, data, len);
		buf->size += len;
		buf->ptr[buf->size] = '\0';
	}
	return 0;
}

int git_str_puts(git_str *buf, const char *string)
{
	GIT_ASSERT_ARG(string);

	return git_str_put(buf, string, strlen(string));
}

static char hex_encode[] = "0123456789abcdef";

int git_str_encode_hexstr(git_str *str, const char *data, size_t len)
{
	size_t new_size, i;
	char *s;

	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&new_size, len, 2);
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);

	if (git_str_grow_by(str, new_size) < 0)
		return -1;

	s = str->ptr + str->size;

	for (i = 0; i < len; i++) {
		*s++ = hex_encode[(data[i] & 0xf0) >> 4];
		*s++ = hex_encode[(data[i] & 0x0f)];
	}

	str->size += (len * 2);
	str->ptr[str->size] = '\0';

	return 0;
}

static const char base64_encode[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int git_str_encode_base64(git_str *buf, const char *data, size_t len)
{
	size_t extra = len % 3;
	uint8_t *write, a, b, c;
	const uint8_t *read = (const uint8_t *)data;
	size_t blocks = (len / 3) + !!extra, alloclen;

	GIT_ERROR_CHECK_ALLOC_ADD(&blocks, blocks, 1);
	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&alloclen, blocks, 4);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, buf->size);

	ENSURE_SIZE(buf, alloclen);
	write = (uint8_t *)&buf->ptr[buf->size];

	/* convert each run of 3 bytes into 4 output bytes */
	for (len -= extra; len > 0; len -= 3) {
		a = *read++;
		b = *read++;
		c = *read++;

		*write++ = base64_encode[a >> 2];
		*write++ = base64_encode[(a & 0x03) << 4 | b >> 4];
		*write++ = base64_encode[(b & 0x0f) << 2 | c >> 6];
		*write++ = base64_encode[c & 0x3f];
	}

	if (extra > 0) {
		a = *read++;
		b = (extra > 1) ? *read++ : 0;

		*write++ = base64_encode[a >> 2];
		*write++ = base64_encode[(a & 0x03) << 4 | b >> 4];
		*write++ = (extra > 1) ? base64_encode[(b & 0x0f) << 2] : '=';
		*write++ = '=';
	}

	buf->size = ((char *)write) - buf->ptr;
	buf->ptr[buf->size] = '\0';

	return 0;
}

/* The inverse of base64_encode */
static const int8_t base64_decode[] = {
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
	52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1,  0, -1, -1,
	-1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
	15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
	-1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
	41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};

int git_str_decode_base64(git_str *buf, const char *base64, size_t len)
{
	size_t i;
	int8_t a, b, c, d;
	size_t orig_size = buf->size, new_size;

	if (len % 4) {
		git_error_set(GIT_ERROR_INVALID, "invalid base64 input");
		return -1;
	}

	GIT_ASSERT_ARG(len % 4 == 0);
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, (len / 4 * 3), buf->size);
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);
	ENSURE_SIZE(buf, new_size);

	for (i = 0; i < len; i += 4) {
		if ((a = base64_decode[(unsigned char)base64[i]]) < 0 ||
			(b = base64_decode[(unsigned char)base64[i+1]]) < 0 ||
			(c = base64_decode[(unsigned char)base64[i+2]]) < 0 ||
			(d = base64_decode[(unsigned char)base64[i+3]]) < 0) {
			buf->size = orig_size;
			buf->ptr[buf->size] = '\0';

			git_error_set(GIT_ERROR_INVALID, "invalid base64 input");
			return -1;
		}

		buf->ptr[buf->size++] = ((a << 2) | (b & 0x30) >> 4);
		buf->ptr[buf->size++] = ((b & 0x0f) << 4) | ((c & 0x3c) >> 2);
		buf->ptr[buf->size++] = (c & 0x03) << 6 | (d & 0x3f);
	}

	buf->ptr[buf->size] = '\0';
	return 0;
}

static const char base85_encode[] =
	"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~";

int git_str_encode_base85(git_str *buf, const char *data, size_t len)
{
	size_t blocks = (len / 4) + !!(len % 4), alloclen;

	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&alloclen, blocks, 5);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, buf->size);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);

	ENSURE_SIZE(buf, alloclen);

	while (len) {
		uint32_t acc = 0;
		char b85[5];
		int i;

		for (i = 24; i >= 0; i -= 8) {
			uint8_t ch = *data++;
			acc |= (uint32_t)ch << i;

			if (--len == 0)
				break;
		}

		for (i = 4; i >= 0; i--) {
			int val = acc % 85;
			acc /= 85;

			b85[i] = base85_encode[val];
		}

		for (i = 0; i < 5; i++)
			buf->ptr[buf->size++] = b85[i];
	}

	buf->ptr[buf->size] = '\0';

	return 0;
}

/* The inverse of base85_encode */
static const int8_t base85_decode[] = {
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, 63, -1, 64, 65, 66, 67, -1, 68, 69, 70, 71, -1, 72, -1, -1,
	 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, -1, 73, 74, 75, 76, 77,
	78, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
	26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, -1, -1, -1, 79, 80,
	81, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
	52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 82, 83, 84, 85, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};

int git_str_decode_base85(
	git_str *buf,
	const char *base85,
	size_t base85_len,
	size_t output_len)
{
	size_t orig_size = buf->size, new_size;

	if (base85_len % 5 ||
		output_len > base85_len * 4 / 5) {
		git_error_set(GIT_ERROR_INVALID, "invalid base85 input");
		return -1;
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, output_len, buf->size);
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);
	ENSURE_SIZE(buf, new_size);

	while (output_len) {
		unsigned acc = 0;
		int de, cnt = 4;
		unsigned char ch;
		do {
			ch = *base85++;
			de = base85_decode[ch];
			if (--de < 0)
				goto on_error;

			acc = acc * 85 + de;
		} while (--cnt);
		ch = *base85++;
		de = base85_decode[ch];
		if (--de < 0)
			goto on_error;

		/* Detect overflow. */
		if (0xffffffff / 85 < acc ||
			0xffffffff - de < (acc *= 85))
			goto on_error;

		acc += de;

		cnt = (output_len < 4) ? (int)output_len : 4;
		output_len -= cnt;
		do {
			acc = (acc << 8) | (acc >> 24);
			buf->ptr[buf->size++] = acc;
		} while (--cnt);
	}

	buf->ptr[buf->size] = 0;

	return 0;

on_error:
	buf->size = orig_size;
	buf->ptr[buf->size] = '\0';

	git_error_set(GIT_ERROR_INVALID, "invalid base85 input");
	return -1;
}

#define HEX_DECODE(c) ((c | 32) % 39 - 9)

int git_str_decode_percent(
	git_str *buf,
	const char *str,
	size_t str_len)
{
	size_t str_pos, new_size;

	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, buf->size, str_len);
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);
	ENSURE_SIZE(buf, new_size);

	for (str_pos = 0; str_pos < str_len; buf->size++, str_pos++) {
		if (str[str_pos] == '%' &&
			str_len > str_pos + 2 &&
			git__isxdigit(str[str_pos + 1]) &&
			git__isxdigit(str[str_pos + 2])) {
			buf->ptr[buf->size] = (HEX_DECODE(str[str_pos + 1]) << 4) +
				HEX_DECODE(str[str_pos + 2]);
			str_pos += 2;
		} else {
			buf->ptr[buf->size] = str[str_pos];
		}
	}

	buf->ptr[buf->size] = '\0';
	return 0;
}

int git_str_vprintf(git_str *buf, const char *format, va_list ap)
{
	size_t expected_size, new_size;
	int len;

	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&expected_size, strlen(format), 2);
	GIT_ERROR_CHECK_ALLOC_ADD(&expected_size, expected_size, buf->size);
	ENSURE_SIZE(buf, expected_size);

	while (1) {
		va_list args;
		va_copy(args, ap);

		len = p_vsnprintf(
			buf->ptr + buf->size,
			buf->asize - buf->size,
			format, args
		);

		va_end(args);

		if (len < 0) {
			git__free(buf->ptr);
			buf->ptr = git_str__oom;
			return -1;
		}

		if ((size_t)len + 1 <= buf->asize - buf->size) {
			buf->size += len;
			break;
		}

		GIT_ERROR_CHECK_ALLOC_ADD(&new_size, buf->size, len);
		GIT_ERROR_CHECK_ALLOC_ADD(&new_size, new_size, 1);
		ENSURE_SIZE(buf, new_size);
	}

	return 0;
}

int git_str_printf(git_str *buf, const char *format, ...)
{
	int r;
	va_list ap;

	va_start(ap, format);
	r = git_str_vprintf(buf, format, ap);
	va_end(ap);

	return r;
}

int git_str_copy_cstr(char *data, size_t datasize, const git_str *buf)
{
	size_t copylen;

	GIT_ASSERT_ARG(data);
	GIT_ASSERT_ARG(datasize);
	GIT_ASSERT_ARG(buf);

	data[0] = '\0';

	if (buf->size == 0 || buf->asize <= 0)
		return 0;

	copylen = buf->size;
	if (copylen > datasize - 1)
		copylen = datasize - 1;
	memmove(data, buf->ptr, copylen);
	data[copylen] = '\0';

	return 0;
}

void git_str_consume_bytes(git_str *buf, size_t len)
{
	git_str_consume(buf, buf->ptr + len);
}

void git_str_consume(git_str *buf, const char *end)
{
	if (end > buf->ptr && end <= buf->ptr + buf->size) {
		size_t consumed = end - buf->ptr;
		memmove(buf->ptr, end, buf->size - consumed);
		buf->size -= consumed;
		buf->ptr[buf->size] = '\0';
	}
}

void git_str_truncate(git_str *buf, size_t len)
{
	if (len >= buf->size)
		return;

	buf->size = len;
	if (buf->size < buf->asize)
		buf->ptr[buf->size] = '\0';
}

void git_str_shorten(git_str *buf, size_t amount)
{
	if (buf->size > amount)
		git_str_truncate(buf, buf->size - amount);
	else
		git_str_clear(buf);
}

void git_str_truncate_at_char(git_str *buf, char separator)
{
	ssize_t idx = git_str_find(buf, separator);
	if (idx >= 0)
		git_str_truncate(buf, (size_t)idx);
}

void git_str_rtruncate_at_char(git_str *buf, char separator)
{
	ssize_t idx = git_str_rfind_next(buf, separator);
	git_str_truncate(buf, idx < 0 ? 0 : (size_t)idx);
}

void git_str_swap(git_str *str_a, git_str *str_b)
{
	git_str t = *str_a;
	*str_a = *str_b;
	*str_b = t;
}

char *git_str_detach(git_str *buf)
{
	char *data = buf->ptr;

	if (buf->asize == 0 || buf->ptr == git_str__oom)
		return NULL;

	git_str_init(buf, 0);

	return data;
}

int git_str_attach(git_str *buf, char *ptr, size_t asize)
{
	git_str_dispose(buf);

	if (ptr) {
		buf->ptr = ptr;
		buf->size = strlen(ptr);
		if (asize)
			buf->asize = (asize < buf->size) ? buf->size + 1 : asize;
		else /* pass 0 to fall back on strlen + 1 */
			buf->asize = buf->size + 1;
	}

	ENSURE_SIZE(buf, asize);
	return 0;
}

void git_str_attach_notowned(git_str *buf, const char *ptr, size_t size)
{
	if (git_str_is_allocated(buf))
		git_str_dispose(buf);

	if (!size) {
		git_str_init(buf, 0);
	} else {
		buf->ptr = (char *)ptr;
		buf->asize = 0;
		buf->size = size;
	}
}

int git_str_join_n(git_str *buf, char separator, int nbuf, ...)
{
	va_list ap;
	int i;
	size_t total_size = 0, original_size = buf->size;
	char *out, *original = buf->ptr;

	if (buf->size > 0 && buf->ptr[buf->size - 1] != separator)
		++total_size; /* space for initial separator */

	/* Make two passes to avoid multiple reallocation */

	va_start(ap, nbuf);
	for (i = 0; i < nbuf; ++i) {
		const char *segment;
		size_t segment_len;

		segment = va_arg(ap, const char *);
		if (!segment)
			continue;

		segment_len = strlen(segment);

		GIT_ERROR_CHECK_ALLOC_ADD(&total_size, total_size, segment_len);

		if (segment_len == 0 || segment[segment_len - 1] != separator)
			GIT_ERROR_CHECK_ALLOC_ADD(&total_size, total_size, 1);
	}
	va_end(ap);

	/* expand buffer if needed */
	if (total_size == 0)
		return 0;

	GIT_ERROR_CHECK_ALLOC_ADD(&total_size, total_size, 1);
	if (git_str_grow_by(buf, total_size) < 0)
		return -1;

	out = buf->ptr + buf->size;

	/* append separator to existing buf if needed */
	if (buf->size > 0 && out[-1] != separator)
		*out++ = separator;

	va_start(ap, nbuf);
	for (i = 0; i < nbuf; ++i) {
		const char *segment;
		size_t segment_len;

		segment = va_arg(ap, const char *);
		if (!segment)
			continue;

		/* deal with join that references buffer's original content */
		if (segment >= original && segment < original + original_size) {
			size_t offset = (segment - original);
			segment = buf->ptr + offset;
			segment_len = original_size - offset;
		} else {
			segment_len = strlen(segment);
		}

		/* skip leading separators */
		if (out > buf->ptr && out[-1] == separator)
			while (segment_len > 0 && *segment == separator) {
				segment++;
				segment_len--;
			}

		/* copy over next buffer */
		if (segment_len > 0) {
			memmove(out, segment, segment_len);
			out += segment_len;
		}

		/* append trailing separator (except for last item) */
		if (i < nbuf - 1 && out > buf->ptr && out[-1] != separator)
			*out++ = separator;
	}
	va_end(ap);

	/* set size based on num characters actually written */
	buf->size = out - buf->ptr;
	buf->ptr[buf->size] = '\0';

	return 0;
}

int git_str_join(
	git_str *buf,
	char separator,
	const char *str_a,
	const char *str_b)
{
	size_t strlen_a = str_a ? strlen(str_a) : 0;
	size_t strlen_b = strlen(str_b);
	size_t alloc_len;
	int need_sep = 0;
	ssize_t offset_a = -1;

	/* not safe to have str_b point internally to the buffer */
	if (buf->size)
		GIT_ASSERT_ARG(str_b < buf->ptr || str_b >= buf->ptr + buf->size);

	/* figure out if we need to insert a separator */
	if (separator && strlen_a) {
		while (*str_b == separator) { str_b++; strlen_b--; }
		if (str_a[strlen_a - 1] != separator)
			need_sep = 1;
	}

	/* str_a could be part of the buffer */
	if (buf->size && str_a >= buf->ptr && str_a < buf->ptr + buf->size)
		offset_a = str_a - buf->ptr;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, strlen_a, strlen_b);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, need_sep);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 1);
	ENSURE_SIZE(buf, alloc_len);

	/* fix up internal pointers */
	if (offset_a >= 0)
		str_a = buf->ptr + offset_a;

	/* do the actual copying */
	if (offset_a != 0 && str_a)
		memmove(buf->ptr, str_a, strlen_a);
	if (need_sep)
		buf->ptr[strlen_a] = separator;
	memcpy(buf->ptr + strlen_a + need_sep, str_b, strlen_b);

	buf->size = strlen_a + strlen_b + need_sep;
	buf->ptr[buf->size] = '\0';

	return 0;
}

int git_str_join3(
	git_str *buf,
	char separator,
	const char *str_a,
	const char *str_b,
	const char *str_c)
{
	size_t len_a = strlen(str_a),
		len_b = strlen(str_b),
		len_c = strlen(str_c),
		len_total;
	int sep_a = 0, sep_b = 0;
	char *tgt;

	/* for this function, disallow pointers into the existing buffer */
	GIT_ASSERT(str_a < buf->ptr || str_a >= buf->ptr + buf->size);
	GIT_ASSERT(str_b < buf->ptr || str_b >= buf->ptr + buf->size);
	GIT_ASSERT(str_c < buf->ptr || str_c >= buf->ptr + buf->size);

	if (separator) {
		if (len_a > 0) {
			while (*str_b == separator) { str_b++; len_b--; }
			sep_a = (str_a[len_a - 1] != separator);
		}
		if (len_a > 0 || len_b > 0)
			while (*str_c == separator) { str_c++; len_c--; }
		if (len_b > 0)
			sep_b = (str_b[len_b - 1] != separator);
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&len_total, len_a, sep_a);
	GIT_ERROR_CHECK_ALLOC_ADD(&len_total, len_total, len_b);
	GIT_ERROR_CHECK_ALLOC_ADD(&len_total, len_total, sep_b);
	GIT_ERROR_CHECK_ALLOC_ADD(&len_total, len_total, len_c);
	GIT_ERROR_CHECK_ALLOC_ADD(&len_total, len_total, 1);
	ENSURE_SIZE(buf, len_total);

	tgt = buf->ptr;

	if (len_a) {
		memcpy(tgt, str_a, len_a);
		tgt += len_a;
	}
	if (sep_a)
		*tgt++ = separator;
	if (len_b) {
		memcpy(tgt, str_b, len_b);
		tgt += len_b;
	}
	if (sep_b)
		*tgt++ = separator;
	if (len_c)
		memcpy(tgt, str_c, len_c);

	buf->size = len_a + sep_a + len_b + sep_b + len_c;
	buf->ptr[buf->size] = '\0';

	return 0;
}

void git_str_rtrim(git_str *buf)
{
	while (buf->size > 0) {
		if (!git__isspace(buf->ptr[buf->size - 1]))
			break;

		buf->size--;
	}

	if (buf->asize > buf->size)
		buf->ptr[buf->size] = '\0';
}

int git_str_cmp(const git_str *a, const git_str *b)
{
	int result = memcmp(a->ptr, b->ptr, min(a->size, b->size));
	return (result != 0) ? result :
		(a->size < b->size) ? -1 : (a->size > b->size) ? 1 : 0;
}

int git_str_splice(
	git_str *buf,
	size_t where,
	size_t nb_to_remove,
	const char *data,
	size_t nb_to_insert)
{
	char *splice_loc;
	size_t new_size, alloc_size;

	GIT_ASSERT(buf);
	GIT_ASSERT(where <= buf->size);
	GIT_ASSERT(nb_to_remove <= buf->size - where);

	splice_loc = buf->ptr + where;

	/* Ported from git.git
	 * https://github.com/git/git/blob/16eed7c/strbuf.c#L159-176
	 */
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, (buf->size - nb_to_remove), nb_to_insert);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_size, new_size, 1);
	ENSURE_SIZE(buf, alloc_size);

	memmove(splice_loc + nb_to_insert,
		splice_loc + nb_to_remove,
		buf->size - where - nb_to_remove);

	memcpy(splice_loc, data, nb_to_insert);

	buf->size = new_size;
	buf->ptr[buf->size] = '\0';
	return 0;
}

/* Quote per http://marc.info/?l=git&m=112927316408690&w=2 */
int git_str_quote(git_str *buf)
{
	const char whitespace[] = { 'a', 'b', 't', 'n', 'v', 'f', 'r' };
	git_str quoted = GIT_STR_INIT;
	size_t i = 0;
	bool quote = false;
	int error = 0;

	/* walk to the first char that needs quoting */
	if (buf->size && buf->ptr[0] == '!')
		quote = true;

	for (i = 0; !quote && i < buf->size; i++) {
		if (buf->ptr[i] == '"' || buf->ptr[i] == '\\' ||
			buf->ptr[i] < ' ' || buf->ptr[i] > '~') {
			quote = true;
			break;
		}
	}

	if (!quote)
		goto done;

	git_str_putc(&quoted, '"');
	git_str_put(&quoted, buf->ptr, i);

	for (; i < buf->size; i++) {
		/* whitespace - use the map above, which is ordered by ascii value */
		if (buf->ptr[i] >= '\a' && buf->ptr[i] <= '\r') {
			git_str_putc(&quoted, '\\');
			git_str_putc(&quoted, whitespace[buf->ptr[i] - '\a']);
		}

		/* double quote and backslash must be escaped */
		else if (buf->ptr[i] == '"' || buf->ptr[i] == '\\') {
			git_str_putc(&quoted, '\\');
			git_str_putc(&quoted, buf->ptr[i]);
		}

		/* escape anything unprintable as octal */
		else if (buf->ptr[i] != ' ' &&
				(buf->ptr[i] < '!' || buf->ptr[i] > '~')) {
			git_str_printf(&quoted, "\\%03o", (unsigned char)buf->ptr[i]);
		}

		/* yay, printable! */
		else {
			git_str_putc(&quoted, buf->ptr[i]);
		}
	}

	git_str_putc(&quoted, '"');

	if (git_str_oom(&quoted)) {
		error = -1;
		goto done;
	}

	git_str_swap(&quoted, buf);

done:
	git_str_dispose(&quoted);
	return error;
}

/* Unquote per http://marc.info/?l=git&m=112927316408690&w=2 */
int git_str_unquote(git_str *buf)
{
	size_t i, j;
	char ch;

	git_str_rtrim(buf);

	if (buf->size < 2 || buf->ptr[0] != '"' || buf->ptr[buf->size-1] != '"')
		goto invalid;

	for (i = 0, j = 1; j < buf->size-1; i++, j++) {
		ch = buf->ptr[j];

		if (ch == '\\') {
			if (j == buf->size-2)
				goto invalid;

			ch = buf->ptr[++j];

			switch (ch) {
			/* \" or \\ simply copy the char in */
			case '"': case '\\':
				break;

			/* add the appropriate escaped char */
			case 'a': ch = '\a'; break;
			case 'b': ch = '\b'; break;
			case 'f': ch = '\f'; break;
			case 'n': ch = '\n'; break;
			case 'r': ch = '\r'; break;
			case 't': ch = '\t'; break;
			case 'v': ch = '\v'; break;

			/* \xyz digits convert to the char*/
			case '0': case '1': case '2': case '3':
				if (j == buf->size-3) {
					git_error_set(GIT_ERROR_INVALID,
						"truncated quoted character \\%c", ch);
					return -1;
				}

				if (buf->ptr[j+1] < '0' || buf->ptr[j+1] > '7' ||
					buf->ptr[j+2] < '0' || buf->ptr[j+2] > '7') {
					git_error_set(GIT_ERROR_INVALID,
						"truncated quoted character \\%c%c%c",
						buf->ptr[j], buf->ptr[j+1], buf->ptr[j+2]);
					return -1;
				}

				ch = ((buf->ptr[j] - '0') << 6) |
					((buf->ptr[j+1] - '0') << 3) |
					(buf->ptr[j+2] - '0');
				j += 2;
				break;

			default:
				git_error_set(GIT_ERROR_INVALID, "invalid quoted character \\%c", ch);
				return -1;
			}
		}

		buf->ptr[i] = ch;
	}

	buf->ptr[i] = '\0';
	buf->size = i;

	return 0;

invalid:
	git_error_set(GIT_ERROR_INVALID, "invalid quoted line");
	return -1;
}

int git_str_puts_escaped(
	git_str *buf,
	const char *string,
	const char *esc_chars,
	const char *esc_with)
{
	const char *scan;
	size_t total = 0, esc_len = strlen(esc_with), count, alloclen;

	if (!string)
		return 0;

	for (scan = string; *scan; ) {
		/* count run of non-escaped characters */
		count = strcspn(scan, esc_chars);
		total += count;
		scan += count;
		/* count run of escaped characters */
		count = strspn(scan, esc_chars);
		total += count * (esc_len + 1);
		scan += count;
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, total, 1);
	if (git_str_grow_by(buf, alloclen) < 0)
		return -1;

	for (scan = string; *scan; ) {
		count = strcspn(scan, esc_chars);

		memmove(buf->ptr + buf->size, scan, count);
		scan += count;
		buf->size += count;

		for (count = strspn(scan, esc_chars); count > 0; --count) {
			/* copy escape sequence */
			memmove(buf->ptr + buf->size, esc_with, esc_len);
			buf->size += esc_len;
			/* copy character to be escaped */
			buf->ptr[buf->size] = *scan;
			buf->size++;
			scan++;
		}
	}

	buf->ptr[buf->size] = '\0';

	return 0;
}

void git_str_unescape(git_str *buf)
{
	buf->size = git__unescape(buf->ptr);
}

int git_str_crlf_to_lf(git_str *tgt, const git_str *src)
{
	const char *scan = src->ptr;
	const char *scan_end = src->ptr + src->size;
	const char *next = memchr(scan, '\r', src->size);
	size_t new_size;
	char *out;

	GIT_ASSERT(tgt != src);

	if (!next)
		return git_str_set(tgt, src->ptr, src->size);

	/* reduce reallocs while in the loop */
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, src->size, 1);
	if (git_str_grow(tgt, new_size) < 0)
		return -1;

	out = tgt->ptr;
	tgt->size = 0;

	/* Find the next \r and copy whole chunk up to there to tgt */
	for (; next; scan = next + 1, next = memchr(scan, '\r', scan_end - scan)) {
		if (next > scan) {
			size_t copylen = (size_t)(next - scan);
			memcpy(out, scan, copylen);
			out += copylen;
		}

		/* Do not drop \r unless it is followed by \n */
		if (next + 1 == scan_end || next[1] != '\n')
			*out++ = '\r';
	}

	/* Copy remaining input into dest */
	if (scan < scan_end) {
		size_t remaining = (size_t)(scan_end - scan);
		memcpy(out, scan, remaining);
		out += remaining;
	}

	tgt->size = (size_t)(out - tgt->ptr);
	tgt->ptr[tgt->size] = '\0';

	return 0;
}

int git_str_lf_to_crlf(git_str *tgt, const git_str *src)
{
	const char *start = src->ptr;
	const char *end = start + src->size;
	const char *scan = start;
	const char *next = memchr(scan, '\n', src->size);
	size_t alloclen;

	GIT_ASSERT(tgt != src);

	if (!next)
		return git_str_set(tgt, src->ptr, src->size);

	/* attempt to reduce reallocs while in the loop */
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, src->size, src->size >> 4);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	if (git_str_grow(tgt, alloclen) < 0)
		return -1;
	tgt->size = 0;

	for (; next; scan = next + 1, next = memchr(scan, '\n', end - scan)) {
		size_t copylen = next - scan;

		/* if we find mixed line endings, carry on */
		if (copylen && next[-1] == '\r')
			copylen--;

		GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, copylen, 3);
		if (git_str_grow_by(tgt, alloclen) < 0)
			return -1;

		if (copylen) {
			memcpy(tgt->ptr + tgt->size, scan, copylen);
			tgt->size += copylen;
		}

		tgt->ptr[tgt->size++] = '\r';
		tgt->ptr[tgt->size++] = '\n';
	}

	tgt->ptr[tgt->size] = '\0';
	return git_str_put(tgt, scan, end - scan);
}

int git_str_common_prefix(git_str *buf, char *const *const strings, size_t count)
{
	size_t i;
	const char *str, *pfx;

	git_str_clear(buf);

	if (!strings || !count)
		return 0;

	/* initialize common prefix to first string */
	if (git_str_sets(buf, strings[0]) < 0)
		return -1;

	/* go through the rest of the strings, truncating to shared prefix */
	for (i = 1; i < count; ++i) {

		for (str = strings[i], pfx = buf->ptr;
		     *str && *str == *pfx;
		     str++, pfx++)
			/* scanning */;

		git_str_truncate(buf, pfx - buf->ptr);

		if (!buf->size)
			break;
	}

	return 0;
}

int git_str_is_binary(const git_str *buf)
{
	const char *scan = buf->ptr, *end = buf->ptr + buf->size;
	git_str_bom_t bom;
	int printable = 0, nonprintable = 0;

	scan += git_str_detect_bom(&bom, buf);

	if (bom > GIT_STR_BOM_UTF8)
		return 1;

	while (scan < end) {
		unsigned char c = *scan++;

		/* Printable characters are those above SPACE (0x1F) excluding DEL,
		 * and including BS, ESC and FF.
		 */
		if ((c > 0x1F && c != 127) || c == '\b' || c == '\033' || c == '\014')
			printable++;
		else if (c == '\0')
			return true;
		else if (!git__isspace(c))
			nonprintable++;
	}

	return ((printable >> 7) < nonprintable);
}

int git_str_contains_nul(const git_str *buf)
{
	return (memchr(buf->ptr, '\0', buf->size) != NULL);
}

int git_str_detect_bom(git_str_bom_t *bom, const git_str *buf)
{
	const char *ptr;
	size_t len;

	*bom = GIT_STR_BOM_NONE;
	/* need at least 2 bytes to look for any BOM */
	if (buf->size < 2)
		return 0;

	ptr = buf->ptr;
	len = buf->size;

	switch (*ptr++) {
	case 0:
		if (len >= 4 && ptr[0] == 0 && ptr[1] == '\xFE' && ptr[2] == '\xFF') {
			*bom = GIT_STR_BOM_UTF32_BE;
			return 4;
		}
		break;
	case '\xEF':
		if (len >= 3 && ptr[0] == '\xBB' && ptr[1] == '\xBF') {
			*bom = GIT_STR_BOM_UTF8;
			return 3;
		}
		break;
	case '\xFE':
		if (*ptr == '\xFF') {
			*bom = GIT_STR_BOM_UTF16_BE;
			return 2;
		}
		break;
	case '\xFF':
		if (*ptr != '\xFE')
			break;
		if (len >= 4 && ptr[1] == 0 && ptr[2] == 0) {
			*bom = GIT_STR_BOM_UTF32_LE;
			return 4;
		} else {
			*bom = GIT_STR_BOM_UTF16_LE;
			return 2;
		}
		break;
	default:
		break;
	}

	return 0;
}

bool git_str_gather_text_stats(
	git_str_text_stats *stats, const git_str *buf, bool skip_bom)
{
	const char *scan = buf->ptr, *end = buf->ptr + buf->size;
	int skip;

	memset(stats, 0, sizeof(*stats));

	/* BOM detection */
	skip = git_str_detect_bom(&stats->bom, buf);
	if (skip_bom)
		scan += skip;

	/* Ignore EOF character */
	if (buf->size > 0 && end[-1] == '\032')
		end--;

	/* Counting loop */
	while (scan < end) {
		unsigned char c = *scan++;

		if (c > 0x1F && c != 0x7F)
			stats->printable++;
		else switch (c) {
			case '\0':
				stats->nul++;
				stats->nonprintable++;
				break;
			case '\n':
				stats->lf++;
				break;
			case '\r':
				stats->cr++;
				if (scan < end && *scan == '\n')
					stats->crlf++;
				break;
			case '\t': case '\f': case '\v': case '\b': case 0x1b: /*ESC*/
				stats->printable++;
				break;
			default:
				stats->nonprintable++;
				break;
			}
	}

	/* Treat files with a bare CR as binary */
	return (stats->cr != stats->crlf || stats->nul > 0 ||
		((stats->printable >> 7) < stats->nonprintable));
}
