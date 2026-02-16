/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "util.h"

#include "git2_util.h"

#ifdef GIT_WIN32
# include "win32/utf-conv.h"
# include "win32/w32_buffer.h"

# ifndef WIN32_LEAN_AND_MEAN
#  define WIN32_LEAN_AND_MEAN
# endif
# include <windows.h>

# ifdef GIT_QSORT_MSC
#  include <search.h>
# endif
#endif

#ifdef _MSC_VER
# include <Shlwapi.h>
#endif

#if defined(hpux) || defined(__hpux) || defined(_hpux)
# include <sys/pstat.h>
#endif

int git__strntol64(int64_t *result, const char *nptr, size_t nptr_len, const char **endptr, int base)
{
	const char *p;
	int64_t n, nn, v;
	int c, ovfl, neg, ndig;

	p = nptr;
	neg = 0;
	n = 0;
	ndig = 0;
	ovfl = 0;

	/*
	 * White space
	 */
	while (nptr_len && git__isspace(*p))
		p++, nptr_len--;

	if (!nptr_len)
		goto Return;

	/*
	 * Sign
	 */
	if (*p == '-' || *p == '+') {
		if (*p == '-')
			neg = 1;
		p++;
		nptr_len--;
	}

	if (!nptr_len)
		goto Return;

	/*
	 * Automatically detect the base if none was given to us.
	 * Right now, we assume that a number starting with '0x'
	 * is hexadecimal and a number starting with '0' is
	 * octal.
	 */
	if (base == 0) {
		if (*p != '0')
			base = 10;
		else if (nptr_len > 2 && (p[1] == 'x' || p[1] == 'X'))
			base = 16;
		else
			base = 8;
	}

	if (base < 0 || 36 < base)
		goto Return;

	/*
	 * Skip prefix of '0x'-prefixed hexadecimal numbers. There is no
	 * need to do the same for '0'-prefixed octal numbers as a
	 * leading '0' does not have any impact. Also, if we skip a
	 * leading '0' in such a string, then we may end up with no
	 * digits left and produce an error later on which isn't one.
	 */
	if (base == 16 && nptr_len > 2 && p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
		p += 2;
		nptr_len -= 2;
	}

	/*
	 * Non-empty sequence of digits
	 */
	for (; nptr_len > 0; p++,ndig++,nptr_len--) {
		c = *p;
		v = base;
		if ('0'<=c && c<='9')
			v = c - '0';
		else if ('a'<=c && c<='z')
			v = c - 'a' + 10;
		else if ('A'<=c && c<='Z')
			v = c - 'A' + 10;
		if (v >= base)
			break;
		v = neg ? -v : v;
		if (git__multiply_int64_overflow(&nn, n, base) || git__add_int64_overflow(&n, nn, v)) {
			ovfl = 1;
			/* Keep on iterating until the end of this number */
			continue;
		}
	}

Return:
	if (ndig == 0) {
		git_error_set(GIT_ERROR_INVALID, "failed to convert string to long: not a number");
		return -1;
	}

	if (endptr)
		*endptr = p;

	if (ovfl) {
		git_error_set(GIT_ERROR_INVALID, "failed to convert string to long: overflow error");
		return -1;
	}

	*result = n;
	return 0;
}

int git__strntol32(int32_t *result, const char *nptr, size_t nptr_len, const char **endptr, int base)
{
	const char *tmp_endptr;
	int32_t tmp_int;
	int64_t tmp_long;
	int error;

	if ((error = git__strntol64(&tmp_long, nptr, nptr_len, &tmp_endptr, base)) < 0)
		return error;

	tmp_int = tmp_long & 0xFFFFFFFF;
	if (tmp_int != tmp_long) {
		int len = (int)(tmp_endptr - nptr);
		git_error_set(GIT_ERROR_INVALID, "failed to convert: '%.*s' is too large", len, nptr);
		return -1;
	}

	*result = tmp_int;
	if (endptr)
		*endptr = tmp_endptr;

	return error;
}

int git__strcasecmp(const char *a, const char *b)
{
	while (*a && *b && git__tolower(*a) == git__tolower(*b))
		++a, ++b;
	return ((unsigned char)git__tolower(*a) - (unsigned char)git__tolower(*b));
}

int git__strcasesort_cmp(const char *a, const char *b)
{
	int cmp = 0;

	while (*a && *b) {
		if (*a != *b) {
			if (git__tolower(*a) != git__tolower(*b))
				break;
			/* use case in sort order even if not in equivalence */
			if (!cmp)
				cmp = (int)(*(const uint8_t *)a) - (int)(*(const uint8_t *)b);
		}

		++a, ++b;
	}

	if (*a || *b)
		return (unsigned char)git__tolower(*a) - (unsigned char)git__tolower(*b);

	return cmp;
}

int git__strncasecmp(const char *a, const char *b, size_t sz)
{
	int al, bl;

	do {
		al = (unsigned char)git__tolower(*a);
		bl = (unsigned char)git__tolower(*b);
		++a, ++b;
	} while (--sz && al && al == bl);

	return al - bl;
}

void git__strntolower(char *str, size_t len)
{
	size_t i;

	for (i = 0; i < len; ++i) {
		str[i] = (char)git__tolower(str[i]);
	}
}

void git__strtolower(char *str)
{
	git__strntolower(str, strlen(str));
}

GIT_INLINE(int) prefixcmp(const char *str, size_t str_n, const char *prefix, bool icase)
{
	int s, p;

	while (str_n--) {
		s = (unsigned char)*str++;
		p = (unsigned char)*prefix++;

		if (icase) {
			s = git__tolower(s);
			p = git__tolower(p);
		}

		if (!p)
			return 0;

		if (s != p)
			return s - p;
	}

	return (0 - *prefix);
}

int git__prefixcmp(const char *str, const char *prefix)
{
	unsigned char s, p;

	while (1) {
		p = *prefix++;
		s = *str++;

		if (!p)
			return 0;

		if (s != p)
			return s - p;
	}
}

int git__prefixncmp(const char *str, size_t str_n, const char *prefix)
{
	return prefixcmp(str, str_n, prefix, false);
}

int git__prefixcmp_icase(const char *str, const char *prefix)
{
	return prefixcmp(str, SIZE_MAX, prefix, true);
}

int git__prefixncmp_icase(const char *str, size_t str_n, const char *prefix)
{
	return prefixcmp(str, str_n, prefix, true);
}

int git__suffixcmp(const char *str, const char *suffix)
{
	size_t a = strlen(str);
	size_t b = strlen(suffix);
	if (a < b)
		return -1;
	return strcmp(str + (a - b), suffix);
}

char *git__strtok(char **end, const char *sep)
{
	char *ptr = *end;

	while (*ptr && strchr(sep, *ptr))
		++ptr;

	if (*ptr) {
		char *start = ptr;
		*end = start + 1;

		while (**end && !strchr(sep, **end))
			++*end;

		if (**end) {
			**end = '\0';
			++*end;
		}

		return start;
	}

	return NULL;
}

/* Similar to strtok, but does not collapse repeated tokens. */
char *git__strsep(char **end, const char *sep)
{
	char *start = *end, *ptr = *end;

	while (*ptr && !strchr(sep, *ptr))
		++ptr;

	if (*ptr) {
		*end = ptr + 1;
		*ptr = '\0';

		return start;
	}

	return NULL;
}

size_t git__linenlen(const char *buffer, size_t buffer_len)
{
	char *nl = memchr(buffer, '\n', buffer_len);
	return nl ? (size_t)(nl - buffer) + 1 : buffer_len;
}

/*
 * Adapted Not So Naive algorithm from http://www-igm.univ-mlv.fr/~lecroq/string/
 */
const void * git__memmem(const void *haystack, size_t haystacklen,
			 const void *needle, size_t needlelen)
{
	const char *h, *n;
	size_t j, k, l;

	if (needlelen > haystacklen || !haystacklen || !needlelen)
		return NULL;

	h = (const char *) haystack,
	n = (const char *) needle;

	if (needlelen == 1)
		return memchr(haystack, *n, haystacklen);

	if (n[0] == n[1]) {
		k = 2;
		l = 1;
	} else {
		k = 1;
		l = 2;
	}

	j = 0;
	while (j <= haystacklen - needlelen) {
		if (n[1] != h[j + 1]) {
			j += k;
		} else {
			if (memcmp(n + 2, h + j + 2, needlelen - 2) == 0 &&
			    n[0] == h[j])
				return h + j;
			j += l;
		}
	}

	return NULL;
}

void git__hexdump(const char *buffer, size_t len)
{
	static const size_t LINE_WIDTH = 16;

	size_t line_count, last_line, i, j;
	const char *line;

	line_count = (len / LINE_WIDTH);
	last_line = (len % LINE_WIDTH);

	for (i = 0; i < line_count; ++i) {
		printf("%08" PRIxZ "  ", (i * LINE_WIDTH));

		line = buffer + (i * LINE_WIDTH);
		for (j = 0; j < LINE_WIDTH; ++j, ++line) {
			printf("%02x ", (unsigned char)*line & 0xFF);

			if (j == (LINE_WIDTH / 2))
				printf(" ");
		}

		printf(" |");

		line = buffer + (i * LINE_WIDTH);
		for (j = 0; j < LINE_WIDTH; ++j, ++line)
			printf("%c", (*line >= 32 && *line <= 126) ? *line : '.');

		printf("|\n");
	}

	if (last_line > 0) {
		printf("%08" PRIxZ "  ", (line_count * LINE_WIDTH));

		line = buffer + (line_count * LINE_WIDTH);
		for (j = 0; j < last_line; ++j, ++line) {
			printf("%02x ", (unsigned char)*line & 0xFF);

			if (j == (LINE_WIDTH / 2))
				printf(" ");
		}

		if (j < (LINE_WIDTH / 2))
			printf(" ");
		for (j = 0; j < (LINE_WIDTH - last_line); ++j)
			printf("   ");

		printf(" |");

		line = buffer + (line_count * LINE_WIDTH);
		for (j = 0; j < last_line; ++j, ++line)
			printf("%c", (*line >= 32 && *line <= 126) ? *line : '.');

		printf("|\n");
	}

	printf("\n");
}

#ifdef GIT_LEGACY_HASH
uint32_t git__hash(const void *key, int len, unsigned int seed)
{
	const uint32_t m = 0x5bd1e995;
	const int r = 24;
	uint32_t h = seed ^ len;

	const unsigned char *data = (const unsigned char *)key;

	while(len >= 4) {
		uint32_t k = *(uint32_t *)data;

		k *= m;
		k ^= k >> r;
		k *= m;

		h *= m;
		h ^= k;

		data += 4;
		len -= 4;
	}

	switch(len) {
	case 3: h ^= data[2] << 16;
	case 2: h ^= data[1] << 8;
	case 1: h ^= data[0];
			h *= m;
	};

	h ^= h >> 13;
	h *= m;
	h ^= h >> 15;

	return h;
}
#else
/*
	Cross-platform version of Murmurhash3
	http://code.google.com/p/smhasher/wiki/MurmurHash3
	by Austin Appleby (aappleby@gmail.com)

	This code is on the public domain.
*/
uint32_t git__hash(const void *key, int len, uint32_t seed)
{

#define MURMUR_BLOCK() {\
	k1 *= c1; \
	k1 = git__rotl(k1,11);\
	k1 *= c2;\
	h1 ^= k1;\
	h1 = h1*3 + 0x52dce729;\
	c1 = c1*5 + 0x7b7d159c;\
	c2 = c2*5 + 0x6bce6396;\
}

	const uint8_t *data = (const uint8_t*)key;
	const int nblocks = len / 4;

	const uint32_t *blocks = (const uint32_t *)(data + nblocks * 4);
	const uint8_t *tail = (const uint8_t *)(data + nblocks * 4);

	uint32_t h1 = 0x971e137b ^ seed;
	uint32_t k1;

	uint32_t c1 = 0x95543787;
	uint32_t c2 = 0x2ad7eb25;

	int i;

	for (i = -nblocks; i; i++) {
		k1 = blocks[i];
		MURMUR_BLOCK();
	}

	k1 = 0;

	switch(len & 3) {
	case 3: k1 ^= tail[2] << 16;
		/* fall through */
	case 2: k1 ^= tail[1] << 8;
		/* fall through */
	case 1: k1 ^= tail[0];
		MURMUR_BLOCK();
	}

	h1 ^= len;
	h1 ^= h1 >> 16;
	h1 *= 0x85ebca6b;
	h1 ^= h1 >> 13;
	h1 *= 0xc2b2ae35;
	h1 ^= h1 >> 16;

	return h1;
}
#endif

/**
 * A modified `bsearch` from the BSD glibc.
 *
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 3. [rescinded 22 July 1999]
 * 4. Neither the name of the University nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
int git__bsearch(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare)(const void *, const void *),
	size_t *position)
{
	size_t lim;
	int cmp = -1;
	void **part, **base = array;

	for (lim = array_len; lim != 0; lim >>= 1) {
		part = base + (lim >> 1);
		cmp = (*compare)(key, *part);
		if (cmp == 0) {
			base = part;
			break;
		}
		if (cmp > 0) { /* key > p; take right partition */
			base = part + 1;
			lim--;
		} /* else take left partition */
	}

	if (position)
		*position = (base - array);

	return (cmp == 0) ? 0 : GIT_ENOTFOUND;
}

int git__bsearch_r(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare_r)(const void *, const void *, void *),
	void *payload,
	size_t *position)
{
	size_t lim;
	int cmp = -1;
	void **part, **base = array;

	for (lim = array_len; lim != 0; lim >>= 1) {
		part = base + (lim >> 1);
		cmp = (*compare_r)(key, *part, payload);
		if (cmp == 0) {
			base = part;
			break;
		}
		if (cmp > 0) { /* key > p; take right partition */
			base = part + 1;
			lim--;
		} /* else take left partition */
	}

	if (position)
		*position = (base - array);

	return (cmp == 0) ? 0 : GIT_ENOTFOUND;
}

/**
 * A strcmp wrapper
 *
 * We don't want direct pointers to the CRT on Windows, we may
 * get stdcall conflicts.
 */
int git__strcmp_cb(const void *a, const void *b)
{
	return git__strcmp((const char *)a, (const char *)b);
}

int git__strcasecmp_cb(const void *a, const void *b)
{
	return git__strcasecmp((const char *)a, (const char *)b);
}

int git__parse_bool(int *out, const char *value)
{
	/* A missing value means true */
	if (value == NULL ||
		!strcasecmp(value, "true") ||
		!strcasecmp(value, "yes") ||
		!strcasecmp(value, "on")) {
		*out = 1;
		return 0;
	}
	if (!strcasecmp(value, "false") ||
		!strcasecmp(value, "no") ||
		!strcasecmp(value, "off") ||
		value[0] == '\0') {
		*out = 0;
		return 0;
	}

	return -1;
}

size_t git__unescape(char *str)
{
	char *scan, *pos = str;

	if (!str)
		return 0;

	for (scan = str; *scan; pos++, scan++) {
		if (*scan == '\\' && *(scan + 1) != '\0')
			scan++; /* skip '\' but include next char */
		if (pos != scan)
			*pos = *scan;
	}

	if (pos != scan) {
		*pos = '\0';
	}

	return (pos - str);
}

#if defined(GIT_QSORT_MSC) || defined(GIT_QSORT_BSD)
typedef struct {
	git__sort_r_cmp cmp;
	void *payload;
} git__qsort_r_glue;

static int GIT_LIBGIT2_CALL git__qsort_r_glue_cmp(
	void *payload, const void *a, const void *b)
{
	git__qsort_r_glue *glue = payload;
	return glue->cmp(a, b, glue->payload);
}
#endif


#if !defined(GIT_QSORT_BSD) && \
    !defined(GIT_QSORT_GNU) && \
    !defined(GIT_QSORT_C11) && \
    !defined(GIT_QSORT_MSC)

static void swap(uint8_t *a, uint8_t *b, size_t elsize)
{
	char tmp[256];

	while (elsize) {
		size_t n = elsize < sizeof(tmp) ? elsize : sizeof(tmp);
		memcpy(tmp, a + elsize - n, n);
		memcpy(a + elsize - n, b + elsize - n, n);
		memcpy(b + elsize - n, tmp, n);
		elsize -= n;
	}
}

static void insertsort(
	void *els, size_t nel, size_t elsize,
	git__sort_r_cmp cmp, void *payload)
{
	uint8_t *base = els;
	uint8_t *end = base + nel * elsize;
	uint8_t *i, *j;

	for (i = base + elsize; i < end; i += elsize)
		for (j = i; j > base && cmp(j, j - elsize, payload) < 0; j -= elsize)
			swap(j, j - elsize, elsize);
}

#endif

void git__qsort_r(
	void *els, size_t nel, size_t elsize, git__sort_r_cmp cmp, void *payload)
{
#if defined(GIT_QSORT_GNU)
	qsort_r(els, nel, elsize, cmp, payload);
#elif defined(GIT_QSORT_C11)
	qsort_s(els, nel, elsize, cmp, payload);
#elif defined(GIT_QSORT_BSD)
	git__qsort_r_glue glue = { cmp, payload };
	qsort_r(els, nel, elsize, &glue, git__qsort_r_glue_cmp);
#elif defined(GIT_QSORT_MSC)
	git__qsort_r_glue glue = { cmp, payload };
	qsort_s(els, nel, elsize, git__qsort_r_glue_cmp, &glue);
#else
	insertsort(els, nel, elsize, cmp, payload);
#endif
}

#ifdef GIT_WIN32
int git__getenv(git_str *out, const char *name)
{
	wchar_t *wide_name = NULL, *wide_value = NULL;
	DWORD value_len;
	int error = -1;

	git_str_clear(out);

	if (git_utf8_to_16_alloc(&wide_name, name) < 0)
		return -1;

	if ((value_len = GetEnvironmentVariableW(wide_name, NULL, 0)) > 0) {
		wide_value = git__malloc(value_len * sizeof(wchar_t));
		GIT_ERROR_CHECK_ALLOC(wide_value);

		value_len = GetEnvironmentVariableW(wide_name, wide_value, value_len);
	}

	if (value_len)
		error = git_str_put_w(out, wide_value, value_len);
	else if (GetLastError() == ERROR_SUCCESS || GetLastError() == ERROR_ENVVAR_NOT_FOUND)
		error = GIT_ENOTFOUND;
	else
		git_error_set(GIT_ERROR_OS, "could not read environment variable '%s'", name);

	git__free(wide_name);
	git__free(wide_value);
	return error;
}
#else
int git__getenv(git_str *out, const char *name)
{
	const char *val = getenv(name);

	git_str_clear(out);

	if (!val)
		return GIT_ENOTFOUND;

	return git_str_puts(out, val);
}
#endif

/*
 * By doing this in two steps we can at least get
 * the function to be somewhat coherent, even
 * with this disgusting nest of #ifdefs.
 */
#ifndef _SC_NPROCESSORS_ONLN
#	ifdef _SC_NPROC_ONLN
#		define _SC_NPROCESSORS_ONLN _SC_NPROC_ONLN
#	elif defined _SC_CRAY_NCPU
#		define _SC_NPROCESSORS_ONLN _SC_CRAY_NCPU
#	endif
#endif

int git__online_cpus(void)
{
#ifdef _SC_NPROCESSORS_ONLN
	long ncpus;
#endif

#ifdef _WIN32
	SYSTEM_INFO info;
	GetSystemInfo(&info);

	if ((int)info.dwNumberOfProcessors > 0)
		return (int)info.dwNumberOfProcessors;
#elif defined(hpux) || defined(__hpux) || defined(_hpux)
	struct pst_dynamic psd;

	if (!pstat_getdynamic(&psd, sizeof(psd), (size_t)1, 0))
		return (int)psd.psd_proc_cnt;
#endif

#ifdef _SC_NPROCESSORS_ONLN
	if ((ncpus = (long)sysconf(_SC_NPROCESSORS_ONLN)) > 0)
		return (int)ncpus;
#endif

	return 1;
}
