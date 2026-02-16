/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_util_h__
#define INCLUDE_util_h__

#include "str.h"
#include "git2_util.h"
#include "strnlen.h"
#include "thread.h"

#ifndef GIT_WIN32
# include <ctype.h>
#endif

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))
#define bitsizeof(x) (CHAR_BIT * sizeof(x))
#define MSB(x, bits) ((x) & (~UINT64_C(0) << (bitsizeof(x) - (bits))))
#ifndef min
# define min(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef max
# define max(a,b) ((a) > (b) ? (a) : (b))
#endif

#if defined(__GNUC__)
# define GIT_CONTAINER_OF(ptr, type, member) \
	__builtin_choose_expr( \
	    __builtin_offsetof(type, member) == 0 && \
	    __builtin_types_compatible_p(__typeof__(&((type *) 0)->member), __typeof__(ptr)), \
		((type *) (ptr)), \
		(void)0)
#else
# define GIT_CONTAINER_OF(ptr, type, member) (type *)(ptr)
#endif

/**
 * Return the length of a constant string.
 * We are aware that `strlen` performs the same task and is usually
 * optimized away by the compiler, whilst being safer because it returns
 * valid values when passed a pointer instead of a constant string; however
 * this macro will transparently work with wide-char and single-char strings.
 */
#define CONST_STRLEN(x) ((sizeof(x)/sizeof(x[0])) - 1)

#define STRCMP_CASESELECT(IGNORE_CASE, STR1, STR2) \
	((IGNORE_CASE) ? strcasecmp((STR1), (STR2)) : strcmp((STR1), (STR2)))

#define CASESELECT(IGNORE_CASE, ICASE, CASE) \
	((IGNORE_CASE) ? (ICASE) : (CASE))

extern int git__prefixcmp(const char *str, const char *prefix);
extern int git__prefixcmp_icase(const char *str, const char *prefix);
extern int git__prefixncmp(const char *str, size_t str_n, const char *prefix);
extern int git__prefixncmp_icase(const char *str, size_t str_n, const char *prefix);
extern int git__suffixcmp(const char *str, const char *suffix);

GIT_INLINE(int) git__signum(int val)
{
	return ((val > 0) - (val < 0));
}

extern int git__strntol32(int32_t *n, const char *buff, size_t buff_len, const char **end_buf, int base);
extern int git__strntol64(int64_t *n, const char *buff, size_t buff_len, const char **end_buf, int base);


extern void git__hexdump(const char *buffer, size_t n);
extern uint32_t git__hash(const void *key, int len, uint32_t seed);

/* 32-bit cross-platform rotl */
#ifdef _MSC_VER /* use built-in method in MSVC */
#	define git__rotl(v, s) (uint32_t)_rotl(v, s)
#else /* use bitops in GCC; with o2 this gets optimized to a rotl instruction */
#	define git__rotl(v, s) (uint32_t)(((uint32_t)(v) << (s)) | ((uint32_t)(v) >> (32 - (s))))
#endif

extern char *git__strtok(char **end, const char *sep);
extern char *git__strsep(char **end, const char *sep);

extern void git__strntolower(char *str, size_t len);
extern void git__strtolower(char *str);

extern size_t git__linenlen(const char *buffer, size_t buffer_len);

GIT_INLINE(const char *) git__next_line(const char *s)
{
	while (*s && *s != '\n') s++;
	while (*s == '\n' || *s == '\r') s++;
	return s;
}

GIT_INLINE(const void *) git__memrchr(const void *s, int c, size_t n)
{
	const unsigned char *cp;

	if (n != 0) {
		cp = (unsigned char *)s + n;
		do {
			if (*(--cp) == (unsigned char)c)
				return cp;
		} while (--n != 0);
	}

	return NULL;
}

extern const void * git__memmem(const void *haystack, size_t haystacklen,
				const void *needle, size_t needlelen);

typedef int (*git__tsort_cmp)(const void *a, const void *b);

extern void git__tsort(void **dst, size_t size, git__tsort_cmp cmp);

typedef int (*git__sort_r_cmp)(const void *a, const void *b, void *payload);

extern void git__tsort_r(
	void **dst, size_t size, git__sort_r_cmp cmp, void *payload);

extern void git__qsort_r(
	void *els, size_t nel, size_t elsize, git__sort_r_cmp cmp, void *payload);

/**
 * @param position If non-NULL, this will be set to the position where the
 * 		element is or would be inserted if not found.
 * @return 0 if found; GIT_ENOTFOUND if not found
 */
extern int git__bsearch(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare)(const void *key, const void *element),
	size_t *position);

extern int git__bsearch_r(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare_r)(const void *key, const void *element, void *payload),
	void *payload,
	size_t *position);

#define git__strcmp strcmp
#define git__strncmp strncmp

extern int git__strcmp_cb(const void *a, const void *b);
extern int git__strcasecmp_cb(const void *a, const void *b);

extern int git__strcasecmp(const char *a, const char *b);
extern int git__strncasecmp(const char *a, const char *b, size_t sz);

extern int git__strcasesort_cmp(const char *a, const char *b);

/*
 * Compare some NUL-terminated `a` to a possibly non-NUL terminated
 * `b` of length `b_len`; like `strncmp` but ensuring that
 * `strlen(a) == b_len` as well.
 */
GIT_INLINE(int) git__strlcmp(const char *a, const char *b, size_t b_len)
{
	int cmp = strncmp(a, b, b_len);
	return cmp ? cmp : (int)a[b_len];
}

typedef struct {
	git_atomic32 refcount;
	void *owner;
} git_refcount;

typedef void (*git_refcount_freeptr)(void *r);

#define GIT_REFCOUNT_INC(r) { \
	git_atomic32_inc(&(r)->rc.refcount);	\
}

#define GIT_REFCOUNT_DEC(_r, do_free) { \
	git_refcount *r = &(_r)->rc; \
	int val = git_atomic32_dec(&r->refcount); \
	if (val <= 0 && r->owner == NULL) { do_free(_r); } \
}

#define GIT_REFCOUNT_OWN(r, o) { \
	(void)git_atomic_swap((r)->rc.owner, o); \
}

#define GIT_REFCOUNT_OWNER(r) git_atomic_load((r)->rc.owner)

#define GIT_REFCOUNT_VAL(r) git_atomic32_get((r)->rc.refcount)


static signed char from_hex[] = {
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 00 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 10 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 20 */
 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1, /* 30 */
-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 40 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 50 */
-1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 60 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 70 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 80 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* 90 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* a0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* b0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* c0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* d0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* e0 */
-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, /* f0 */
};

GIT_INLINE(int) git__fromhex(char h)
{
	return from_hex[(unsigned char) h];
}

GIT_INLINE(int) git__ishex(const char *str)
{
	unsigned i;
	for (i=0; str[i] != '\0'; i++)
		if (git__fromhex(str[i]) < 0)
			return 0;
	return 1;
}

GIT_INLINE(size_t) git__size_t_bitmask(size_t v)
{
	v--;
	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;
	v |= v >> 8;
	v |= v >> 16;

	return v;
}

GIT_INLINE(size_t) git__size_t_powerof2(size_t v)
{
	return git__size_t_bitmask(v) + 1;
}

GIT_INLINE(bool) git__isspace_nonlf(int c)
{
	return (c == ' ' || c == '\t' || c == '\f' || c == '\r' || c == '\v');
}

GIT_INLINE(bool) git__iswildcard(int c)
{
	return (c == '*' || c == '?' || c == '[');
}

/*
 * Parse a string value as a boolean, just like Core Git does.
 *
 * Valid values for true are: 'true', 'yes', 'on'
 * Valid values for false are: 'false', 'no', 'off'
 */
extern int git__parse_bool(int *out, const char *value);

/*
 * Unescapes a string in-place.
 *
 * Edge cases behavior:
 * - "jackie\" -> "jacky\"
 * - "chan\\" -> "chan\"
 */
extern size_t git__unescape(char *str);

/*
 * Safely zero-out memory, making sure that the compiler
 * doesn't optimize away the operation.
 */
GIT_INLINE(void) git__memzero(void *data, size_t size)
{
#ifdef _MSC_VER
	SecureZeroMemory((PVOID)data, size);
#else
	volatile uint8_t *scan = (volatile uint8_t *)data;

	while (size--)
		*scan++ = 0x0;
#endif
}

#ifdef GIT_WIN32

GIT_INLINE(uint64_t) git_time_monotonic(void)
{
	/* GetTickCount64 returns the number of milliseconds that have
	 * elapsed since the system was started. */
	return GetTickCount64();
}

#elif __APPLE__

#include <mach/mach_time.h>
#include <sys/time.h>

GIT_INLINE(uint64_t) git_time_monotonic(void)
{
	static double scaling_factor = 0;

	if (scaling_factor == 0) {
		mach_timebase_info_data_t info;

		scaling_factor = mach_timebase_info(&info) == KERN_SUCCESS ?
			((double)info.numer / (double)info.denom) / 1.0E6 :
			-1;
	} else if (scaling_factor < 0) {
		struct timeval tv;

		/* mach_timebase_info failed; fall back to gettimeofday */
		gettimeofday(&tv, NULL);
		return (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
	}

	return (uint64_t)(mach_absolute_time() * scaling_factor);
}

#elif defined(__amigaos4__)

#include <proto/timer.h>

GIT_INLINE(uint64_t) git_time_monotonic(void)
{
	struct TimeVal tv;
	ITimer->GetUpTime(&tv);
	return (tv.Seconds * 1000) + (tv.Microseconds / 1000);
}

#else

#include <sys/time.h>

GIT_INLINE(uint64_t) git_time_monotonic(void)
{
	struct timeval tv;

#ifdef CLOCK_MONOTONIC
	struct timespec tp;
	if (clock_gettime(CLOCK_MONOTONIC, &tp) == 0)
		return (tp.tv_sec * 1000) + (tp.tv_nsec / 1.0E6);
#endif

	/* Fall back to using gettimeofday */
	gettimeofday(&tv, NULL);
	return (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
}

#endif

extern int git__getenv(git_str *out, const char *name);

extern int git__online_cpus(void);

GIT_INLINE(int) git__noop(void) { return 0; }
GIT_INLINE(int) git__noop_args(void *a, ...) { GIT_UNUSED(a); return 0; }

#include "alloc.h"

#endif
