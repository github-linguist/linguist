/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_cc_compat_h__
#define INCLUDE_cc_compat_h__

#include <stdarg.h>

/*
 * See if our compiler is known to support flexible array members.
 */
#ifndef GIT_FLEX_ARRAY
#	if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#		define GIT_FLEX_ARRAY /* empty */
#	elif defined(__GNUC__)
#		if (__GNUC__ >= 3)
#			define GIT_FLEX_ARRAY /* empty */
#		else
#			define GIT_FLEX_ARRAY 0 /* older GNU extension */
#		endif
#	endif

/* Default to safer but a bit wasteful traditional style */
#	ifndef GIT_FLEX_ARRAY
#		define GIT_FLEX_ARRAY 1
#	endif
#endif

#if defined(__GNUC__)
#	define GIT_ALIGN(x,size) x __attribute__ ((aligned(size)))
#elif defined(_MSC_VER)
#	define GIT_ALIGN(x,size) __declspec(align(size)) x
#else
#	define GIT_ALIGN(x,size) x
#endif

#if defined(__GNUC__)
# define GIT_UNUSED(x)                                                         \
	do {                                                                   \
		__typeof__(x) _unused __attribute__((unused));                 \
		_unused = (x);                                                 \
	} while (0)
# define GIT_UNUSED_ARG __attribute__((unused))
# define GIT_UNUSED_FUNCTION __attribute__((unused))
#else
# define GIT_UNUSED(x) ((void)(x))
# define GIT_UNUSED_ARG
# define GIT_UNUSED_FUNCTION
#endif

/* Define the printf format specifier to use for size_t output */
#if defined(_MSC_VER) || defined(__MINGW32__)

/* Visual Studio 2012 and prior lack PRId64 entirely */
#	ifndef PRId64
#		define PRId64 "I64d"
#	endif

/* The first block is needed to avoid warnings on MingW amd64 */
#	if (SIZE_MAX == ULLONG_MAX)
#		define PRIuZ "I64u"
#		define PRIxZ "I64x"
#		define PRIXZ "I64X"
#		define PRIdZ "I64d"
#	else
#		define PRIuZ "Iu"
#		define PRIxZ "Ix"
#		define PRIXZ "IX"
#		define PRIdZ "Id"
#	endif

#else
#	define PRIuZ "zu"
#	define PRIxZ "zx"
#	define PRIXZ "zX"
#	define PRIdZ "zd"
#endif

/* Microsoft Visual C/C++ */
#if defined(_MSC_VER)
/* disable "deprecated function" warnings */
#	pragma warning ( disable : 4996 )
/* disable "conditional expression is constant" level 4 warnings */
#	pragma warning ( disable : 4127 )
#endif

#if defined (_MSC_VER)
	typedef unsigned char bool;
#	ifndef true
#		define true 1
#	endif
#	ifndef false
#		define false 0
#	endif
#else
#	include <stdbool.h>
#endif

#ifndef va_copy
#	ifdef __va_copy
#		define va_copy(dst, src) __va_copy(dst, src)
#	else
#		define va_copy(dst, src) ((dst) = (src))
#	endif
#endif

#endif
