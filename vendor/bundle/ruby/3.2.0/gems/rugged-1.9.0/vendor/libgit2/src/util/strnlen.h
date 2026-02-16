/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_strlen_h__
#define INCLUDE_strlen_h__

#if defined(__MINGW32__) || defined(__sun) || defined(__APPLE__) || defined(__MidnightBSD__) ||\
	(defined(_MSC_VER) && _MSC_VER < 1500)
#   define NO_STRNLEN
#endif

#ifdef NO_STRNLEN
GIT_INLINE(size_t) p_strnlen(const char *s, size_t maxlen) {
	const char *end = memchr(s, 0, maxlen);
	return end ? (size_t)(end - s) : maxlen;
}
#else
#   define p_strnlen strnlen
#endif

#endif
