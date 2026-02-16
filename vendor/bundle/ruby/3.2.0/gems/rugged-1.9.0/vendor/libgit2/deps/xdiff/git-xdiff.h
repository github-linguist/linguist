/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

/*
 * This file provides the necessary indirection between xdiff and
 * libgit2.  libgit2-specific functionality should live here, so
 * that git and libgit2 can share a common xdiff implementation.
 */

#ifndef INCLUDE_git_xdiff_h__
#define INCLUDE_git_xdiff_h__

#include "regexp.h"

/* Work around C90-conformance issues */
#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 199901L)
# if defined(_MSC_VER)
#  define inline __inline
# elif defined(__GNUC__)
#  define inline __inline__
# else
#  define inline
# endif
#endif

#define XDL_UNUSED GIT_UNUSED_ARG

#define xdl_malloc(x) git__malloc(x)
#define xdl_calloc(n, sz) git__calloc(n, sz)
#define xdl_free(ptr) git__free(ptr)
#define xdl_realloc(ptr, x) git__realloc(ptr, x)

#define XDL_BUG(msg) GIT_ASSERT(!msg)

#define xdl_regex_t git_regexp
#define xdl_regmatch_t git_regmatch

GIT_INLINE(int) xdl_regexec_buf(
	const xdl_regex_t *preg, const char *buf, size_t size,
	size_t nmatch, xdl_regmatch_t pmatch[], int eflags)
{
	GIT_UNUSED(preg);
	GIT_UNUSED(buf);
	GIT_UNUSED(size);
	GIT_UNUSED(nmatch);
	GIT_UNUSED(pmatch);
	GIT_UNUSED(eflags);
	GIT_ASSERT("not implemented");
	return -1;
}

#endif
