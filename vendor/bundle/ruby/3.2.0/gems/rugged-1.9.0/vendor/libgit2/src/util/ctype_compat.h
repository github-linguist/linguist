/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_ctype_compat_h__
#define INCLUDE_ctype_compat_h__

/*
 * The Microsoft C runtime (MSVCRT) may take a heavy lock on the
 * locale in order to figure out how the `ctype` functions work.
 * This is deeply slow. Provide our own to avoid that.
 */

#ifdef GIT_WIN32

GIT_INLINE(int) git__tolower(int c)
{
	return (c >= 'A' && c <= 'Z') ? (c + 32) : c;
}

GIT_INLINE(int) git__toupper(int c)
{
	return (c >= 'a' && c <= 'z') ? (c - 32) : c;
}

GIT_INLINE(bool) git__isalpha(int c)
{
	return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
}

GIT_INLINE(bool) git__isdigit(int c)
{
	return (c >= '0' && c <= '9');
}

GIT_INLINE(bool) git__isalnum(int c)
{
	return git__isalpha(c) || git__isdigit(c);
}

GIT_INLINE(bool) git__isspace(int c)
{
	return (c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r' || c == '\v');
}

GIT_INLINE(bool) git__isxdigit(int c)
{
	return ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'));
}

GIT_INLINE(bool) git__isprint(int c)
{
	return (c >= ' ' && c <= '~');
}

#else
# define git__tolower(a) tolower((unsigned char)(a))
# define git__toupper(a) toupper((unsigned char)(a))

# define git__isalpha(a)  (!!isalpha((unsigned char)(a)))
# define git__isdigit(a)  (!!isdigit((unsigned char)(a)))
# define git__isalnum(a)  (!!isalnum((unsigned char)(a)))
# define git__isspace(a)  (!!isspace((unsigned char)(a)))
# define git__isxdigit(a) (!!isxdigit((unsigned char)(a)))
# define git__isprint(a)  (!!isprint((unsigned char)(a)))
#endif

#endif
