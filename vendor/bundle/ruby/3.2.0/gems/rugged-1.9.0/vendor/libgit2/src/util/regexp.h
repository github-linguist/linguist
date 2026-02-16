/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_regexp_h__
#define INCLUDE_regexp_h__

#include "git2_util.h"

#if defined(GIT_REGEX_BUILTIN) || defined(GIT_REGEX_PCRE)
# include "pcre.h"
typedef pcre *git_regexp;
# define GIT_REGEX_INIT NULL
#elif defined(GIT_REGEX_PCRE2)
# define PCRE2_CODE_UNIT_WIDTH 8
# include <pcre2.h>
typedef pcre2_code *git_regexp;
# define GIT_REGEX_INIT NULL
#elif defined(GIT_REGEX_REGCOMP) || defined(GIT_REGEX_REGCOMP_L)
# include <regex.h>
typedef regex_t git_regexp;
# define GIT_REGEX_INIT { 0 }
#else
# error "No regex backend"
#endif

/** Options supported by @git_regexp_compile. */
typedef enum {
	/** Enable case-insensitive matching */
	GIT_REGEXP_ICASE = (1 << 0)
} git_regexp_flags_t;

/** Structure containing information about regular expression matching groups */
typedef struct {
	/** Start of the given match. -1 if the group didn't match anything */
	ssize_t start;
	/** End of the given match. -1 if the group didn't match anything */
	ssize_t end;
} git_regmatch;

/**
 * Compile a regular expression. The compiled expression needs to
 * be cleaned up afterwards with `git_regexp_dispose`.
 *
 * @param r Pointer to the storage where to initialize the regular expression.
 * @param pattern The pattern that shall be compiled.
 * @param flags Flags to alter how the pattern shall be handled.
 *              0 for defaults, otherwise see @git_regexp_flags_t.
 * @return 0 on success, otherwise a negative return value.
 */
int git_regexp_compile(git_regexp *r, const char *pattern, int flags);

/**
 * Free memory associated with the regular expression
 *
 * @param r The regular expression structure to dispose.
 */
void git_regexp_dispose(git_regexp *r);

/**
 * Test whether a given string matches a compiled regular
 * expression.
 *
 * @param r Compiled regular expression.
 * @param string String to match against the regular expression.
 * @return 0 if the string matches, a negative error code
 *         otherwise. GIT_ENOTFOUND if no match was found,
 *         GIT_EINVALIDSPEC if the regular expression matching
 *         was invalid.
 */
int git_regexp_match(const git_regexp *r, const char *string);

/**
 * Search for matches inside of a given string.
 *
 * Given a regular expression with capturing groups, this
 * function will populate provided @git_regmatch structures with
 * offsets for each of the given matches. Non-matching groups
 * will have start and end values of the respective @git_regmatch
 * structure set to -1.
 *
 * @param r Compiled regular expression.
 * @param string String to match against the regular expression.
 * @param nmatches Number of @git_regmatch structures provided by
 *                 the user.
 * @param matches Pointer to an array of @git_regmatch structures.
 * @return 0 if the string matches, a negative error code
 *         otherwise. GIT_ENOTFOUND if no match was found,
 *         GIT_EINVALIDSPEC if the regular expression matching
 *         was invalid.
 */
int git_regexp_search(const git_regexp *r, const char *string, size_t nmatches, git_regmatch *matches);

#endif
