/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "regexp.h"

#if defined(GIT_REGEX_BUILTIN) || defined(GIT_REGEX_PCRE)

int git_regexp_compile(git_regexp *r, const char *pattern, int flags)
{
	int erroffset, cflags = 0;
	const char *error = NULL;

	if (flags & GIT_REGEXP_ICASE)
		cflags |= PCRE_CASELESS;

	if ((*r = pcre_compile(pattern, cflags, &error, &erroffset, NULL)) == NULL) {
		git_error_set_str(GIT_ERROR_REGEX, error);
		return GIT_EINVALIDSPEC;
	}

	return 0;
}

void git_regexp_dispose(git_regexp *r)
{
	pcre_free(*r);
	*r = NULL;
}

int git_regexp_match(const git_regexp *r, const char *string)
{
	int error;
	if ((error = pcre_exec(*r, NULL, string, (int) strlen(string), 0, 0, NULL, 0)) < 0)
		return (error == PCRE_ERROR_NOMATCH) ? GIT_ENOTFOUND : GIT_EINVALIDSPEC;
	return 0;
}

int git_regexp_search(const git_regexp *r, const char *string, size_t nmatches, git_regmatch *matches)
{
	int static_ovec[9] = {0}, *ovec;
	int error;
	size_t i;

	/* The ovec array always needs to be a multiple of three */
	if (nmatches <= ARRAY_SIZE(static_ovec) / 3)
		ovec = static_ovec;
	else
		ovec = git__calloc(nmatches * 3, sizeof(*ovec));
	GIT_ERROR_CHECK_ALLOC(ovec);

	if ((error = pcre_exec(*r, NULL, string, (int) strlen(string), 0, 0, ovec, (int) nmatches * 3)) < 0)
		goto out;

	if (error == 0)
		error = (int) nmatches;

	for (i = 0; i < (unsigned int) error; i++) {
		matches[i].start = (ovec[i * 2] < 0) ? -1 : ovec[i * 2];
		matches[i].end = (ovec[i * 2 + 1] < 0) ? -1 : ovec[i * 2 + 1];
	}
	for (i = (unsigned int) error; i < nmatches; i++)
		matches[i].start = matches[i].end = -1;

out:
	if (nmatches > ARRAY_SIZE(static_ovec) / 3)
		git__free(ovec);
	if (error < 0)
		return (error == PCRE_ERROR_NOMATCH) ? GIT_ENOTFOUND : GIT_EINVALIDSPEC;
	return 0;
}

#elif defined(GIT_REGEX_PCRE2)

int git_regexp_compile(git_regexp *r, const char *pattern, int flags)
{
	unsigned char errmsg[1024];
	PCRE2_SIZE erroff;
	int error, cflags = 0;

	if (flags & GIT_REGEXP_ICASE)
		cflags |= PCRE2_CASELESS;

	if ((*r = pcre2_compile((const unsigned char *) pattern, PCRE2_ZERO_TERMINATED,
				cflags, &error, &erroff, NULL)) == NULL) {
		pcre2_get_error_message(error, errmsg, sizeof(errmsg));
		git_error_set_str(GIT_ERROR_REGEX, (char *) errmsg);
		return GIT_EINVALIDSPEC;
	}

	return 0;
}

void git_regexp_dispose(git_regexp *r)
{
	pcre2_code_free(*r);
	*r = NULL;
}

int git_regexp_match(const git_regexp *r, const char *string)
{
	pcre2_match_data *data;
	int error;

	data = pcre2_match_data_create(1, NULL);
	GIT_ERROR_CHECK_ALLOC(data);

	error = pcre2_match(*r, (const unsigned char *) string, strlen(string), 0, 0, data, NULL);
	pcre2_match_data_free(data);
	if (error < 0)
		return (error == PCRE2_ERROR_NOMATCH) ? GIT_ENOTFOUND : GIT_EINVALIDSPEC;

	return 0;
}

int git_regexp_search(const git_regexp *r, const char *string, size_t nmatches, git_regmatch *matches)
{
	pcre2_match_data *data = NULL;
	PCRE2_SIZE *ovec;
	int error;
	size_t i;

	if ((data = pcre2_match_data_create(nmatches, NULL)) == NULL) {
		git_error_set_oom();
		return -1;
	}

	if ((error = pcre2_match(*r, (const unsigned char *) string, strlen(string),
			     0, 0, data, NULL)) < 0)
		goto out;

	if (error == 0 || (unsigned int) error  > nmatches)
		error = nmatches;
	ovec = pcre2_get_ovector_pointer(data);

	for (i = 0; i < (unsigned int) error; i++) {
		matches[i].start = (ovec[i * 2] == PCRE2_UNSET) ? -1 : (ssize_t) ovec[i * 2];
		matches[i].end = (ovec[i * 2 + 1] == PCRE2_UNSET) ? -1 :  (ssize_t) ovec[i * 2 + 1];
	}
	for (i = (unsigned int) error; i < nmatches; i++)
		matches[i].start = matches[i].end = -1;

out:
	pcre2_match_data_free(data);
	if (error < 0)
		return (error == PCRE2_ERROR_NOMATCH) ? GIT_ENOTFOUND : GIT_EINVALIDSPEC;
	return 0;
}

#elif defined(GIT_REGEX_REGCOMP) || defined(GIT_REGEX_REGCOMP_L)

#if defined(GIT_REGEX_REGCOMP_L)
# include <xlocale.h>
#endif

int git_regexp_compile(git_regexp *r, const char *pattern, int flags)
{
	int cflags = REG_EXTENDED, error;
	char errmsg[1024];

	if (flags & GIT_REGEXP_ICASE)
		cflags |= REG_ICASE;

# if defined(GIT_REGEX_REGCOMP)
	if ((error = regcomp(r, pattern, cflags)) != 0)
# else
	if ((error = regcomp_l(r, pattern, cflags, (locale_t) 0)) != 0)
# endif
	{
		regerror(error, r, errmsg, sizeof(errmsg));
		git_error_set_str(GIT_ERROR_REGEX, errmsg);
		return GIT_EINVALIDSPEC;
	}

	return 0;
}

void git_regexp_dispose(git_regexp *r)
{
	regfree(r);
}

int git_regexp_match(const git_regexp *r, const char *string)
{
	int error;
	if ((error = regexec(r, string, 0, NULL, 0)) != 0)
		return (error == REG_NOMATCH) ? GIT_ENOTFOUND : GIT_EINVALIDSPEC;
	return 0;
}

int git_regexp_search(const git_regexp *r, const char *string, size_t nmatches, git_regmatch *matches)
{
	regmatch_t static_m[3], *m;
	int error;
	size_t i;

	if (nmatches <= ARRAY_SIZE(static_m))
		m = static_m;
	else
		m = git__calloc(nmatches, sizeof(*m));

	if ((error = regexec(r, string, nmatches, m, 0)) != 0)
		goto out;

	for (i = 0; i < nmatches; i++) {
		matches[i].start = (m[i].rm_so < 0) ? -1 : m[i].rm_so;
		matches[i].end = (m[i].rm_eo < 0) ? -1 : m[i].rm_eo;
	}

out:
	if (nmatches > ARRAY_SIZE(static_m))
		git__free(m);
	if (error)
		return (error == REG_NOMATCH) ? GIT_ENOTFOUND : GIT_EINVALIDSPEC;
	return 0;
}

#endif
