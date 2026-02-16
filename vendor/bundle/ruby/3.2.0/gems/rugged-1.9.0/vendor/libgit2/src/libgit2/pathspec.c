/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "pathspec.h"

#include "git2/pathspec.h"
#include "git2/diff.h"
#include "attr_file.h"
#include "iterator.h"
#include "repository.h"
#include "index.h"
#include "bitvec.h"
#include "diff.h"
#include "wildmatch.h"

/* what is the common non-wildcard prefix for all items in the pathspec */
char *git_pathspec_prefix(const git_strarray *pathspec)
{
	git_str prefix = GIT_STR_INIT;
	const char *scan;

	if (!pathspec || !pathspec->count ||
		git_str_common_prefix(&prefix, pathspec->strings, pathspec->count) < 0)
		return NULL;

	/* diff prefix will only be leading non-wildcards */
	for (scan = prefix.ptr; *scan; ++scan) {
		if (git__iswildcard(*scan) &&
			(scan == prefix.ptr || (*(scan - 1) != '\\')))
			break;
	}
	git_str_truncate(&prefix, scan - prefix.ptr);

	if (prefix.size <= 0) {
		git_str_dispose(&prefix);
		return NULL;
	}

	git_str_unescape(&prefix);

	return git_str_detach(&prefix);
}

/* is there anything in the spec that needs to be filtered on */
bool git_pathspec_is_empty(const git_strarray *pathspec)
{
	size_t i;

	if (pathspec == NULL)
		return true;

	for (i = 0; i < pathspec->count; ++i) {
		const char *str = pathspec->strings[i];

		if (str && str[0])
			return false;
	}

	return true;
}

/* build a vector of fnmatch patterns to evaluate efficiently */
int git_pathspec__vinit(
	git_vector *vspec, const git_strarray *strspec, git_pool *strpool)
{
	size_t i;

	memset(vspec, 0, sizeof(*vspec));

	if (git_pathspec_is_empty(strspec))
		return 0;

	if (git_vector_init(vspec, strspec->count, NULL) < 0)
		return -1;

	for (i = 0; i < strspec->count; ++i) {
		int ret;
		const char *pattern = strspec->strings[i];
		git_attr_fnmatch *match = git__calloc(1, sizeof(git_attr_fnmatch));
		if (!match)
			return -1;

		match->flags = GIT_ATTR_FNMATCH_ALLOWSPACE | GIT_ATTR_FNMATCH_ALLOWNEG;

		ret = git_attr_fnmatch__parse(match, strpool, NULL, &pattern);
		if (ret == GIT_ENOTFOUND) {
			git__free(match);
			continue;
		} else if (ret < 0) {
			git__free(match);
			return ret;
		}

		if (git_vector_insert(vspec, match) < 0)
			return -1;
	}

	return 0;
}

/* free data from the pathspec vector */
void git_pathspec__vfree(git_vector *vspec)
{
	git_vector_dispose_deep(vspec);
}

struct pathspec_match_context {
	int wildmatch_flags;
	int (*strcomp)(const char *, const char *);
	int (*strncomp)(const char *, const char *, size_t);
};

static void pathspec_match_context_init(
	struct pathspec_match_context *ctxt,
	bool disable_fnmatch,
	bool casefold)
{
	if (disable_fnmatch)
		ctxt->wildmatch_flags = -1;
	else if (casefold)
		ctxt->wildmatch_flags = WM_CASEFOLD;
	else
		ctxt->wildmatch_flags = 0;

	if (casefold) {
		ctxt->strcomp  = git__strcasecmp;
		ctxt->strncomp = git__strncasecmp;
	} else {
		ctxt->strcomp  = git__strcmp;
		ctxt->strncomp = git__strncmp;
	}
}

static int pathspec_match_one(
	const git_attr_fnmatch *match,
	struct pathspec_match_context *ctxt,
	const char *path)
{
	int result = (match->flags & GIT_ATTR_FNMATCH_MATCH_ALL) ? 0 : WM_NOMATCH;

	if (result == WM_NOMATCH)
		result = ctxt->strcomp(match->pattern, path) ? WM_NOMATCH : 0;

	if (ctxt->wildmatch_flags >= 0 && result == WM_NOMATCH)
		result = wildmatch(match->pattern, path, ctxt->wildmatch_flags);

	/* if we didn't match, look for exact dirname prefix match */
	if (result == WM_NOMATCH &&
		(match->flags & GIT_ATTR_FNMATCH_HASWILD) == 0 &&
		ctxt->strncomp(path, match->pattern, match->length) == 0 &&
		path[match->length] == '/')
		result = 0;

	/* if we didn't match and this is a negative match, check for exact
	 * match of filename with leading '!'
	 */
	if (result == WM_NOMATCH &&
		(match->flags & GIT_ATTR_FNMATCH_NEGATIVE) != 0 &&
		*path == '!' &&
		ctxt->strncomp(path + 1, match->pattern, match->length) == 0 &&
		(!path[match->length + 1] || path[match->length + 1] == '/'))
		return 1;

	if (result == 0)
		return (match->flags & GIT_ATTR_FNMATCH_NEGATIVE) ? 0 : 1;
	return -1;
}

static int git_pathspec__match_at(
	size_t *matched_at,
	const git_vector *vspec,
	struct pathspec_match_context *ctxt,
	const char *path0,
	const char *path1)
{
	int result = GIT_ENOTFOUND;
	size_t i = 0;
	const git_attr_fnmatch *match;

	git_vector_foreach(vspec, i, match) {
		if (path0 && (result = pathspec_match_one(match, ctxt, path0)) >= 0)
			break;
		if (path1 && (result = pathspec_match_one(match, ctxt, path1)) >= 0)
			break;
	}

	*matched_at = i;
	return result;
}

/* match a path against the vectorized pathspec */
bool git_pathspec__match(
	const git_vector *vspec,
	const char *path,
	bool disable_fnmatch,
	bool casefold,
	const char **matched_pathspec,
	size_t *matched_at)
{
	int result;
	size_t pos;
	struct pathspec_match_context ctxt;

	if (matched_pathspec)
		*matched_pathspec = NULL;
	if (matched_at)
		*matched_at = GIT_PATHSPEC_NOMATCH;

	if (!vspec || !vspec->length)
		return true;

	pathspec_match_context_init(&ctxt, disable_fnmatch, casefold);

	result = git_pathspec__match_at(&pos, vspec, &ctxt, path, NULL);
	if (result >= 0) {
		if (matched_pathspec) {
			const git_attr_fnmatch *match = git_vector_get(vspec, pos);
			*matched_pathspec = match->pattern;
		}

		if (matched_at)
			*matched_at = pos;
	}

	return (result > 0);
}


int git_pathspec__init(git_pathspec *ps, const git_strarray *paths)
{
	int error = 0;

	memset(ps, 0, sizeof(*ps));

	ps->prefix = git_pathspec_prefix(paths);

	if ((error = git_pool_init(&ps->pool, 1)) < 0 ||
	    (error = git_pathspec__vinit(&ps->pathspec, paths, &ps->pool)) < 0)
		git_pathspec__clear(ps);

	return error;
}

void git_pathspec__clear(git_pathspec *ps)
{
	git__free(ps->prefix);
	git_pathspec__vfree(&ps->pathspec);
	git_pool_clear(&ps->pool);
	memset(ps, 0, sizeof(*ps));
}

int git_pathspec_new(git_pathspec **out, const git_strarray *pathspec)
{
	int error = 0;
	git_pathspec *ps = git__malloc(sizeof(git_pathspec));
	GIT_ERROR_CHECK_ALLOC(ps);

	if ((error = git_pathspec__init(ps, pathspec)) < 0) {
		git__free(ps);
		return error;
	}

	GIT_REFCOUNT_INC(ps);
	*out = ps;
	return 0;
}

static void pathspec_free(git_pathspec *ps)
{
	git_pathspec__clear(ps);
	git__free(ps);
}

void git_pathspec_free(git_pathspec *ps)
{
	if (!ps)
		return;
	GIT_REFCOUNT_DEC(ps, pathspec_free);
}

int git_pathspec_matches_path(
	const git_pathspec *ps, uint32_t flags, const char *path)
{
	bool no_fnmatch = (flags & GIT_PATHSPEC_NO_GLOB) != 0;
	bool casefold =  (flags & GIT_PATHSPEC_IGNORE_CASE) != 0;

	GIT_ASSERT_ARG(ps);
	GIT_ASSERT_ARG(path);

	return (0 != git_pathspec__match(
		&ps->pathspec, path, no_fnmatch, casefold, NULL, NULL));
}

static void pathspec_match_free(git_pathspec_match_list *m)
{
	if (!m)
		return;

	git_pathspec_free(m->pathspec);
	m->pathspec = NULL;

	git_array_clear(m->matches);
	git_array_clear(m->failures);
	git_pool_clear(&m->pool);
	git__free(m);
}

static git_pathspec_match_list *pathspec_match_alloc(
	git_pathspec *ps, int datatype)
{
	git_pathspec_match_list *m = git__calloc(1, sizeof(git_pathspec_match_list));
	if (!m)
		return NULL;

	if (git_pool_init(&m->pool, 1) < 0)
		return NULL;

	/* need to keep reference to pathspec and increment refcount because
	 * failures array stores pointers to the pattern strings of the
	 * pathspec that had no matches
	 */
	GIT_REFCOUNT_INC(ps);
	m->pathspec = ps;
	m->datatype = datatype;

	return m;
}

GIT_INLINE(size_t) pathspec_mark_pattern(git_bitvec *used, size_t pos)
{
	if (!git_bitvec_get(used, pos)) {
		git_bitvec_set(used, pos, true);
		return 1;
	}

	return 0;
}

static size_t pathspec_mark_remaining(
	git_bitvec *used,
	git_vector *patterns,
	struct pathspec_match_context *ctxt,
	size_t start,
	const char *path0,
	const char *path1)
{
	size_t count = 0;

	if (path1 == path0)
		path1 = NULL;

	for (; start < patterns->length; ++start) {
		const git_attr_fnmatch *pat = git_vector_get(patterns, start);

		if (git_bitvec_get(used, start))
			continue;

		if (path0 && pathspec_match_one(pat, ctxt, path0) > 0)
			count += pathspec_mark_pattern(used, start);
		else if (path1 && pathspec_match_one(pat, ctxt, path1) > 0)
			count += pathspec_mark_pattern(used, start);
	}

	return count;
}

static int pathspec_build_failure_array(
	git_pathspec_string_array_t *failures,
	git_vector *patterns,
	git_bitvec *used,
	git_pool *pool)
{
	size_t pos;
	char **failed;
	const git_attr_fnmatch *pat;

	for (pos = 0; pos < patterns->length; ++pos) {
		if (git_bitvec_get(used, pos))
			continue;

		if ((failed = git_array_alloc(*failures)) == NULL)
			return -1;

		pat = git_vector_get(patterns, pos);

		if ((*failed = git_pool_strdup(pool, pat->pattern)) == NULL)
			return -1;
	}

	return 0;
}

static int pathspec_match_from_iterator(
	git_pathspec_match_list **out,
	git_iterator *iter,
	uint32_t flags,
	git_pathspec *ps)
{
	int error = 0;
	git_pathspec_match_list *m = NULL;
	const git_index_entry *entry = NULL;
	struct pathspec_match_context ctxt;
	git_vector *patterns = &ps->pathspec;
	bool find_failures = out && (flags & GIT_PATHSPEC_FIND_FAILURES) != 0;
	bool failures_only = !out || (flags & GIT_PATHSPEC_FAILURES_ONLY) != 0;
	size_t pos, used_ct = 0, found_files = 0;
	git_index *index = NULL;
	git_bitvec used_patterns;
	char **file;

	if (git_bitvec_init(&used_patterns, patterns->length) < 0)
		return -1;

	if (out) {
		*out = m = pathspec_match_alloc(ps, PATHSPEC_DATATYPE_STRINGS);
		GIT_ERROR_CHECK_ALLOC(m);
	}

	if ((error = git_iterator_reset_range(iter, ps->prefix, ps->prefix)) < 0)
		goto done;

	if (git_iterator_type(iter) == GIT_ITERATOR_WORKDIR &&
		(error = git_repository_index__weakptr(
			&index, git_iterator_owner(iter))) < 0)
		goto done;

	pathspec_match_context_init(
		&ctxt, (flags & GIT_PATHSPEC_NO_GLOB) != 0,
		git_iterator_ignore_case(iter));

	while (!(error = git_iterator_advance(&entry, iter))) {
		/* search for match with entry->path */
		int result = git_pathspec__match_at(
			&pos, patterns, &ctxt, entry->path, NULL);

		/* no matches for this path */
		if (result < 0)
			continue;

		/* if result was a negative pattern match, then don't list file */
		if (!result) {
			used_ct += pathspec_mark_pattern(&used_patterns, pos);
			continue;
		}

		/* check if path is ignored and untracked */
		if (index != NULL &&
			git_iterator_current_is_ignored(iter) &&
			git_index__find_pos(NULL, index, entry->path, 0, GIT_INDEX_STAGE_ANY) < 0)
			continue;

		/* mark the matched pattern as used */
		used_ct += pathspec_mark_pattern(&used_patterns, pos);
		++found_files;

		/* if find_failures is on, check if any later patterns also match */
		if (find_failures && used_ct < patterns->length)
			used_ct += pathspec_mark_remaining(
				&used_patterns, patterns, &ctxt, pos + 1, entry->path, NULL);

		/* if only looking at failures, exit early or just continue */
		if (failures_only || !out) {
			if (used_ct == patterns->length)
				break;
			continue;
		}

		/* insert matched path into matches array */
		if ((file = (char **)git_array_alloc(m->matches)) == NULL ||
			(*file = git_pool_strdup(&m->pool, entry->path)) == NULL) {
			error = -1;
			goto done;
		}
	}

	if (error < 0 && error != GIT_ITEROVER)
		goto done;
	error = 0;

	/* insert patterns that had no matches into failures array */
	if (find_failures && used_ct < patterns->length &&
		(error = pathspec_build_failure_array(
			&m->failures, patterns, &used_patterns, &m->pool)) < 0)
		goto done;

	/* if every pattern failed to match, then we have failed */
	if ((flags & GIT_PATHSPEC_NO_MATCH_ERROR) != 0 && !found_files) {
		git_error_set(GIT_ERROR_INVALID, "no matching files were found");
		error = GIT_ENOTFOUND;
	}

done:
	git_bitvec_free(&used_patterns);

	if (error < 0) {
		pathspec_match_free(m);
		if (out) *out = NULL;
	}

	return error;
}

static git_iterator_flag_t pathspec_match_iter_flags(uint32_t flags)
{
	git_iterator_flag_t f = 0;

	if ((flags & GIT_PATHSPEC_IGNORE_CASE) != 0)
		f |= GIT_ITERATOR_IGNORE_CASE;
	else if ((flags & GIT_PATHSPEC_USE_CASE) != 0)
		f |= GIT_ITERATOR_DONT_IGNORE_CASE;

	return f;
}

int git_pathspec_match_workdir(
	git_pathspec_match_list **out,
	git_repository *repo,
	uint32_t flags,
	git_pathspec *ps)
{
	git_iterator *iter;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;
	int error = 0;

	GIT_ASSERT_ARG(repo);

	iter_opts.flags = pathspec_match_iter_flags(flags);

	if (!(error = git_iterator_for_workdir(&iter, repo, NULL, NULL, &iter_opts))) {
		error = pathspec_match_from_iterator(out, iter, flags, ps);
		git_iterator_free(iter);
	}

	return error;
}

int git_pathspec_match_index(
	git_pathspec_match_list **out,
	git_index *index,
	uint32_t flags,
	git_pathspec *ps)
{
	git_iterator *iter;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;
	int error = 0;

	GIT_ASSERT_ARG(index);

	iter_opts.flags = pathspec_match_iter_flags(flags);

	if (!(error = git_iterator_for_index(&iter, git_index_owner(index), index, &iter_opts))) {
		error = pathspec_match_from_iterator(out, iter, flags, ps);
		git_iterator_free(iter);
	}

	return error;
}

int git_pathspec_match_tree(
	git_pathspec_match_list **out,
	git_tree *tree,
	uint32_t flags,
	git_pathspec *ps)
{
	git_iterator *iter;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;
	int error = 0;

	GIT_ASSERT_ARG(tree);

	iter_opts.flags = pathspec_match_iter_flags(flags);

	if (!(error = git_iterator_for_tree(&iter, tree, &iter_opts))) {
		error = pathspec_match_from_iterator(out, iter, flags, ps);
		git_iterator_free(iter);
	}

	return error;
}

int git_pathspec_match_diff(
	git_pathspec_match_list **out,
	git_diff *diff,
	uint32_t flags,
	git_pathspec *ps)
{
	int error = 0;
	git_pathspec_match_list *m = NULL;
	struct pathspec_match_context ctxt;
	git_vector *patterns = &ps->pathspec;
	bool find_failures = out && (flags & GIT_PATHSPEC_FIND_FAILURES) != 0;
	bool failures_only = !out || (flags & GIT_PATHSPEC_FAILURES_ONLY) != 0;
	size_t i, pos, used_ct = 0, found_deltas = 0;
	const git_diff_delta *delta, **match;
	git_bitvec used_patterns;

	GIT_ASSERT_ARG(diff);

	if (git_bitvec_init(&used_patterns, patterns->length) < 0)
		return -1;

	if (out) {
		*out = m = pathspec_match_alloc(ps, PATHSPEC_DATATYPE_DIFF);
		GIT_ERROR_CHECK_ALLOC(m);
	}

	pathspec_match_context_init(
		&ctxt, (flags & GIT_PATHSPEC_NO_GLOB) != 0,
		git_diff_is_sorted_icase(diff));

	git_vector_foreach(&diff->deltas, i, delta) {
		/* search for match with delta */
		int result = git_pathspec__match_at(
			&pos, patterns, &ctxt, delta->old_file.path, delta->new_file.path);

		/* no matches for this path */
		if (result < 0)
			continue;

		/* mark the matched pattern as used */
		used_ct += pathspec_mark_pattern(&used_patterns, pos);

		/* if result was a negative pattern match, then don't list file */
		if (!result)
			continue;

		++found_deltas;

		/* if find_failures is on, check if any later patterns also match */
		if (find_failures && used_ct < patterns->length)
			used_ct += pathspec_mark_remaining(
				&used_patterns, patterns, &ctxt, pos + 1,
				delta->old_file.path, delta->new_file.path);

		/* if only looking at failures, exit early or just continue */
		if (failures_only || !out) {
			if (used_ct == patterns->length)
				break;
			continue;
		}

		/* insert matched delta into matches array */
		if (!(match = (const git_diff_delta **)git_array_alloc(m->matches))) {
			error = -1;
			goto done;
		} else {
			*match = delta;
		}
	}

	/* insert patterns that had no matches into failures array */
	if (find_failures && used_ct < patterns->length &&
		(error = pathspec_build_failure_array(
			&m->failures, patterns, &used_patterns, &m->pool)) < 0)
		goto done;

	/* if every pattern failed to match, then we have failed */
	if ((flags & GIT_PATHSPEC_NO_MATCH_ERROR) != 0 && !found_deltas) {
		git_error_set(GIT_ERROR_INVALID, "no matching deltas were found");
		error = GIT_ENOTFOUND;
	}

done:
	git_bitvec_free(&used_patterns);

	if (error < 0) {
		pathspec_match_free(m);
		if (out) *out = NULL;
	}

	return error;
}

void git_pathspec_match_list_free(git_pathspec_match_list *m)
{
	if (m)
		pathspec_match_free(m);
}

size_t git_pathspec_match_list_entrycount(
	const git_pathspec_match_list *m)
{
	return m ? git_array_size(m->matches) : 0;
}

const char *git_pathspec_match_list_entry(
	const git_pathspec_match_list *m, size_t pos)
{
	if (!m || m->datatype != PATHSPEC_DATATYPE_STRINGS ||
		!git_array_valid_index(m->matches, pos))
		return NULL;

	return *((const char **)git_array_get(m->matches, pos));
}

const git_diff_delta *git_pathspec_match_list_diff_entry(
	const git_pathspec_match_list *m, size_t pos)
{
	if (!m || m->datatype != PATHSPEC_DATATYPE_DIFF ||
		!git_array_valid_index(m->matches, pos))
		return NULL;

	return *((const git_diff_delta **)git_array_get(m->matches, pos));
}

size_t git_pathspec_match_list_failed_entrycount(
	const git_pathspec_match_list *m)
{
	return m ? git_array_size(m->failures) : 0;
}

const char * git_pathspec_match_list_failed_entry(
	const git_pathspec_match_list *m, size_t pos)
{
	char **entry = m ? git_array_get(m->failures, pos) : NULL;

	return entry ? *entry : NULL;
}
