/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "ignore.h"

#include "git2/ignore.h"
#include "common.h"
#include "attrcache.h"
#include "fs_path.h"
#include "config.h"
#include "wildmatch.h"
#include "path.h"

#define GIT_IGNORE_INTERNAL		"[internal]exclude"

#define GIT_IGNORE_DEFAULT_RULES ".\n..\n.git\n"

/**
 * A negative ignore pattern can negate a positive one without
 * wildcards if it is a basename only and equals the basename of
 * the positive pattern. Thus
 *
 * foo/bar
 * !bar
 *
 * would result in foo/bar being unignored again while
 *
 * moo/foo/bar
 * !foo/bar
 *
 * would do nothing. The reverse also holds true: a positive
 * basename pattern can be negated by unignoring the basename in
 * subdirectories. Thus
 *
 * bar
 * !foo/bar
 *
 * would result in foo/bar being unignored again. As with the
 * first case,
 *
 * foo/bar
 * !moo/foo/bar
 *
 * would do nothing, again.
 */
static int does_negate_pattern(git_attr_fnmatch *rule, git_attr_fnmatch *neg)
{
	int (*cmp)(const char *, const char *, size_t);
	git_attr_fnmatch *longer, *shorter;
	char *p;

	if ((rule->flags & GIT_ATTR_FNMATCH_NEGATIVE) != 0
	    || (neg->flags & GIT_ATTR_FNMATCH_NEGATIVE) == 0)
		return false;

	if (neg->flags & GIT_ATTR_FNMATCH_ICASE)
		cmp = git__strncasecmp;
	else
		cmp = git__strncmp;

	/* If lengths match we need to have an exact match */
	if (rule->length == neg->length) {
		return cmp(rule->pattern, neg->pattern, rule->length) == 0;
	} else if (rule->length < neg->length) {
		shorter = rule;
		longer = neg;
	} else {
		shorter = neg;
		longer = rule;
	}

	/* Otherwise, we need to check if the shorter
	 * rule is a basename only (that is, it contains
	 * no path separator) and, if so, if it
	 * matches the tail of the longer rule */
	p = longer->pattern + longer->length - shorter->length;

	if (p[-1] != '/')
		return false;
	if (memchr(shorter->pattern, '/', shorter->length) != NULL)
		return false;

	return cmp(p, shorter->pattern, shorter->length) == 0;
}

/**
 * A negative ignore can only unignore a file which is given explicitly before, thus
 *
 *    foo
 *    !foo/bar
 *
 * does not unignore 'foo/bar' as it's not in the list. However
 *
 *    foo/<star>
 *    !foo/bar
 *
 * does unignore 'foo/bar', as it is contained within the 'foo/<star>' rule.
 */
static int does_negate_rule(int *out, git_vector *rules, git_attr_fnmatch *match)
{
	int error = 0, wildmatch_flags, effective_flags;
	size_t i;
	git_attr_fnmatch *rule;
	char *path;
	git_str buf = GIT_STR_INIT;

	*out = 0;

	wildmatch_flags = WM_PATHNAME;
	if (match->flags & GIT_ATTR_FNMATCH_ICASE)
		wildmatch_flags |= WM_CASEFOLD;

	/* path of the file relative to the workdir, so we match the rules in subdirs */
	if (match->containing_dir) {
		git_str_puts(&buf, match->containing_dir);
	}
	if (git_str_puts(&buf, match->pattern) < 0)
		return -1;

	path = git_str_detach(&buf);

	git_vector_foreach(rules, i, rule) {
		if (!(rule->flags & GIT_ATTR_FNMATCH_HASWILD)) {
			if (does_negate_pattern(rule, match)) {
				error = 0;
				*out = 1;
				goto out;
			}
			else
				continue;
		}

		git_str_clear(&buf);
		if (rule->containing_dir)
			git_str_puts(&buf, rule->containing_dir);
		git_str_puts(&buf, rule->pattern);

		if (git_str_oom(&buf))
			goto out;

		/*
		 * if rule isn't for full path we match without PATHNAME flag
		 * as lines like *.txt should match something like dir/test.txt
		 * requiring * to also match /
		 */
		effective_flags = wildmatch_flags;
		if (!(rule->flags & GIT_ATTR_FNMATCH_FULLPATH))
			effective_flags &= ~WM_PATHNAME;

		/* if we found a match, we want to keep this rule */
		if ((wildmatch(git_str_cstr(&buf), path, effective_flags)) == WM_MATCH) {
			*out = 1;
			error = 0;
			goto out;
		}
	}

	error = 0;

out:
	git__free(path);
	git_str_dispose(&buf);
	return error;
}

static int parse_ignore_file(
	git_repository *repo, git_attr_file *attrs, const char *data, bool allow_macros)
{
	int error = 0;
	int ignore_case = false;
	const char *scan = data, *context = NULL;
	git_attr_fnmatch *match = NULL;

	GIT_UNUSED(allow_macros);

	if (git_repository__configmap_lookup(&ignore_case, repo, GIT_CONFIGMAP_IGNORECASE) < 0)
		git_error_clear();

	/* if subdir file path, convert context for file paths */
	if (attrs->entry &&
		git_fs_path_root(attrs->entry->path) < 0 &&
		!git__suffixcmp(attrs->entry->path, "/" GIT_IGNORE_FILE))
		context = attrs->entry->path;

	if (git_mutex_lock(&attrs->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock ignore file");
		return -1;
	}

	while (!error && *scan) {
		int valid_rule = 1;

		if (!match && !(match = git__calloc(1, sizeof(*match)))) {
			error = -1;
			break;
		}

		match->flags =
		    GIT_ATTR_FNMATCH_ALLOWSPACE | GIT_ATTR_FNMATCH_ALLOWNEG;

		if (!(error = git_attr_fnmatch__parse(
			match, &attrs->pool, context, &scan)))
		{
			match->flags |= GIT_ATTR_FNMATCH_IGNORE;

			if (ignore_case)
				match->flags |= GIT_ATTR_FNMATCH_ICASE;

			scan = git__next_line(scan);

			/*
			 * If a negative match doesn't actually do anything,
			 * throw it away. As we cannot always verify whether a
			 * rule containing wildcards negates another rule, we
			 * do not optimize away these rules, though.
			 * */
			if (match->flags & GIT_ATTR_FNMATCH_NEGATIVE
			    && !(match->flags & GIT_ATTR_FNMATCH_HASWILD))
				error = does_negate_rule(&valid_rule, &attrs->rules, match);

			if (!error && valid_rule)
				error = git_vector_insert(&attrs->rules, match);
		}

		if (error != 0 || !valid_rule) {
			match->pattern = NULL;

			if (error == GIT_ENOTFOUND)
				error = 0;
		} else {
			match = NULL; /* vector now "owns" the match */
		}
	}

	git_mutex_unlock(&attrs->lock);
	git__free(match);

	return error;
}

static int push_ignore_file(
	git_ignores *ignores,
	git_vector *which_list,
	const char *base,
	const char *filename)
{
	git_attr_file_source source = { GIT_ATTR_FILE_SOURCE_FILE, base, filename };
	git_attr_file *file = NULL;
	int error = 0;

	error = git_attr_cache__get(&file, ignores->repo, NULL, &source, parse_ignore_file, false);

	if (error < 0)
		return error;

	if (file != NULL) {
		if ((error = git_vector_insert(which_list, file)) < 0)
			git_attr_file__free(file);
	}

	return error;
}

static int push_one_ignore(void *payload, const char *path)
{
	git_ignores *ign = payload;
	ign->depth++;
	return push_ignore_file(ign, &ign->ign_path, path, GIT_IGNORE_FILE);
}

static int get_internal_ignores(git_attr_file **out, git_repository *repo)
{
	git_attr_file_source source = { GIT_ATTR_FILE_SOURCE_MEMORY, NULL, GIT_IGNORE_INTERNAL };
	int error;

	if ((error = git_attr_cache__init(repo)) < 0)
		return error;

	error = git_attr_cache__get(out, repo, NULL, &source, NULL, false);

	/* if internal rules list is empty, insert default rules */
	if (!error && !(*out)->rules.length)
		error = parse_ignore_file(repo, *out, GIT_IGNORE_DEFAULT_RULES, false);

	return error;
}

int git_ignore__for_path(
	git_repository *repo,
	const char *path,
	git_ignores *ignores)
{
	int error = 0;
	const char *workdir = git_repository_workdir(repo);
	git_attr_cache *attrcache;
	const char *excludes_file = NULL;
	git_str infopath = GIT_STR_INIT;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(ignores);
	GIT_ASSERT_ARG(path);

	memset(ignores, 0, sizeof(*ignores));
	ignores->repo = repo;

	/* Read the ignore_case flag */
	if ((error = git_repository__configmap_lookup(
			&ignores->ignore_case, repo, GIT_CONFIGMAP_IGNORECASE)) < 0)
		goto cleanup;

	if ((error = git_attr_cache__init(repo)) < 0)
		goto cleanup;

	/* given a unrooted path in a non-bare repo, resolve it */
	if (workdir && git_fs_path_root(path) < 0) {
		git_str local = GIT_STR_INIT;

		if ((error = git_fs_path_dirname_r(&local, path)) < 0 ||
		    (error = git_fs_path_resolve_relative(&local, 0)) < 0 ||
		    (error = git_fs_path_to_dir(&local)) < 0 ||
		    (error = git_str_joinpath(&ignores->dir, workdir, local.ptr)) < 0 ||
		    (error = git_path_validate_str_length(repo, &ignores->dir)) < 0) {
			/* Nothing, we just want to stop on the first error */
		}

		git_str_dispose(&local);
	} else {
		if (!(error = git_str_joinpath(&ignores->dir, path, "")))
		    error = git_path_validate_str_length(NULL, &ignores->dir);
	}

	if (error < 0)
		goto cleanup;

	if (workdir && !git__prefixcmp(ignores->dir.ptr, workdir))
		ignores->dir_root = strlen(workdir);

	/* set up internals */
	if ((error = get_internal_ignores(&ignores->ign_internal, repo)) < 0)
		goto cleanup;

	/* load .gitignore up the path */
	if (workdir != NULL) {
		error = git_fs_path_walk_up(
			&ignores->dir, workdir, push_one_ignore, ignores);
		if (error < 0)
			goto cleanup;
	}

	/* load .git/info/exclude if possible */
	if ((error = git_repository__item_path(&infopath, repo, GIT_REPOSITORY_ITEM_INFO)) < 0 ||
		(error = push_ignore_file(ignores, &ignores->ign_global, infopath.ptr, GIT_IGNORE_FILE_INREPO)) < 0) {
		if (error != GIT_ENOTFOUND)
			goto cleanup;
		error = 0;
	}

	/* load core.excludesfile */
	attrcache = git_repository_attr_cache(repo);
	excludes_file = git_attr_cache_excludesfile(attrcache);

	if (excludes_file != NULL)
		error = push_ignore_file(
			ignores, &ignores->ign_global, NULL, excludes_file);

cleanup:
	git_str_dispose(&infopath);
	if (error < 0)
		git_ignore__free(ignores);

	return error;
}

int git_ignore__push_dir(git_ignores *ign, const char *dir)
{
	if (git_str_joinpath(&ign->dir, ign->dir.ptr, dir) < 0)
		return -1;

	ign->depth++;

	return push_ignore_file(
		ign, &ign->ign_path, ign->dir.ptr, GIT_IGNORE_FILE);
}

int git_ignore__pop_dir(git_ignores *ign)
{
	if (ign->ign_path.length > 0) {
		git_attr_file *file = git_vector_last(&ign->ign_path);
		const char *start = file->entry->path, *end;

		/* - ign->dir looks something like "/home/user/a/b/" (or "a/b/c/d/")
		 * - file->path looks something like "a/b/.gitignore
		 *
		 * We are popping the last directory off ign->dir.  We also want
		 * to remove the file from the vector if the popped directory
		 * matches the ignore path.  We need to test if the "a/b" part of
		 * the file key matches the path we are about to pop.
		 */

		if ((end = strrchr(start, '/')) != NULL) {
			size_t dirlen = (end - start) + 1;
			const char *relpath = ign->dir.ptr + ign->dir_root;
			size_t pathlen = ign->dir.size - ign->dir_root;

			if (pathlen == dirlen && !memcmp(relpath, start, dirlen)) {
				git_vector_pop(&ign->ign_path);
				git_attr_file__free(file);
			}
		}
	}

	if (--ign->depth > 0) {
		git_str_rtruncate_at_char(&ign->dir, '/');
		git_fs_path_to_dir(&ign->dir);
	}

	return 0;
}

void git_ignore__free(git_ignores *ignores)
{
	unsigned int i;
	git_attr_file *file;

	git_attr_file__free(ignores->ign_internal);

	git_vector_foreach(&ignores->ign_path, i, file) {
		git_attr_file__free(file);
		ignores->ign_path.contents[i] = NULL;
	}
	git_vector_dispose(&ignores->ign_path);

	git_vector_foreach(&ignores->ign_global, i, file) {
		git_attr_file__free(file);
		ignores->ign_global.contents[i] = NULL;
	}
	git_vector_dispose(&ignores->ign_global);

	git_str_dispose(&ignores->dir);
}

static bool ignore_lookup_in_rules(
	int *ignored, git_attr_file *file, git_attr_path *path)
{
	size_t j;
	git_attr_fnmatch *match;

	git_vector_rforeach(&file->rules, j, match) {
		if (match->flags & GIT_ATTR_FNMATCH_DIRECTORY &&
		    path->is_dir == GIT_DIR_FLAG_FALSE)
			continue;
		if (git_attr_fnmatch__match(match, path)) {
			*ignored = ((match->flags & GIT_ATTR_FNMATCH_NEGATIVE) == 0) ?
				GIT_IGNORE_TRUE : GIT_IGNORE_FALSE;
			return true;
		}
	}

	return false;
}

int git_ignore__lookup(
	int *out, git_ignores *ignores, const char *pathname, git_dir_flag dir_flag)
{
	size_t i;
	git_attr_file *file;
	git_attr_path path;

	*out = GIT_IGNORE_NOTFOUND;

	if (git_attr_path__init(
		&path, pathname, git_repository_workdir(ignores->repo), dir_flag) < 0)
		return -1;

	/* first process builtins - success means path was found */
	if (ignore_lookup_in_rules(out, ignores->ign_internal, &path))
		goto cleanup;

	/* next process files in the path.
	 * this process has to process ignores in reverse order
	 * to ensure correct prioritization of rules
	 */
	git_vector_rforeach(&ignores->ign_path, i, file) {
		if (ignore_lookup_in_rules(out, file, &path))
			goto cleanup;
	}

	/* last process global ignores */
	git_vector_foreach(&ignores->ign_global, i, file) {
		if (ignore_lookup_in_rules(out, file, &path))
			goto cleanup;
	}

cleanup:
	git_attr_path__free(&path);
	return 0;
}

int git_ignore_add_rule(git_repository *repo, const char *rules)
{
	int error;
	git_attr_file *ign_internal = NULL;

	if ((error = get_internal_ignores(&ign_internal, repo)) < 0)
		return error;

	error = parse_ignore_file(repo, ign_internal, rules, false);
	git_attr_file__free(ign_internal);

	return error;
}

int git_ignore_clear_internal_rules(git_repository *repo)
{
	int error;
	git_attr_file *ign_internal;

	if ((error = get_internal_ignores(&ign_internal, repo)) < 0)
		return error;

	if (!(error = git_attr_file__clear_rules(ign_internal, true)))
		error = parse_ignore_file(
				repo, ign_internal, GIT_IGNORE_DEFAULT_RULES, false);

	git_attr_file__free(ign_internal);
	return error;
}

int git_ignore_path_is_ignored(
	int *ignored,
	git_repository *repo,
	const char *pathname)
{
	int error;
	const char *workdir;
	git_attr_path path;
	git_ignores ignores;
	unsigned int i;
	git_attr_file *file;
	git_dir_flag dir_flag = GIT_DIR_FLAG_UNKNOWN;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(ignored);
	GIT_ASSERT_ARG(pathname);

	workdir = git_repository_workdir(repo);

	memset(&path, 0, sizeof(path));
	memset(&ignores, 0, sizeof(ignores));

	if (!git__suffixcmp(pathname, "/"))
		dir_flag = GIT_DIR_FLAG_TRUE;
	else if (git_repository_is_bare(repo))
		dir_flag = GIT_DIR_FLAG_FALSE;

	if ((error = git_attr_path__init(&path, pathname, workdir, dir_flag)) < 0 ||
		(error = git_ignore__for_path(repo, path.path, &ignores)) < 0)
		goto cleanup;

	while (1) {
		/* first process builtins - success means path was found */
		if (ignore_lookup_in_rules(ignored, ignores.ign_internal, &path))
			goto cleanup;

		/* next process files in the path */
		git_vector_foreach(&ignores.ign_path, i, file) {
			if (ignore_lookup_in_rules(ignored, file, &path))
				goto cleanup;
		}

		/* last process global ignores */
		git_vector_foreach(&ignores.ign_global, i, file) {
			if (ignore_lookup_in_rules(ignored, file, &path))
				goto cleanup;
		}

		/* move up one directory */
		if (path.basename == path.path)
			break;
		path.basename[-1] = '\0';
		while (path.basename > path.path && *path.basename != '/')
			path.basename--;
		if (path.basename > path.path)
			path.basename++;
		path.is_dir = 1;

		if ((error = git_ignore__pop_dir(&ignores)) < 0)
			break;
	}

	*ignored = 0;

cleanup:
	git_attr_path__free(&path);
	git_ignore__free(&ignores);
	return error;
}

int git_ignore__check_pathspec_for_exact_ignores(
	git_repository *repo,
	git_vector *vspec,
	bool no_fnmatch)
{
	int error = 0;
	size_t i;
	git_attr_fnmatch *match;
	int ignored;
	git_str path = GIT_STR_INIT;
	const char *filename;
	git_index *idx;

	if ((error = git_repository__ensure_not_bare(
			repo, "validate pathspec")) < 0 ||
		(error = git_repository_index(&idx, repo)) < 0)
		return error;

	git_vector_foreach(vspec, i, match) {
		/* skip wildcard matches (if they are being used) */
		if ((match->flags & GIT_ATTR_FNMATCH_HASWILD) != 0 &&
			!no_fnmatch)
			continue;

		filename = match->pattern;

		/* if file is already in the index, it's fine */
		if (git_index_get_bypath(idx, filename, 0) != NULL)
			continue;

		if ((error = git_repository_workdir_path(&path, repo, filename)) < 0)
			break;

		/* is there a file on disk that matches this exactly? */
		if (!git_fs_path_isfile(path.ptr))
			continue;

		/* is that file ignored? */
		if ((error = git_ignore_path_is_ignored(&ignored, repo, filename)) < 0)
			break;

		if (ignored) {
			git_error_set(GIT_ERROR_INVALID, "pathspec contains ignored file '%s'",
				filename);
			error = GIT_EINVALIDSPEC;
			break;
		}
	}

	git_index_free(idx);
	git_str_dispose(&path);

	return error;
}
