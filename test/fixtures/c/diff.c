/*
 * Copyright (C) 2012 the libgit2 contributors
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "git2/diff.h"
#include "diff.h"
#include "fileops.h"
#include "config.h"
#include "attr_file.h"

static char *diff_prefix_from_pathspec(const git_strarray *pathspec)
{
	git_buf prefix = GIT_BUF_INIT;
	const char *scan;

	if (git_buf_common_prefix(&prefix, pathspec) < 0)
		return NULL;

	/* diff prefix will only be leading non-wildcards */
	for (scan = prefix.ptr; *scan && !git__iswildcard(*scan); ++scan);
	git_buf_truncate(&prefix, scan - prefix.ptr);

	if (prefix.size > 0)
		return git_buf_detach(&prefix);

	git_buf_free(&prefix);
	return NULL;
}

static bool diff_pathspec_is_interesting(const git_strarray *pathspec)
{
	const char *str;

	if (pathspec == NULL || pathspec->count == 0)
		return false;
	if (pathspec->count > 1)
		return true;

	str = pathspec->strings[0];
	if (!str || !str[0] || (!str[1] && (str[0] == '*' || str[0] == '.')))
		return false;
	return true;
}

static bool diff_path_matches_pathspec(git_diff_list *diff, const char *path)
{
	unsigned int i;
	git_attr_fnmatch *match;

	if (!diff->pathspec.length)
		return true;

	git_vector_foreach(&diff->pathspec, i, match) {
		int result = p_fnmatch(match->pattern, path, 0);

		/* if we didn't match, look for exact dirname prefix match */
		if (result == FNM_NOMATCH &&
			(match->flags & GIT_ATTR_FNMATCH_HASWILD) == 0 &&
			strncmp(path, match->pattern, match->length) == 0 &&
			path[match->length] == '/')
			result = 0;

		if (result == 0)
			return (match->flags & GIT_ATTR_FNMATCH_NEGATIVE) ? false : true;
	}

	return false;
}

static git_diff_delta *diff_delta__alloc(
	git_diff_list *diff,
	git_delta_t status,
	const char *path)
{
	git_diff_delta *delta = git__calloc(1, sizeof(git_diff_delta));
	if (!delta)
		return NULL;

	delta->old_file.path = git_pool_strdup(&diff->pool, path);
	if (delta->old_file.path == NULL) {
		git__free(delta);
		return NULL;
	}

	delta->new_file.path = delta->old_file.path;

	if (diff->opts.flags & GIT_DIFF_REVERSE) {
		switch (status) {
		case GIT_DELTA_ADDED:   status = GIT_DELTA_DELETED; break;
		case GIT_DELTA_DELETED: status = GIT_DELTA_ADDED; break;
		default: break; /* leave other status values alone */
		}
	}
	delta->status = status;

	return delta;
}

static git_diff_delta *diff_delta__dup(
	const git_diff_delta *d, git_pool *pool)
{
	git_diff_delta *delta = git__malloc(sizeof(git_diff_delta));
	if (!delta)
		return NULL;

	memcpy(delta, d, sizeof(git_diff_delta));

	delta->old_file.path = git_pool_strdup(pool, d->old_file.path);
	if (delta->old_file.path == NULL)
		goto fail;

	if (d->new_file.path != d->old_file.path) {
		delta->new_file.path = git_pool_strdup(pool, d->new_file.path);
		if (delta->new_file.path == NULL)
			goto fail;
	} else {
		delta->new_file.path = delta->old_file.path;
	}

	return delta;

fail:
	git__free(delta);
	return NULL;
}

static git_diff_delta *diff_delta__merge_like_cgit(
	const git_diff_delta *a, const git_diff_delta *b, git_pool *pool)
{
	git_diff_delta *dup = diff_delta__dup(a, pool);
	if (!dup)
		return NULL;

	if (git_oid_cmp(&dup->new_file.oid, &b->new_file.oid) == 0)
		return dup;

	git_oid_cpy(&dup->new_file.oid, &b->new_file.oid);

	dup->new_file.mode = b->new_file.mode;
	dup->new_file.size = b->new_file.size;
	dup->new_file.flags = b->new_file.flags;

	/* Emulate C git for merging two diffs (a la 'git diff <sha>').
	 *
	 * When C git does a diff between the work dir and a tree, it actually
	 * diffs with the index but uses the workdir contents.  This emulates
	 * those choices so we can emulate the type of diff.
	 */
	if (git_oid_cmp(&dup->old_file.oid, &dup->new_file.oid) == 0) {
		if (dup->status == GIT_DELTA_DELETED)
			/* preserve pending delete info */;
		else if (b->status == GIT_DELTA_UNTRACKED ||
				 b->status == GIT_DELTA_IGNORED)
			dup->status = b->status;
		else
			dup->status = GIT_DELTA_UNMODIFIED;
	}
	else if (dup->status == GIT_DELTA_UNMODIFIED ||
			 b->status == GIT_DELTA_DELETED)
		dup->status = b->status;

	return dup;
}

static int diff_delta__from_one(
	git_diff_list *diff,
	git_delta_t   status,
	const git_index_entry *entry)
{
	git_diff_delta *delta;

	if (status == GIT_DELTA_IGNORED &&
		(diff->opts.flags & GIT_DIFF_INCLUDE_IGNORED) == 0)
		return 0;

	if (status == GIT_DELTA_UNTRACKED &&
		(diff->opts.flags & GIT_DIFF_INCLUDE_UNTRACKED) == 0)
		return 0;

	if (!diff_path_matches_pathspec(diff, entry->path))
		return 0;

	delta = diff_delta__alloc(diff, status, entry->path);
	GITERR_CHECK_ALLOC(delta);

	/* This fn is just for single-sided diffs */
	assert(status != GIT_DELTA_MODIFIED);

	if (delta->status == GIT_DELTA_DELETED) {
		delta->old_file.mode = entry->mode;
		delta->old_file.size = entry->file_size;
		git_oid_cpy(&delta->old_file.oid, &entry->oid);
	} else /* ADDED, IGNORED, UNTRACKED */ {
		delta->new_file.mode = entry->mode;
		delta->new_file.size = entry->file_size;
		git_oid_cpy(&delta->new_file.oid, &entry->oid);
	}

	delta->old_file.flags |= GIT_DIFF_FILE_VALID_OID;
	delta->new_file.flags |= GIT_DIFF_FILE_VALID_OID;

	if (git_vector_insert(&diff->deltas, delta) < 0) {
		git__free(delta);
		return -1;
	}

	return 0;
}

static int diff_delta__from_two(
	git_diff_list *diff,
	git_delta_t   status,
	const git_index_entry *old_entry,
	const git_index_entry *new_entry,
	git_oid *new_oid)
{
	git_diff_delta *delta;

	if (status == GIT_DELTA_UNMODIFIED &&
		(diff->opts.flags & GIT_DIFF_INCLUDE_UNMODIFIED) == 0)
		return 0;

	if ((diff->opts.flags & GIT_DIFF_REVERSE) != 0) {
		const git_index_entry *temp = old_entry;
		old_entry = new_entry;
		new_entry = temp;
	}

	delta = diff_delta__alloc(diff, status, old_entry->path);
	GITERR_CHECK_ALLOC(delta);

	delta->old_file.mode = old_entry->mode;
	git_oid_cpy(&delta->old_file.oid, &old_entry->oid);
	delta->old_file.flags |= GIT_DIFF_FILE_VALID_OID;

	delta->new_file.mode = new_entry->mode;
	git_oid_cpy(&delta->new_file.oid, new_oid ? new_oid : &new_entry->oid);
	if (new_oid || !git_oid_iszero(&new_entry->oid))
		delta->new_file.flags |= GIT_DIFF_FILE_VALID_OID;

	if (git_vector_insert(&diff->deltas, delta) < 0) {
		git__free(delta);
		return -1;
	}

	return 0;
}

static char *diff_strdup_prefix(git_pool *pool, const char *prefix)
{
	size_t len = strlen(prefix);

	/* append '/' at end if needed */
	if (len > 0 && prefix[len - 1] != '/')
		return git_pool_strcat(pool, prefix, "/");
	else
		return git_pool_strndup(pool, prefix, len + 1);
}

static int diff_delta__cmp(const void *a, const void *b)
{
	const git_diff_delta *da = a, *db = b;
	int val = strcmp(da->old_file.path, db->old_file.path);
	return val ? val : ((int)da->status - (int)db->status);
}

static int config_bool(git_config *cfg, const char *name, int defvalue)
{
	int val = defvalue;

	if (git_config_get_bool(&val, cfg, name) < 0)
		giterr_clear();

	return val;
}

static git_diff_list *git_diff_list_alloc(
	git_repository *repo, const git_diff_options *opts)
{
	git_config *cfg;
	size_t i;
	git_diff_list *diff = git__calloc(1, sizeof(git_diff_list));
	if (diff == NULL)
		return NULL;

	diff->repo = repo;

	if (git_vector_init(&diff->deltas, 0, diff_delta__cmp) < 0 ||
		git_pool_init(&diff->pool, 1, 0) < 0)
		goto fail;

	/* load config values that affect diff behavior */
	if (git_repository_config__weakptr(&cfg, repo) < 0)
		goto fail;
	if (config_bool(cfg, "core.symlinks", 1))
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_HAS_SYMLINKS;
	if (config_bool(cfg, "core.ignorestat", 0))
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_ASSUME_UNCHANGED;
	if (config_bool(cfg, "core.filemode", 1))
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_TRUST_EXEC_BIT;
	if (config_bool(cfg, "core.trustctime", 1))
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_TRUST_CTIME;
	/* Don't set GIT_DIFFCAPS_USE_DEV - compile time option in core git */

	if (opts == NULL)
		return diff;

	memcpy(&diff->opts, opts, sizeof(git_diff_options));
	memset(&diff->opts.pathspec, 0, sizeof(diff->opts.pathspec));

	diff->opts.old_prefix = diff_strdup_prefix(&diff->pool,
		opts->old_prefix ? opts->old_prefix : DIFF_OLD_PREFIX_DEFAULT);
	diff->opts.new_prefix = diff_strdup_prefix(&diff->pool,
		opts->new_prefix ? opts->new_prefix : DIFF_NEW_PREFIX_DEFAULT);

	if (!diff->opts.old_prefix || !diff->opts.new_prefix)
		goto fail;

	if (diff->opts.flags & GIT_DIFF_REVERSE) {
		char *swap = diff->opts.old_prefix;
		diff->opts.old_prefix = diff->opts.new_prefix;
		diff->opts.new_prefix = swap;
	}

	/* only copy pathspec if it is "interesting" so we can test
	 * diff->pathspec.length > 0 to know if it is worth calling
	 * fnmatch as we iterate.
	 */
	if (!diff_pathspec_is_interesting(&opts->pathspec))
		return diff;

	if (git_vector_init(
		&diff->pathspec, (unsigned int)opts->pathspec.count, NULL) < 0)
		goto fail;

	for (i = 0; i < opts->pathspec.count; ++i) {
		int ret;
		const char *pattern = opts->pathspec.strings[i];
		git_attr_fnmatch *match = git__calloc(1, sizeof(git_attr_fnmatch));
		if (!match)
			goto fail;
		match->flags = GIT_ATTR_FNMATCH_ALLOWSPACE;
		ret = git_attr_fnmatch__parse(match, &diff->pool, NULL, &pattern);
		if (ret == GIT_ENOTFOUND) {
			git__free(match);
			continue;
		} else if (ret < 0)
			goto fail;

		if (git_vector_insert(&diff->pathspec, match) < 0)
			goto fail;
	}

	return diff;

fail:
	git_diff_list_free(diff);
	return NULL;
}

void git_diff_list_free(git_diff_list *diff)
{
	git_diff_delta *delta;
	git_attr_fnmatch *match;
	unsigned int i;

	if (!diff)
		return;

	git_vector_foreach(&diff->deltas, i, delta) {
		git__free(delta);
		diff->deltas.contents[i] = NULL;
	}
	git_vector_free(&diff->deltas);

	git_vector_foreach(&diff->pathspec, i, match) {
		git__free(match);
		diff->pathspec.contents[i] = NULL;
	}
	git_vector_free(&diff->pathspec);

	git_pool_clear(&diff->pool);
	git__free(diff);
}

static int oid_for_workdir_item(
	git_repository *repo,
	const git_index_entry *item,
	git_oid *oid)
{
	int result;
	git_buf full_path = GIT_BUF_INIT;

	if (git_buf_joinpath(&full_path, git_repository_workdir(repo), item->path) < 0)
		return -1;

	/* calculate OID for file if possible*/
	if (S_ISLNK(item->mode))
		result = git_odb__hashlink(oid, full_path.ptr);
	else if (!git__is_sizet(item->file_size)) {
		giterr_set(GITERR_OS, "File size overflow for 32-bit systems");
		result = -1;
	} else {
		int fd = git_futils_open_ro(full_path.ptr);
		if (fd < 0)
			result = fd;
		else {
			result = git_odb__hashfd(
				oid, fd, (size_t)item->file_size, GIT_OBJ_BLOB);
			p_close(fd);
		}
	}

	git_buf_free(&full_path);

	return result;
}

#define EXEC_BIT_MASK 0000111

static int maybe_modified(
	git_iterator *old_iter,
	const git_index_entry *oitem,
	git_iterator *new_iter,
	const git_index_entry *nitem,
	git_diff_list *diff)
{
	git_oid noid, *use_noid = NULL;
	git_delta_t status = GIT_DELTA_MODIFIED;
	unsigned int omode = oitem->mode;
	unsigned int nmode = nitem->mode;

	GIT_UNUSED(old_iter);

	if (!diff_path_matches_pathspec(diff, oitem->path))
		return 0;

	/* on platforms with no symlinks, promote plain files to symlinks */
	if (S_ISLNK(omode) && S_ISREG(nmode) &&
		!(diff->diffcaps & GIT_DIFFCAPS_HAS_SYMLINKS))
		nmode = GIT_MODE_TYPE(omode) | (nmode & GIT_MODE_PERMS_MASK);

	/* on platforms with no execmode, clear exec bit from comparisons */
	if (!(diff->diffcaps & GIT_DIFFCAPS_TRUST_EXEC_BIT)) {
		omode = omode & ~EXEC_BIT_MASK;
		nmode = nmode & ~EXEC_BIT_MASK;
	}

	/* support "assume unchanged" (badly, b/c we still stat everything) */
	if ((diff->diffcaps & GIT_DIFFCAPS_ASSUME_UNCHANGED) != 0)
		status = (oitem->flags_extended & GIT_IDXENTRY_INTENT_TO_ADD) ?
			GIT_DELTA_MODIFIED : GIT_DELTA_UNMODIFIED;

	/* support "skip worktree" index bit */
	else if ((oitem->flags_extended & GIT_IDXENTRY_SKIP_WORKTREE) != 0)
		status = GIT_DELTA_UNMODIFIED;

	/* if basic type of file changed, then split into delete and add */
	else if (GIT_MODE_TYPE(omode) != GIT_MODE_TYPE(nmode)) {
		if (diff_delta__from_one(diff, GIT_DELTA_DELETED, oitem) < 0 ||
			diff_delta__from_one(diff, GIT_DELTA_ADDED, nitem) < 0)
			return -1;
		return 0;
	}

	/* if oids and modes match, then file is unmodified */
	else if (git_oid_cmp(&oitem->oid, &nitem->oid) == 0 &&
			 omode == nmode)
		status = GIT_DELTA_UNMODIFIED;

	/* if we have a workdir item with an unknown oid, check deeper */
	else if (git_oid_iszero(&nitem->oid) && new_iter->type == GIT_ITERATOR_WORKDIR) {
		/* TODO: add check against index file st_mtime to avoid racy-git */

		/* if they files look exactly alike, then we'll assume the same */
		if (oitem->file_size == nitem->file_size &&
			(!(diff->diffcaps & GIT_DIFFCAPS_TRUST_CTIME) ||
			 (oitem->ctime.seconds == nitem->ctime.seconds)) &&
			oitem->mtime.seconds == nitem->mtime.seconds &&
			(!(diff->diffcaps & GIT_DIFFCAPS_USE_DEV) ||
			 (oitem->dev == nitem->dev)) &&
			oitem->ino == nitem->ino &&
			oitem->uid == nitem->uid &&
			oitem->gid == nitem->gid)
			status = GIT_DELTA_UNMODIFIED;

		else if (S_ISGITLINK(nmode)) {
			git_submodule *sub;

			if ((diff->opts.flags & GIT_DIFF_IGNORE_SUBMODULES) != 0)
				status = GIT_DELTA_UNMODIFIED;
			else if (git_submodule_lookup(&sub, diff->repo, nitem->path) < 0)
				return -1;
			else if (sub->ignore == GIT_SUBMODULE_IGNORE_ALL)
				status = GIT_DELTA_UNMODIFIED;
			else {
				/* TODO: support other GIT_SUBMODULE_IGNORE values */
				status = GIT_DELTA_UNMODIFIED;
			}
		}

		/* TODO: check git attributes so we will not have to read the file
		 * in if it is marked binary.
		 */

		else if (oid_for_workdir_item(diff->repo, nitem, &noid) < 0)
			return -1;

		else if (git_oid_cmp(&oitem->oid, &noid) == 0 &&
				 omode == nmode)
			status = GIT_DELTA_UNMODIFIED;

		/* store calculated oid so we don't have to recalc later */
		use_noid = &noid;
	}

	return diff_delta__from_two(diff, status, oitem, nitem, use_noid);
}

static int diff_from_iterators(
	git_repository *repo,
	const git_diff_options *opts, /**< can be NULL for defaults */
	git_iterator *old_iter,
	git_iterator *new_iter,
	git_diff_list **diff_ptr)
{
	const git_index_entry *oitem, *nitem;
	git_buf ignore_prefix = GIT_BUF_INIT;
	git_diff_list *diff = git_diff_list_alloc(repo, opts);
	if (!diff)
		goto fail;

	diff->old_src = old_iter->type;
	diff->new_src = new_iter->type;

	if (git_iterator_current(old_iter, &oitem) < 0 ||
		git_iterator_current(new_iter, &nitem) < 0)
		goto fail;

	/* run iterators building diffs */
	while (oitem || nitem) {

		/* create DELETED records for old items not matched in new */
		if (oitem && (!nitem || strcmp(oitem->path, nitem->path) < 0)) {
			if (diff_delta__from_one(diff, GIT_DELTA_DELETED, oitem) < 0 ||
				git_iterator_advance(old_iter, &oitem) < 0)
				goto fail;
		}

		/* create ADDED, TRACKED, or IGNORED records for new items not
		 * matched in old (and/or descend into directories as needed)
		 */
		else if (nitem && (!oitem || strcmp(oitem->path, nitem->path) > 0)) {
			git_delta_t delta_type = GIT_DELTA_UNTRACKED;

			/* check if contained in ignored parent directory */
			if (git_buf_len(&ignore_prefix) &&
				git__prefixcmp(nitem->path, git_buf_cstr(&ignore_prefix)) == 0)
				delta_type = GIT_DELTA_IGNORED;

			if (S_ISDIR(nitem->mode)) {
				/* recurse into directory only if there are tracked items in
				 * it or if the user requested the contents of untracked
				 * directories and it is not under an ignored directory.
				 */
				if ((oitem && git__prefixcmp(oitem->path, nitem->path) == 0) ||
					(delta_type == GIT_DELTA_UNTRACKED &&
					 (diff->opts.flags & GIT_DIFF_RECURSE_UNTRACKED_DIRS) != 0))
				{
					/* if this directory is ignored, remember it as the
					 * "ignore_prefix" for processing contained items
					 */
					if (delta_type == GIT_DELTA_UNTRACKED &&
						git_iterator_current_is_ignored(new_iter))
						git_buf_sets(&ignore_prefix, nitem->path);

					if (git_iterator_advance_into_directory(new_iter, &nitem) < 0)
						goto fail;

					continue;
				}
			}

			/* In core git, the next two "else if" clauses are effectively
			 * reversed -- i.e. when an untracked file contained in an
			 * ignored directory is individually ignored, it shows up as an
			 * ignored file in the diff list, even though other untracked
			 * files in the same directory are skipped completely.
			 *
			 * To me, this is odd.  If the directory is ignored and the file
			 * is untracked, we should skip it consistently, regardless of
			 * whether it happens to match a pattern in the ignore file.
			 *
			 * To match the core git behavior, just reverse the following
			 * two "else if" cases so that individual file ignores are
			 * checked before container directory exclusions are used to
			 * skip the file.
			 */
			else if (delta_type == GIT_DELTA_IGNORED) {
				if (git_iterator_advance(new_iter, &nitem) < 0)
					goto fail;
				continue; /* ignored parent directory, so skip completely */
			}

			else if (git_iterator_current_is_ignored(new_iter))
				delta_type = GIT_DELTA_IGNORED;

			else if (new_iter->type != GIT_ITERATOR_WORKDIR)
				delta_type = GIT_DELTA_ADDED;

			if (diff_delta__from_one(diff, delta_type, nitem) < 0 ||
				git_iterator_advance(new_iter, &nitem) < 0)
				goto fail;
		}

		/* otherwise item paths match, so create MODIFIED record
		 * (or ADDED and DELETED pair if type changed)
		 */
		else {
			assert(oitem && nitem && strcmp(oitem->path, nitem->path) == 0);

			if (maybe_modified(old_iter, oitem, new_iter, nitem, diff) < 0 ||
				git_iterator_advance(old_iter, &oitem) < 0 ||
				git_iterator_advance(new_iter, &nitem) < 0)
				goto fail;
		}
	}

	git_iterator_free(old_iter);
	git_iterator_free(new_iter);
	git_buf_free(&ignore_prefix);

	*diff_ptr = diff;
	return 0;

fail:
	git_iterator_free(old_iter);
	git_iterator_free(new_iter);
	git_buf_free(&ignore_prefix);

	git_diff_list_free(diff);
	*diff_ptr = NULL;
	return -1;
}


int git_diff_tree_to_tree(
	git_repository *repo,
	const git_diff_options *opts, /**< can be NULL for defaults */
	git_tree *old_tree,
	git_tree *new_tree,
	git_diff_list **diff)
{
	git_iterator *a = NULL, *b = NULL;
	char *prefix = opts ? diff_prefix_from_pathspec(&opts->pathspec) : NULL;

	assert(repo && old_tree && new_tree && diff);

	if (git_iterator_for_tree_range(&a, repo, old_tree, prefix, prefix) < 0 ||
		git_iterator_for_tree_range(&b, repo, new_tree, prefix, prefix) < 0)
		return -1;

	git__free(prefix);

	return diff_from_iterators(repo, opts, a, b, diff);
}

int git_diff_index_to_tree(
	git_repository *repo,
	const git_diff_options *opts,
	git_tree *old_tree,
	git_diff_list **diff)
{
	git_iterator *a = NULL, *b = NULL;
	char *prefix = opts ? diff_prefix_from_pathspec(&opts->pathspec) : NULL;

	assert(repo && diff);

	if (git_iterator_for_tree_range(&a, repo, old_tree, prefix, prefix) < 0 ||
		git_iterator_for_index_range(&b, repo, prefix, prefix) < 0)
		return -1;

	git__free(prefix);

	return diff_from_iterators(repo, opts, a, b, diff);
}

int git_diff_workdir_to_index(
	git_repository *repo,
	const git_diff_options *opts,
	git_diff_list **diff)
{
	git_iterator *a = NULL, *b = NULL;
	char *prefix = opts ? diff_prefix_from_pathspec(&opts->pathspec) : NULL;

	assert(repo && diff);

	if (git_iterator_for_index_range(&a, repo, prefix, prefix) < 0 ||
		git_iterator_for_workdir_range(&b, repo, prefix, prefix) < 0)
		return -1;

	git__free(prefix);

	return diff_from_iterators(repo, opts, a, b, diff);
}


int git_diff_workdir_to_tree(
	git_repository *repo,
	const git_diff_options *opts,
	git_tree *old_tree,
	git_diff_list **diff)
{
	git_iterator *a = NULL, *b = NULL;
	char *prefix = opts ? diff_prefix_from_pathspec(&opts->pathspec) : NULL;

	assert(repo && old_tree && diff);

	if (git_iterator_for_tree_range(&a, repo, old_tree, prefix, prefix) < 0 ||
		git_iterator_for_workdir_range(&b, repo, prefix, prefix) < 0)
		return -1;

	git__free(prefix);

	return diff_from_iterators(repo, opts, a, b, diff);
}

int git_diff_merge(
	git_diff_list *onto,
	const git_diff_list *from)
{
	int error = 0;
	git_pool onto_pool;
	git_vector onto_new;
	git_diff_delta *delta;
	unsigned int i, j;

	assert(onto && from);

	if (!from->deltas.length)
		return 0;

	if (git_vector_init(&onto_new, onto->deltas.length, diff_delta__cmp) < 0 ||
		git_pool_init(&onto_pool, 1, 0) < 0)
		return -1;

	for (i = 0, j = 0; i < onto->deltas.length || j < from->deltas.length; ) {
		git_diff_delta *o = GIT_VECTOR_GET(&onto->deltas, i);
		const git_diff_delta *f = GIT_VECTOR_GET(&from->deltas, j);
		int cmp = !f ? -1 : !o ? 1 : strcmp(o->old_file.path, f->old_file.path);

		if (cmp < 0) {
			delta = diff_delta__dup(o, &onto_pool);
			i++;
		} else if (cmp > 0) {
			delta = diff_delta__dup(f, &onto_pool);
			j++;
		} else {
			delta = diff_delta__merge_like_cgit(o, f, &onto_pool);
			i++;
			j++;
		}

		if ((error = !delta ? -1 : git_vector_insert(&onto_new, delta)) < 0)
			break;
	}

	if (!error) {
		git_vector_swap(&onto->deltas, &onto_new);
		git_pool_swap(&onto->pool, &onto_pool);
		onto->new_src = from->new_src;
	}

	git_vector_foreach(&onto_new, i, delta)
		git__free(delta);
	git_vector_free(&onto_new);
	git_pool_clear(&onto_pool);

	return error;
}

