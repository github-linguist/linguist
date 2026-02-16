/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "diff_tform.h"

#include "git2/config.h"
#include "git2/blob.h"
#include "git2/sys/hashsig.h"

#include "diff.h"
#include "diff_generate.h"
#include "fs_path.h"
#include "futils.h"
#include "config.h"

git_diff_delta *git_diff__delta_dup(
	const git_diff_delta *d, git_pool *pool)
{
	git_diff_delta *delta = git__malloc(sizeof(git_diff_delta));
	if (!delta)
		return NULL;

	memcpy(delta, d, sizeof(git_diff_delta));
	GIT_DIFF_FLAG__CLEAR_INTERNAL(delta->flags);

	if (d->old_file.path != NULL) {
		delta->old_file.path = git_pool_strdup(pool, d->old_file.path);
		if (delta->old_file.path == NULL)
			goto fail;
	}

	if (d->new_file.path != d->old_file.path && d->new_file.path != NULL) {
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

git_diff_delta *git_diff__merge_like_cgit(
	const git_diff_delta *a,
	const git_diff_delta *b,
	git_pool *pool)
{
	git_diff_delta *dup;

	/* Emulate C git for merging two diffs (a la 'git diff <sha>').
	 *
	 * When C git does a diff between the work dir and a tree, it actually
	 * diffs with the index but uses the workdir contents.  This emulates
	 * those choices so we can emulate the type of diff.
	 *
	 * We have three file descriptions here, let's call them:
	 *  f1 = a->old_file
	 *  f2 = a->new_file AND b->old_file
	 *  f3 = b->new_file
	 */

	/* If one of the diffs is a conflict, just dup it */
	if (b->status == GIT_DELTA_CONFLICTED)
		return git_diff__delta_dup(b, pool);
	if (a->status == GIT_DELTA_CONFLICTED)
		return git_diff__delta_dup(a, pool);

	/* if f2 == f3 or f2 is deleted, then just dup the 'a' diff */
	if (b->status == GIT_DELTA_UNMODIFIED || a->status == GIT_DELTA_DELETED)
		return git_diff__delta_dup(a, pool);

	/* otherwise, base this diff on the 'b' diff */
	if ((dup = git_diff__delta_dup(b, pool)) == NULL)
		return NULL;

	/* If 'a' status is uninteresting, then we're done */
	if (a->status == GIT_DELTA_UNMODIFIED ||
		a->status == GIT_DELTA_UNTRACKED ||
		a->status == GIT_DELTA_UNREADABLE)
		return dup;

	GIT_ASSERT_WITH_RETVAL(b->status != GIT_DELTA_UNMODIFIED, NULL);

	/* A cgit exception is that the diff of a file that is only in the
	 * index (i.e. not in HEAD nor workdir) is given as empty.
	 */
	if (dup->status == GIT_DELTA_DELETED) {
		if (a->status == GIT_DELTA_ADDED) {
			dup->status = GIT_DELTA_UNMODIFIED;
			dup->nfiles = 2;
		}
		/* else don't overwrite DELETE status */
	} else {
		dup->status = a->status;
		dup->nfiles = a->nfiles;
	}

	git_oid_cpy(&dup->old_file.id, &a->old_file.id);
	dup->old_file.mode  = a->old_file.mode;
	dup->old_file.size  = a->old_file.size;
	dup->old_file.flags = a->old_file.flags;

	return dup;
}

int git_diff__merge(
	git_diff *onto, const git_diff *from, git_diff__merge_cb cb)
{
	int error = 0;
	git_pool onto_pool;
	git_vector onto_new;
	git_diff_delta *delta;
	bool ignore_case, reversed;
	unsigned int i, j;

	GIT_ASSERT_ARG(onto);
	GIT_ASSERT_ARG(from);

	if (!from->deltas.length)
		return 0;

	ignore_case = ((onto->opts.flags & GIT_DIFF_IGNORE_CASE) != 0);
	reversed    = ((onto->opts.flags & GIT_DIFF_REVERSE) != 0);

	if (ignore_case != ((from->opts.flags & GIT_DIFF_IGNORE_CASE) != 0) ||
		reversed    != ((from->opts.flags & GIT_DIFF_REVERSE) != 0)) {
		git_error_set(GIT_ERROR_INVALID,
			"attempt to merge diffs created with conflicting options");
		return -1;
	}

	if (git_vector_init(&onto_new, onto->deltas.length, git_diff_delta__cmp) < 0 ||
	    git_pool_init(&onto_pool, 1) < 0)
		return -1;

	for (i = 0, j = 0; i < onto->deltas.length || j < from->deltas.length; ) {
		git_diff_delta *o = GIT_VECTOR_GET(&onto->deltas, i);
		const git_diff_delta *f = GIT_VECTOR_GET(&from->deltas, j);
		int cmp = !f ? -1 : !o ? 1 :
			STRCMP_CASESELECT(ignore_case, o->old_file.path, f->old_file.path);

		if (cmp < 0) {
			delta = git_diff__delta_dup(o, &onto_pool);
			i++;
		} else if (cmp > 0) {
			delta = git_diff__delta_dup(f, &onto_pool);
			j++;
		} else {
			const git_diff_delta *left = reversed ? f : o;
			const git_diff_delta *right = reversed ? o : f;

			delta = cb(left, right, &onto_pool);
			i++;
			j++;
		}

		/* the ignore rules for the target may not match the source
		 * or the result of a merged delta could be skippable...
		 */
		if (delta && git_diff_delta__should_skip(&onto->opts, delta)) {
			git__free(delta);
			continue;
		}

		if ((error = !delta ? -1 : git_vector_insert(&onto_new, delta)) < 0)
			break;
	}

	if (!error) {
		git_vector_swap(&onto->deltas, &onto_new);
		git_pool_swap(&onto->pool, &onto_pool);

		if ((onto->opts.flags & GIT_DIFF_REVERSE) != 0)
			onto->old_src = from->old_src;
		else
			onto->new_src = from->new_src;

		/* prefix strings also come from old pool, so recreate those.*/
		onto->opts.old_prefix =
			git_pool_strdup_safe(&onto->pool, onto->opts.old_prefix);
		onto->opts.new_prefix =
			git_pool_strdup_safe(&onto->pool, onto->opts.new_prefix);
	}

	git_vector_dispose_deep(&onto_new);
	git_pool_clear(&onto_pool);

	return error;
}

int git_diff_merge(git_diff *onto, const git_diff *from)
{
	return git_diff__merge(onto, from, git_diff__merge_like_cgit);
}

int git_diff_find_similar__hashsig_for_file(
	void **out, const git_diff_file *f, const char *path, void *p)
{
	git_hashsig_option_t opt = (git_hashsig_option_t)(intptr_t)p;

	GIT_UNUSED(f);
	return git_hashsig_create_fromfile((git_hashsig **)out, path, opt);
}

int git_diff_find_similar__hashsig_for_buf(
	void **out, const git_diff_file *f, const char *buf, size_t len, void *p)
{
	git_hashsig_option_t opt = (git_hashsig_option_t)(intptr_t)p;

	GIT_UNUSED(f);
	return git_hashsig_create((git_hashsig **)out, buf, len, opt);
}

void git_diff_find_similar__hashsig_free(void *sig, void *payload)
{
	GIT_UNUSED(payload);
	git_hashsig_free(sig);
}

int git_diff_find_similar__calc_similarity(
	int *score, void *siga, void *sigb, void *payload)
{
	int error;

	GIT_UNUSED(payload);
	error = git_hashsig_compare(siga, sigb);
	if (error < 0)
		return error;

	*score = error;
	return 0;
}

#define DEFAULT_THRESHOLD 50
#define DEFAULT_BREAK_REWRITE_THRESHOLD 60
#define DEFAULT_RENAME_LIMIT 1000

static int normalize_find_opts(
	git_diff *diff,
	git_diff_find_options *opts,
	const git_diff_find_options *given)
{
	git_config *cfg = NULL;
	git_hashsig_option_t hashsig_opts;

	GIT_ERROR_CHECK_VERSION(given, GIT_DIFF_FIND_OPTIONS_VERSION, "git_diff_find_options");

	if (diff->repo != NULL &&
		git_repository_config__weakptr(&cfg, diff->repo) < 0)
		return -1;

	if (given)
		memcpy(opts, given, sizeof(*opts));

	if (!given ||
		 (given->flags & GIT_DIFF_FIND_ALL) == GIT_DIFF_FIND_BY_CONFIG)
	{
		if (cfg) {
			char *rule =
				git_config__get_string_force(cfg, "diff.renames", "true");
			int boolval;

			if (!git__parse_bool(&boolval, rule) && !boolval)
				/* don't set FIND_RENAMES if bool value is false */;
			else if (!strcasecmp(rule, "copies") || !strcasecmp(rule, "copy"))
				opts->flags |= GIT_DIFF_FIND_RENAMES | GIT_DIFF_FIND_COPIES;
			else
				opts->flags |= GIT_DIFF_FIND_RENAMES;

			git__free(rule);
		} else {
			/* set default flag */
			opts->flags |= GIT_DIFF_FIND_RENAMES;
		}
	}

	/* some flags imply others */

	if (opts->flags & GIT_DIFF_FIND_EXACT_MATCH_ONLY) {
		/* if we are only looking for exact matches, then don't turn
		 * MODIFIED items into ADD/DELETE pairs because it's too picky
		 */
		opts->flags &= ~(GIT_DIFF_FIND_REWRITES | GIT_DIFF_BREAK_REWRITES);

		/* similarly, don't look for self-rewrites to split */
		opts->flags &= ~GIT_DIFF_FIND_RENAMES_FROM_REWRITES;
	}

	if (opts->flags & GIT_DIFF_FIND_RENAMES_FROM_REWRITES)
		opts->flags |= GIT_DIFF_FIND_RENAMES;

	if (opts->flags & GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED)
		opts->flags |= GIT_DIFF_FIND_COPIES;

	if (opts->flags & GIT_DIFF_BREAK_REWRITES)
		opts->flags |= GIT_DIFF_FIND_REWRITES;

#define USE_DEFAULT(X) ((X) == 0 || (X) > 100)

	if (USE_DEFAULT(opts->rename_threshold))
		opts->rename_threshold = DEFAULT_THRESHOLD;

	if (USE_DEFAULT(opts->rename_from_rewrite_threshold))
		opts->rename_from_rewrite_threshold = DEFAULT_THRESHOLD;

	if (USE_DEFAULT(opts->copy_threshold))
		opts->copy_threshold = DEFAULT_THRESHOLD;

	if (USE_DEFAULT(opts->break_rewrite_threshold))
		opts->break_rewrite_threshold = DEFAULT_BREAK_REWRITE_THRESHOLD;

#undef USE_DEFAULT

	if (!opts->rename_limit) {
		if (cfg) {
			opts->rename_limit = git_config__get_int_force(
				cfg, "diff.renamelimit", DEFAULT_RENAME_LIMIT);
		}

		if (opts->rename_limit <= 0)
			opts->rename_limit = DEFAULT_RENAME_LIMIT;
	}

	/* assign the internal metric with whitespace flag as payload */
	if (!opts->metric) {
		opts->metric = git__malloc(sizeof(git_diff_similarity_metric));
		GIT_ERROR_CHECK_ALLOC(opts->metric);

		opts->metric->file_signature = git_diff_find_similar__hashsig_for_file;
		opts->metric->buffer_signature = git_diff_find_similar__hashsig_for_buf;
		opts->metric->free_signature = git_diff_find_similar__hashsig_free;
		opts->metric->similarity = git_diff_find_similar__calc_similarity;

		if (opts->flags & GIT_DIFF_FIND_IGNORE_WHITESPACE)
			hashsig_opts = GIT_HASHSIG_IGNORE_WHITESPACE;
		else if (opts->flags & GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE)
			hashsig_opts = GIT_HASHSIG_NORMAL;
		else
			hashsig_opts = GIT_HASHSIG_SMART_WHITESPACE;
		hashsig_opts |= GIT_HASHSIG_ALLOW_SMALL_FILES;
		opts->metric->payload = (void *)hashsig_opts;
	}

	return 0;
}

static int insert_delete_side_of_split(
	git_diff *diff, git_vector *onto, const git_diff_delta *delta)
{
	/* make new record for DELETED side of split */
	git_diff_delta *deleted = git_diff__delta_dup(delta, &diff->pool);
	GIT_ERROR_CHECK_ALLOC(deleted);

	deleted->status = GIT_DELTA_DELETED;
	deleted->nfiles = 1;
	memset(&deleted->new_file, 0, sizeof(deleted->new_file));
	deleted->new_file.path = deleted->old_file.path;
	deleted->new_file.flags |= GIT_DIFF_FLAG_VALID_ID;
	git_oid_clear(&deleted->new_file.id, diff->opts.oid_type);

	return git_vector_insert(onto, deleted);
}

static int apply_splits_and_deletes(
	git_diff *diff, size_t expected_size, bool actually_split)
{
	git_vector onto = GIT_VECTOR_INIT;
	size_t i;
	git_diff_delta *delta;

	if (git_vector_init(&onto, expected_size, diff->deltas._cmp) < 0)
		return -1;

	/* build new delta list without TO_DELETE and splitting TO_SPLIT */
	git_vector_foreach(&diff->deltas, i, delta) {
		if ((delta->flags & GIT_DIFF_FLAG__TO_DELETE) != 0)
			continue;

		if ((delta->flags & GIT_DIFF_FLAG__TO_SPLIT) != 0 && actually_split) {
			delta->similarity = 0;

			if (insert_delete_side_of_split(diff, &onto, delta) < 0)
				goto on_error;

			if (diff->new_src == GIT_ITERATOR_WORKDIR)
				delta->status = GIT_DELTA_UNTRACKED;
			else
				delta->status = GIT_DELTA_ADDED;
			delta->nfiles = 1;
			memset(&delta->old_file, 0, sizeof(delta->old_file));
			delta->old_file.path = delta->new_file.path;
			delta->old_file.flags |= GIT_DIFF_FLAG_VALID_ID;
			git_oid_clear(&delta->old_file.id, diff->opts.oid_type);
		}

		/* clean up delta before inserting into new list */
		GIT_DIFF_FLAG__CLEAR_INTERNAL(delta->flags);

		if (delta->status != GIT_DELTA_COPIED &&
			delta->status != GIT_DELTA_RENAMED &&
			(delta->status != GIT_DELTA_MODIFIED || actually_split))
			delta->similarity = 0;

		/* insert into new list */
		if (git_vector_insert(&onto, delta) < 0)
			goto on_error;
	}

	/* cannot return an error past this point */

	/* free deltas from old list that didn't make it to the new one */
	git_vector_foreach(&diff->deltas, i, delta) {
		if ((delta->flags & GIT_DIFF_FLAG__TO_DELETE) != 0)
			git__free(delta);
	}

	/* swap new delta list into place */
	git_vector_swap(&diff->deltas, &onto);
	git_vector_dispose(&onto);
	git_vector_sort(&diff->deltas);

	return 0;

on_error:
	git_vector_dispose_deep(&onto);

	return -1;
}

GIT_INLINE(git_diff_file *) similarity_get_file(git_diff *diff, size_t idx)
{
	git_diff_delta *delta = git_vector_get(&diff->deltas, idx / 2);
	return (idx & 1) ? &delta->new_file : &delta->old_file;
}

typedef struct {
	size_t idx;
	git_iterator_t src;
	git_repository *repo;
	git_diff_file *file;
	git_str data;
	git_odb_object *odb_obj;
	git_blob *blob;
} similarity_info;

static int similarity_init(
	similarity_info *info, git_diff *diff, size_t file_idx)
{
	info->idx  = file_idx;
	info->src  = (file_idx & 1) ? diff->new_src : diff->old_src;
	info->repo = diff->repo;
	info->file = similarity_get_file(diff, file_idx);
	info->odb_obj = NULL;
	info->blob = NULL;
	git_str_init(&info->data, 0);

	if ((info->file->flags & GIT_DIFF_FLAG_VALID_SIZE) ||
	    info->src == GIT_ITERATOR_WORKDIR)
		return 0;

	return git_diff_file__resolve_zero_size(
		info->file, &info->odb_obj, info->repo);
}

static int similarity_sig(
	similarity_info *info,
	const git_diff_find_options *opts,
	void **cache)
{
	int error = 0;
	git_diff_file *file = info->file;

	if (info->src == GIT_ITERATOR_WORKDIR) {
		if ((error = git_repository_workdir_path(
			&info->data, info->repo, file->path)) < 0)
			return error;

		/* if path is not a regular file, just skip this item */
		if (!git_fs_path_isfile(info->data.ptr))
			return 0;

		/* TODO: apply wd-to-odb filters to file data if necessary */

		error = opts->metric->file_signature(
			&cache[info->idx], info->file,
			info->data.ptr, opts->metric->payload);
	} else {
		/* if we didn't initially know the size, we might have an odb_obj
		 * around from earlier, so convert that, otherwise load the blob now
		 */
		if (info->odb_obj != NULL)
			error = git_object__from_odb_object(
				(git_object **)&info->blob, info->repo,
				info->odb_obj, GIT_OBJECT_BLOB);
		else
			error = git_blob_lookup(&info->blob, info->repo, &file->id);

		if (error < 0) {
			/* if lookup fails, just skip this item in similarity calc */
			git_error_clear();
		} else {
			size_t sz;

			/* index size may not be actual blob size if filtered */
			if (file->size != git_blob_rawsize(info->blob))
				file->size = git_blob_rawsize(info->blob);

			sz = git__is_sizet(file->size) ? (size_t)file->size : (size_t)-1;

			error = opts->metric->buffer_signature(
				&cache[info->idx], info->file,
				git_blob_rawcontent(info->blob), sz, opts->metric->payload);
		}
	}

	return error;
}

static void similarity_unload(similarity_info *info)
{
	if (info->odb_obj)
		git_odb_object_free(info->odb_obj);

	if (info->blob)
		git_blob_free(info->blob);
	else
		git_str_dispose(&info->data);
}

#define FLAG_SET(opts,flag_name) (((opts)->flags & flag_name) != 0)

/* - score < 0 means files cannot be compared
 * - score >= 100 means files are exact match
 * - score == 0 means files are completely different
 */
static int similarity_measure(
	int *score,
	git_diff *diff,
	const git_diff_find_options *opts,
	void **cache,
	size_t a_idx,
	size_t b_idx)
{
	git_diff_file *a_file = similarity_get_file(diff, a_idx);
	git_diff_file *b_file = similarity_get_file(diff, b_idx);
	bool exact_match = FLAG_SET(opts, GIT_DIFF_FIND_EXACT_MATCH_ONLY);
	int error = 0;
	similarity_info a_info, b_info;

	*score = -1;

	/* don't try to compare things that aren't files */
	if (!GIT_MODE_ISBLOB(a_file->mode) || !GIT_MODE_ISBLOB(b_file->mode))
		return 0;

	/* if exact match is requested, force calculation of missing OIDs now */
	if (exact_match) {
		if (git_oid_is_zero(&a_file->id) &&
			diff->old_src == GIT_ITERATOR_WORKDIR &&
			!git_diff__oid_for_file(&a_file->id,
				diff, a_file->path, a_file->mode, a_file->size))
			a_file->flags |= GIT_DIFF_FLAG_VALID_ID;

		if (git_oid_is_zero(&b_file->id) &&
			diff->new_src == GIT_ITERATOR_WORKDIR &&
			!git_diff__oid_for_file(&b_file->id,
				diff, b_file->path, b_file->mode, b_file->size))
			b_file->flags |= GIT_DIFF_FLAG_VALID_ID;
	}

	/* check OID match as a quick test */
	if (git_oid__cmp(&a_file->id, &b_file->id) == 0) {
		*score = 100;
		return 0;
	}

	/* don't calculate signatures if we are doing exact match */
	if (exact_match) {
		*score = 0;
		return 0;
	}

	memset(&a_info, 0, sizeof(a_info));
	memset(&b_info, 0, sizeof(b_info));

	/* set up similarity data (will try to update missing file sizes) */
	if (!cache[a_idx] && (error = similarity_init(&a_info, diff, a_idx)) < 0)
		return error;
	if (!cache[b_idx] && (error = similarity_init(&b_info, diff, b_idx)) < 0)
		goto cleanup;

	/* check if file sizes are nowhere near each other */
	if (a_file->size > 127 &&
		b_file->size > 127 &&
		(a_file->size > (b_file->size << 3) ||
		 b_file->size > (a_file->size << 3)))
		goto cleanup;

	/* update signature cache if needed */
	if (!cache[a_idx]) {
		if ((error = similarity_sig(&a_info, opts, cache)) < 0)
			goto cleanup;
	}
	if (!cache[b_idx]) {
		if ((error = similarity_sig(&b_info, opts, cache)) < 0)
			goto cleanup;
	}

	/* calculate similarity provided that the metric choose to process
	 * both the a and b files (some may not if file is too big, etc).
	 */
	if (cache[a_idx] && cache[b_idx])
		error = opts->metric->similarity(
			score, cache[a_idx], cache[b_idx], opts->metric->payload);

cleanup:
	similarity_unload(&a_info);
	similarity_unload(&b_info);

	return error;
}

static int calc_self_similarity(
	git_diff *diff,
	const git_diff_find_options *opts,
	size_t delta_idx,
	void **cache)
{
	int error, similarity = -1;
	git_diff_delta *delta = GIT_VECTOR_GET(&diff->deltas, delta_idx);

	if ((delta->flags & GIT_DIFF_FLAG__HAS_SELF_SIMILARITY) != 0)
		return 0;

	error = similarity_measure(
		&similarity, diff, opts, cache, 2 * delta_idx, 2 * delta_idx + 1);
	if (error < 0)
		return error;

	if (similarity >= 0) {
		delta->similarity = (uint16_t)similarity;
		delta->flags |= GIT_DIFF_FLAG__HAS_SELF_SIMILARITY;
	}

	return 0;
}

static void handle_non_blob(
	git_diff *diff,
	const git_diff_find_options *opts,
	size_t delta_idx)
{
	git_diff_delta *delta = GIT_VECTOR_GET(&diff->deltas, delta_idx);

	/* skip things that are blobs */
	if (GIT_MODE_ISBLOB(delta->old_file.mode))
		return;

	/* honor "remove unmodified" flag for non-blobs (eg submodules) */
	if (delta->status == GIT_DELTA_UNMODIFIED &&
	    FLAG_SET(opts, GIT_DIFF_FIND_REMOVE_UNMODIFIED))
		delta->flags |= GIT_DIFF_FLAG__TO_DELETE;
}

static bool is_rename_target(
	git_diff *diff,
	const git_diff_find_options *opts,
	size_t delta_idx,
	void **cache)
{
	git_diff_delta *delta = GIT_VECTOR_GET(&diff->deltas, delta_idx);

	/* skip things that aren't plain blobs */
	if (!GIT_MODE_ISBLOB(delta->new_file.mode))
		return false;

	/* only consider ADDED, RENAMED, COPIED, and split MODIFIED as
	 * targets; maybe include UNTRACKED if requested.
	 */
	switch (delta->status) {
	case GIT_DELTA_UNMODIFIED:
	case GIT_DELTA_DELETED:
	case GIT_DELTA_IGNORED:
	case GIT_DELTA_CONFLICTED:
		return false;

	case GIT_DELTA_MODIFIED:
		if (!FLAG_SET(opts, GIT_DIFF_FIND_REWRITES) &&
			!FLAG_SET(opts, GIT_DIFF_FIND_RENAMES_FROM_REWRITES))
			return false;

		if (calc_self_similarity(diff, opts, delta_idx, cache) < 0)
			return false;

		if (FLAG_SET(opts, GIT_DIFF_BREAK_REWRITES) &&
			delta->similarity < opts->break_rewrite_threshold) {
			delta->flags |= GIT_DIFF_FLAG__TO_SPLIT;
			break;
		}
		if (FLAG_SET(opts, GIT_DIFF_FIND_RENAMES_FROM_REWRITES) &&
			delta->similarity < opts->rename_from_rewrite_threshold) {
			delta->flags |= GIT_DIFF_FLAG__TO_SPLIT;
			break;
		}

		return false;

	case GIT_DELTA_UNTRACKED:
		if (!FLAG_SET(opts, GIT_DIFF_FIND_FOR_UNTRACKED))
			return false;
		break;

	default: /* all other status values should be checked */
		break;
	}

	delta->flags |= GIT_DIFF_FLAG__IS_RENAME_TARGET;
	return true;
}

static bool is_rename_source(
	git_diff *diff,
	const git_diff_find_options *opts,
	size_t delta_idx,
	void **cache)
{
	git_diff_delta *delta = GIT_VECTOR_GET(&diff->deltas, delta_idx);

	/* skip things that aren't blobs */
	if (!GIT_MODE_ISBLOB(delta->old_file.mode))
		return false;

	switch (delta->status) {
	case GIT_DELTA_ADDED:
	case GIT_DELTA_UNTRACKED:
	case GIT_DELTA_UNREADABLE:
	case GIT_DELTA_IGNORED:
	case GIT_DELTA_CONFLICTED:
		return false;

	case GIT_DELTA_DELETED:
	case GIT_DELTA_TYPECHANGE:
		break;

	case GIT_DELTA_UNMODIFIED:
		if (!FLAG_SET(opts, GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED))
			return false;
		if (FLAG_SET(opts, GIT_DIFF_FIND_REMOVE_UNMODIFIED))
			delta->flags |= GIT_DIFF_FLAG__TO_DELETE;
		break;

	default: /* MODIFIED, RENAMED, COPIED */
		/* if we're finding copies, this could be a source */
		if (FLAG_SET(opts, GIT_DIFF_FIND_COPIES))
			break;

		/* otherwise, this is only a source if we can split it */
		if (!FLAG_SET(opts, GIT_DIFF_FIND_REWRITES) &&
			!FLAG_SET(opts, GIT_DIFF_FIND_RENAMES_FROM_REWRITES))
			return false;

		if (calc_self_similarity(diff, opts, delta_idx, cache) < 0)
			return false;

		if (FLAG_SET(opts, GIT_DIFF_BREAK_REWRITES) &&
			delta->similarity < opts->break_rewrite_threshold) {
			delta->flags |= GIT_DIFF_FLAG__TO_SPLIT;
			break;
		}

		if (FLAG_SET(opts, GIT_DIFF_FIND_RENAMES_FROM_REWRITES) &&
			delta->similarity < opts->rename_from_rewrite_threshold)
			break;

		return false;
	}

	delta->flags |= GIT_DIFF_FLAG__IS_RENAME_SOURCE;
	return true;
}

GIT_INLINE(bool) delta_is_split(git_diff_delta *delta)
{
	return (delta->status == GIT_DELTA_TYPECHANGE ||
			(delta->flags & GIT_DIFF_FLAG__TO_SPLIT) != 0);
}

GIT_INLINE(bool) delta_is_new_only(git_diff_delta *delta)
{
	return (delta->status == GIT_DELTA_ADDED ||
			delta->status == GIT_DELTA_UNTRACKED ||
			delta->status == GIT_DELTA_UNREADABLE ||
			delta->status == GIT_DELTA_IGNORED);
}

GIT_INLINE(void) delta_make_rename(
	git_diff_delta *to, const git_diff_delta *from, uint16_t similarity)
{
	to->status     = GIT_DELTA_RENAMED;
	to->similarity = similarity;
	to->nfiles     = 2;
	memcpy(&to->old_file, &from->old_file, sizeof(to->old_file));
	to->flags &= ~GIT_DIFF_FLAG__TO_SPLIT;
}

typedef struct {
	size_t   idx;
	uint16_t similarity;
} diff_find_match;

int git_diff_find_similar(
	git_diff *diff,
	const git_diff_find_options *given_opts)
{
	size_t s, t;
	int error = 0, result;
	uint16_t similarity;
	git_diff_delta *src, *tgt;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	size_t num_deltas, num_srcs = 0, num_tgts = 0;
	size_t tried_srcs = 0, tried_tgts = 0;
	size_t num_rewrites = 0, num_updates = 0, num_bumped = 0,
	       num_to_delete = 0;
	size_t sigcache_size;
	void **sigcache = NULL; /* cache of similarity metric file signatures */
	diff_find_match *tgt2src = NULL;
	diff_find_match *src2tgt = NULL;
	diff_find_match *tgt2src_copy = NULL;
	diff_find_match *best_match;
	git_diff_file swap;

	GIT_ASSERT_ARG(diff);

	if ((error = normalize_find_opts(diff, &opts, given_opts)) < 0)
		return error;

	num_deltas = diff->deltas.length;

	/* TODO: maybe abort if deltas.length > rename_limit ??? */
	if (!num_deltas || !git__is_uint32(num_deltas))
		goto cleanup;

	/* No flags set; nothing to do */
	if ((opts.flags & GIT_DIFF_FIND_ALL) == 0)
		goto cleanup;

	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&sigcache_size, num_deltas, 2);
	sigcache = git__calloc(sigcache_size, sizeof(void *));
	GIT_ERROR_CHECK_ALLOC(sigcache);

	/* Label rename sources and targets
	 *
	 * This will also set self-similarity scores for MODIFIED files and
	 * mark them for splitting if break-rewrites is enabled
	 */
	git_vector_foreach(&diff->deltas, t, tgt) {
		handle_non_blob(diff, &opts, t);

		if (is_rename_source(diff, &opts, t, sigcache))
			++num_srcs;

		if (is_rename_target(diff, &opts, t, sigcache))
			++num_tgts;

		if ((tgt->flags & GIT_DIFF_FLAG__TO_SPLIT) != 0)
			num_rewrites++;

		if ((tgt->flags & GIT_DIFF_FLAG__TO_DELETE) != 0)
			num_to_delete++;
	}

	/* If there are no candidate srcs or tgts, no need to find matches */
	if (!num_srcs || !num_tgts)
		goto split_and_delete;

	src2tgt = git__calloc(num_deltas, sizeof(diff_find_match));
	GIT_ERROR_CHECK_ALLOC(src2tgt);
	tgt2src = git__calloc(num_deltas, sizeof(diff_find_match));
	GIT_ERROR_CHECK_ALLOC(tgt2src);

	if (FLAG_SET(&opts, GIT_DIFF_FIND_COPIES)) {
		tgt2src_copy = git__calloc(num_deltas, sizeof(diff_find_match));
		GIT_ERROR_CHECK_ALLOC(tgt2src_copy);
	}

	/*
	 * Find best-fit matches for rename / copy candidates
	 */

find_best_matches:
	tried_tgts = num_bumped = 0;

	git_vector_foreach(&diff->deltas, t, tgt) {
		/* skip things that are not rename targets */
		if ((tgt->flags & GIT_DIFF_FLAG__IS_RENAME_TARGET) == 0)
			continue;

		tried_srcs = 0;

		git_vector_foreach(&diff->deltas, s, src) {
			/* skip things that are not rename sources */
			if ((src->flags & GIT_DIFF_FLAG__IS_RENAME_SOURCE) == 0)
				continue;

			/* calculate similarity for this pair and find best match */
			if (s == t)
				result = -1; /* don't measure self-similarity here */
			else if ((error = similarity_measure(
				&result, diff, &opts, sigcache, 2 * s, 2 * t + 1)) < 0)
				goto cleanup;

			if (result < 0)
				continue;
			similarity = (uint16_t)result;

			/* is this a better rename? */
			if (tgt2src[t].similarity < similarity &&
				src2tgt[s].similarity < similarity)
			{
				/* eject old mapping */
				if (src2tgt[s].similarity > 0) {
					tgt2src[src2tgt[s].idx].similarity = 0;
					num_bumped++;
				}
				if (tgt2src[t].similarity > 0) {
					src2tgt[tgt2src[t].idx].similarity = 0;
					num_bumped++;
				}

				/* write new mapping */
				tgt2src[t].idx = s;
				tgt2src[t].similarity = similarity;
				src2tgt[s].idx = t;
				src2tgt[s].similarity = similarity;
			}

			/* keep best absolute match for copies */
			if (tgt2src_copy != NULL &&
				tgt2src_copy[t].similarity < similarity)
			{
				tgt2src_copy[t].idx = s;
				tgt2src_copy[t].similarity = similarity;
			}

			if (++tried_srcs >= num_srcs)
				break;

			/* cap on maximum targets we'll examine (per "tgt" file) */
			if (tried_srcs > opts.rename_limit)
				break;
		}

		if (++tried_tgts >= num_tgts)
			break;
	}

	if (num_bumped > 0) /* try again if we bumped some items */
		goto find_best_matches;

	/*
	 * Rewrite the diffs with renames / copies
	 */

	git_vector_foreach(&diff->deltas, t, tgt) {
		/* skip things that are not rename targets */
		if ((tgt->flags & GIT_DIFF_FLAG__IS_RENAME_TARGET) == 0)
			continue;

		/* check if this delta was the target of a similarity */
		if (tgt2src[t].similarity)
			best_match = &tgt2src[t];
		else if (tgt2src_copy && tgt2src_copy[t].similarity)
			best_match = &tgt2src_copy[t];
		else
			continue;

		s = best_match->idx;
		src = GIT_VECTOR_GET(&diff->deltas, s);

		/* possible scenarios:
		 * 1. from DELETE to ADD/UNTRACK/IGNORE = RENAME
		 * 2. from DELETE to SPLIT/TYPECHANGE = RENAME + DELETE
		 * 3. from SPLIT/TYPECHANGE to ADD/UNTRACK/IGNORE = ADD + RENAME
		 * 4. from SPLIT/TYPECHANGE to SPLIT/TYPECHANGE = RENAME + SPLIT
		 * 5. from OTHER to ADD/UNTRACK/IGNORE = OTHER + COPY
		 */

		if (src->status == GIT_DELTA_DELETED) {

			if (delta_is_new_only(tgt)) {

				if (best_match->similarity < opts.rename_threshold)
					continue;

				delta_make_rename(tgt, src, best_match->similarity);

				src->flags |= GIT_DIFF_FLAG__TO_DELETE;
				num_rewrites++;
			} else {
				GIT_ASSERT(delta_is_split(tgt));

				if (best_match->similarity < opts.rename_from_rewrite_threshold)
					continue;

				memcpy(&swap, &tgt->old_file, sizeof(swap));

				delta_make_rename(tgt, src, best_match->similarity);
				num_rewrites--;

				GIT_ASSERT(src->status == GIT_DELTA_DELETED);
				memcpy(&src->old_file, &swap, sizeof(src->old_file));
				memset(&src->new_file, 0, sizeof(src->new_file));
				src->new_file.path = src->old_file.path;
				src->new_file.flags |= GIT_DIFF_FLAG_VALID_ID;
				git_oid_clear(&src->new_file.id, diff->opts.oid_type);

				num_updates++;

				if (src2tgt[t].similarity > 0 && src2tgt[t].idx > t) {
					/* what used to be at src t is now at src s */
					tgt2src[src2tgt[t].idx].idx = s;
				}
			}
		}

		else if (delta_is_split(src)) {

			if (delta_is_new_only(tgt)) {

				if (best_match->similarity < opts.rename_threshold)
					continue;

				delta_make_rename(tgt, src, best_match->similarity);

				src->status = (diff->new_src == GIT_ITERATOR_WORKDIR) ?
					GIT_DELTA_UNTRACKED : GIT_DELTA_ADDED;
				src->nfiles = 1;
				memset(&src->old_file, 0, sizeof(src->old_file));
				src->old_file.path = src->new_file.path;
				src->old_file.flags |= GIT_DIFF_FLAG_VALID_ID;
				git_oid_clear(&src->old_file.id, diff->opts.oid_type);

				src->flags &= ~GIT_DIFF_FLAG__TO_SPLIT;
				num_rewrites--;

				num_updates++;
			} else {
				GIT_ASSERT(delta_is_split(src));

				if (best_match->similarity < opts.rename_from_rewrite_threshold)
					continue;

				memcpy(&swap, &tgt->old_file, sizeof(swap));

				delta_make_rename(tgt, src, best_match->similarity);
				num_rewrites--;
				num_updates++;

				memcpy(&src->old_file, &swap, sizeof(src->old_file));

				/* if we've just swapped the new element into the correct
				 * place, clear the SPLIT and RENAME_TARGET flags
				 */
				if (tgt2src[s].idx == t &&
					tgt2src[s].similarity >
					opts.rename_from_rewrite_threshold) {
					src->status     = GIT_DELTA_RENAMED;
					src->similarity = tgt2src[s].similarity;
					tgt2src[s].similarity = 0;
					src->flags &= ~(GIT_DIFF_FLAG__TO_SPLIT | GIT_DIFF_FLAG__IS_RENAME_TARGET);
					num_rewrites--;
				}
				/* otherwise, if we just overwrote a source, update mapping */
				else if (src2tgt[t].similarity > 0 && src2tgt[t].idx > t) {
					/* what used to be at src t is now at src s */
					tgt2src[src2tgt[t].idx].idx = s;
				}

				num_updates++;
			}
		}

		else if (FLAG_SET(&opts, GIT_DIFF_FIND_COPIES)) {
			if (tgt2src_copy[t].similarity < opts.copy_threshold)
				continue;

			/* always use best possible source for copy */
			best_match = &tgt2src_copy[t];
			src = GIT_VECTOR_GET(&diff->deltas, best_match->idx);

			if (delta_is_split(tgt)) {
				error = insert_delete_side_of_split(diff, &diff->deltas, tgt);
				if (error < 0)
					goto cleanup;
				num_rewrites--;
			}

			if (!delta_is_split(tgt) && !delta_is_new_only(tgt))
				continue;

			tgt->status     = GIT_DELTA_COPIED;
			tgt->similarity = best_match->similarity;
			tgt->nfiles     = 2;
			memcpy(&tgt->old_file, &src->old_file, sizeof(tgt->old_file));
			tgt->flags &= ~GIT_DIFF_FLAG__TO_SPLIT;

			num_updates++;
		}
	}

split_and_delete:
	/*
	 * Actually split and delete entries as needed
	 */

	if (num_rewrites > 0 || num_updates > 0 || num_to_delete > 0) {
		size_t apply_len = diff->deltas.length -
		                   num_rewrites - num_to_delete;

		error = apply_splits_and_deletes(
			diff, apply_len,
			FLAG_SET(&opts, GIT_DIFF_BREAK_REWRITES) &&
			!FLAG_SET(&opts, GIT_DIFF_BREAK_REWRITES_FOR_RENAMES_ONLY));
	}

cleanup:
	git__free(tgt2src);
	git__free(src2tgt);
	git__free(tgt2src_copy);

	if (sigcache) {
		for (t = 0; t < num_deltas * 2; ++t) {
			if (sigcache[t] != NULL)
				opts.metric->free_signature(sigcache[t], opts.metric->payload);
		}
		git__free(sigcache);
	}

	if (!given_opts || !given_opts->metric)
		git__free(opts.metric);

	return error;
}

#undef FLAG_SET
