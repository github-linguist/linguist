/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "status.h"

#include "git2.h"
#include "futils.h"
#include "hash.h"
#include "vector.h"
#include "tree.h"
#include "git2/status.h"
#include "repository.h"
#include "ignore.h"
#include "index.h"
#include "wildmatch.h"

#include "git2/diff.h"
#include "diff.h"
#include "diff_generate.h"

static unsigned int index_delta2status(const git_diff_delta *head2idx)
{
	git_status_t st = GIT_STATUS_CURRENT;

	switch (head2idx->status) {
	case GIT_DELTA_ADDED:
	case GIT_DELTA_COPIED:
		st = GIT_STATUS_INDEX_NEW;
		break;
	case GIT_DELTA_DELETED:
		st = GIT_STATUS_INDEX_DELETED;
		break;
	case GIT_DELTA_MODIFIED:
		st = GIT_STATUS_INDEX_MODIFIED;
		break;
	case GIT_DELTA_RENAMED:
		st = GIT_STATUS_INDEX_RENAMED;

		if (!git_oid_equal(&head2idx->old_file.id, &head2idx->new_file.id))
			st |= GIT_STATUS_INDEX_MODIFIED;
		break;
	case GIT_DELTA_TYPECHANGE:
		st = GIT_STATUS_INDEX_TYPECHANGE;
		break;
	case GIT_DELTA_CONFLICTED:
		st = GIT_STATUS_CONFLICTED;
		break;
	default:
		break;
	}

	return st;
}

static unsigned int workdir_delta2status(
	git_diff *diff, git_diff_delta *idx2wd)
{
	git_status_t st = GIT_STATUS_CURRENT;

	switch (idx2wd->status) {
	case GIT_DELTA_ADDED:
	case GIT_DELTA_COPIED:
	case GIT_DELTA_UNTRACKED:
		st = GIT_STATUS_WT_NEW;
		break;
	case GIT_DELTA_UNREADABLE:
		st = GIT_STATUS_WT_UNREADABLE;
		break;
	case GIT_DELTA_DELETED:
		st = GIT_STATUS_WT_DELETED;
		break;
	case GIT_DELTA_MODIFIED:
		st = GIT_STATUS_WT_MODIFIED;
		break;
	case GIT_DELTA_IGNORED:
		st = GIT_STATUS_IGNORED;
		break;
	case GIT_DELTA_RENAMED:
		st = GIT_STATUS_WT_RENAMED;

		if (!git_oid_equal(&idx2wd->old_file.id, &idx2wd->new_file.id)) {
			/* if OIDs don't match, we might need to calculate them now to
			 * discern between RENAMED vs RENAMED+MODIFIED
			 */
			if (git_oid_is_zero(&idx2wd->old_file.id) &&
				diff->old_src == GIT_ITERATOR_WORKDIR &&
				!git_diff__oid_for_file(
					&idx2wd->old_file.id, diff, idx2wd->old_file.path,
					idx2wd->old_file.mode, idx2wd->old_file.size))
			idx2wd->old_file.flags |= GIT_DIFF_FLAG_VALID_ID;

			if (git_oid_is_zero(&idx2wd->new_file.id) &&
				diff->new_src == GIT_ITERATOR_WORKDIR &&
				!git_diff__oid_for_file(
					&idx2wd->new_file.id, diff, idx2wd->new_file.path,
					idx2wd->new_file.mode, idx2wd->new_file.size))
				idx2wd->new_file.flags |= GIT_DIFF_FLAG_VALID_ID;

			if (!git_oid_equal(&idx2wd->old_file.id, &idx2wd->new_file.id))
				st |= GIT_STATUS_WT_MODIFIED;
		}
		break;
	case GIT_DELTA_TYPECHANGE:
		st = GIT_STATUS_WT_TYPECHANGE;
		break;
	case GIT_DELTA_CONFLICTED:
		st = GIT_STATUS_CONFLICTED;
		break;
	default:
		break;
	}

	return st;
}

static bool status_is_included(
	git_status_list *status,
	git_diff_delta *head2idx,
	git_diff_delta *idx2wd)
{
	if (!(status->opts.flags & GIT_STATUS_OPT_EXCLUDE_SUBMODULES))
		return 1;

	/* if excluding submodules and this is a submodule everywhere */
	if (head2idx) {
		if (head2idx->status != GIT_DELTA_ADDED &&
			head2idx->old_file.mode != GIT_FILEMODE_COMMIT)
			return 1;
		if (head2idx->status != GIT_DELTA_DELETED &&
			head2idx->new_file.mode != GIT_FILEMODE_COMMIT)
			return 1;
	}
	if (idx2wd) {
		if (idx2wd->status != GIT_DELTA_ADDED &&
			idx2wd->old_file.mode != GIT_FILEMODE_COMMIT)
			return 1;
		if (idx2wd->status != GIT_DELTA_DELETED &&
			idx2wd->new_file.mode != GIT_FILEMODE_COMMIT)
			return 1;
	}

	/* only get here if every valid mode is GIT_FILEMODE_COMMIT */
	return 0;
}

static git_status_t status_compute(
	git_status_list *status,
	git_diff_delta *head2idx,
	git_diff_delta *idx2wd)
{
	git_status_t st = GIT_STATUS_CURRENT;

	if (head2idx)
		st |= index_delta2status(head2idx);

	if (idx2wd)
		st |= workdir_delta2status(status->idx2wd, idx2wd);

	return st;
}

static int status_collect(
	git_diff_delta *head2idx,
	git_diff_delta *idx2wd,
	void *payload)
{
	git_status_list *status = payload;
	git_status_entry *status_entry;

	if (!status_is_included(status, head2idx, idx2wd))
		return 0;

	status_entry = git__malloc(sizeof(git_status_entry));
	GIT_ERROR_CHECK_ALLOC(status_entry);

	status_entry->status = status_compute(status, head2idx, idx2wd);
	status_entry->head_to_index = head2idx;
	status_entry->index_to_workdir = idx2wd;

	return git_vector_insert(&status->paired, status_entry);
}

GIT_INLINE(int) status_entry_cmp_base(
	const void *a,
	const void *b,
	int (*strcomp)(const char *a, const char *b))
{
	const git_status_entry *entry_a = a;
	const git_status_entry *entry_b = b;
	const git_diff_delta *delta_a, *delta_b;

	delta_a = entry_a->index_to_workdir ? entry_a->index_to_workdir :
		entry_a->head_to_index;
	delta_b = entry_b->index_to_workdir ? entry_b->index_to_workdir :
		entry_b->head_to_index;

	if (!delta_a && delta_b)
		return -1;
	if (delta_a && !delta_b)
		return 1;
	if (!delta_a && !delta_b)
		return 0;

	return strcomp(delta_a->new_file.path, delta_b->new_file.path);
}

static int status_entry_icmp(const void *a, const void *b)
{
	return status_entry_cmp_base(a, b, git__strcasecmp);
}

static int status_entry_cmp(const void *a, const void *b)
{
	return status_entry_cmp_base(a, b, git__strcmp);
}

static git_status_list *git_status_list_alloc(git_index *index)
{
	git_status_list *status = NULL;
	int (*entrycmp)(const void *a, const void *b);

	if (!(status = git__calloc(1, sizeof(git_status_list))))
		return NULL;

	entrycmp = index->ignore_case ? status_entry_icmp : status_entry_cmp;

	if (git_vector_init(&status->paired, 0, entrycmp) < 0) {
		git__free(status);
		return NULL;
	}

	return status;
}

static int status_validate_options(const git_status_options *opts)
{
	if (!opts)
		return 0;

	GIT_ERROR_CHECK_VERSION(opts, GIT_STATUS_OPTIONS_VERSION, "git_status_options");

	if (opts->show > GIT_STATUS_SHOW_WORKDIR_ONLY) {
		git_error_set(GIT_ERROR_INVALID, "unknown status 'show' option");
		return -1;
	}

	if ((opts->flags & GIT_STATUS_OPT_NO_REFRESH) != 0 &&
		(opts->flags & GIT_STATUS_OPT_UPDATE_INDEX) != 0) {
		git_error_set(GIT_ERROR_INVALID, "updating index from status "
			"is not allowed when index refresh is disabled");
		return -1;
	}

	return 0;
}

int git_status_list_new(
	git_status_list **out,
	git_repository *repo,
	const git_status_options *opts)
{
	git_index *index = NULL;
	git_status_list *status = NULL;
	git_diff_options diffopt = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopt = GIT_DIFF_FIND_OPTIONS_INIT;
	git_tree *head = NULL;
	git_status_show_t show =
		opts ? opts->show : GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
	int error = 0;
	unsigned int flags = opts ? opts->flags : GIT_STATUS_OPT_DEFAULTS;

	*out = NULL;

	if (status_validate_options(opts) < 0)
		return -1;

	if ((error = git_repository__ensure_not_bare(repo, "status")) < 0 ||
		(error = git_repository_index(&index, repo)) < 0)
		return error;

	if (opts != NULL && opts->baseline != NULL) {
		head = opts->baseline;
	} else {
		/* if there is no HEAD, that's okay - we'll make an empty iterator */
		if ((error = git_repository_head_tree(&head, repo)) < 0) {
			if (error != GIT_ENOTFOUND && error != GIT_EUNBORNBRANCH)
				goto done;
			git_error_clear();
		}
	}

	/* refresh index from disk unless prevented */
	if ((flags & GIT_STATUS_OPT_NO_REFRESH) == 0 &&
		git_index_read_safely(index) < 0)
		git_error_clear();

	status = git_status_list_alloc(index);
	GIT_ERROR_CHECK_ALLOC(status);

	if (opts) {
		memcpy(&status->opts, opts, sizeof(git_status_options));
		memcpy(&diffopt.pathspec, &opts->pathspec, sizeof(diffopt.pathspec));
	}

	diffopt.flags = GIT_DIFF_INCLUDE_TYPECHANGE;
	findopt.flags = GIT_DIFF_FIND_FOR_UNTRACKED;

	if ((flags & GIT_STATUS_OPT_INCLUDE_UNTRACKED) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_INCLUDE_UNTRACKED;
	if ((flags & GIT_STATUS_OPT_INCLUDE_IGNORED) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_INCLUDE_IGNORED;
	if ((flags & GIT_STATUS_OPT_INCLUDE_UNMODIFIED) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_INCLUDE_UNMODIFIED;
	if ((flags & GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_RECURSE_UNTRACKED_DIRS;
	if ((flags & GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_DISABLE_PATHSPEC_MATCH;
	if ((flags & GIT_STATUS_OPT_RECURSE_IGNORED_DIRS) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_RECURSE_IGNORED_DIRS;
	if ((flags & GIT_STATUS_OPT_EXCLUDE_SUBMODULES) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_IGNORE_SUBMODULES;
	if ((flags & GIT_STATUS_OPT_UPDATE_INDEX) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_UPDATE_INDEX;
	if ((flags & GIT_STATUS_OPT_INCLUDE_UNREADABLE) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_INCLUDE_UNREADABLE;
	if ((flags & GIT_STATUS_OPT_INCLUDE_UNREADABLE_AS_UNTRACKED) != 0)
		diffopt.flags = diffopt.flags | GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED;

	if ((flags & GIT_STATUS_OPT_RENAMES_FROM_REWRITES) != 0)
		findopt.flags = findopt.flags |
			GIT_DIFF_FIND_AND_BREAK_REWRITES |
			GIT_DIFF_FIND_RENAMES_FROM_REWRITES |
			GIT_DIFF_BREAK_REWRITES_FOR_RENAMES_ONLY;

	if (opts != NULL && opts->rename_threshold != 0)
		findopt.rename_threshold = opts->rename_threshold;

	if (show != GIT_STATUS_SHOW_WORKDIR_ONLY) {
		if ((error = git_diff_tree_to_index(
				&status->head2idx, repo, head, index, &diffopt)) < 0)
			goto done;

		if ((flags & GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX) != 0 &&
			(error = git_diff_find_similar(status->head2idx, &findopt)) < 0)
			goto done;
	}

	if (show != GIT_STATUS_SHOW_INDEX_ONLY) {
		if ((error = git_diff_index_to_workdir(
				&status->idx2wd, repo, index, &diffopt)) < 0) {
			goto done;
		}

		if ((flags & GIT_STATUS_OPT_RENAMES_INDEX_TO_WORKDIR) != 0 &&
			(error = git_diff_find_similar(status->idx2wd, &findopt)) < 0)
			goto done;
	}

	error = git_diff__paired_foreach(
		status->head2idx, status->idx2wd, status_collect, status);
	if (error < 0)
		goto done;

	if (flags & GIT_STATUS_OPT_SORT_CASE_SENSITIVELY)
		git_vector_set_cmp(&status->paired, status_entry_cmp);
	if (flags & GIT_STATUS_OPT_SORT_CASE_INSENSITIVELY)
		git_vector_set_cmp(&status->paired, status_entry_icmp);

	if ((flags &
		 (GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX |
		  GIT_STATUS_OPT_RENAMES_INDEX_TO_WORKDIR |
		  GIT_STATUS_OPT_SORT_CASE_SENSITIVELY |
		  GIT_STATUS_OPT_SORT_CASE_INSENSITIVELY)) != 0)
		git_vector_sort(&status->paired);

done:
	if (error < 0) {
		git_status_list_free(status);
		status = NULL;
	}

	*out = status;

	if (opts == NULL || opts->baseline != head)
		git_tree_free(head);
	git_index_free(index);

	return error;
}

size_t git_status_list_entrycount(git_status_list *status)
{
	GIT_ASSERT_ARG_WITH_RETVAL(status, 0);

	return status->paired.length;
}

const git_status_entry *git_status_byindex(git_status_list *status, size_t i)
{
	GIT_ASSERT_ARG_WITH_RETVAL(status, NULL);

	return git_vector_get(&status->paired, i);
}

void git_status_list_free(git_status_list *status)
{
	if (status == NULL)
		return;

	git_diff_free(status->head2idx);
	git_diff_free(status->idx2wd);

	git_vector_dispose_deep(&status->paired);

	git__memzero(status, sizeof(*status));
	git__free(status);
}

int git_status_foreach_ext(
	git_repository *repo,
	const git_status_options *opts,
	git_status_cb cb,
	void *payload)
{
	git_status_list *status;
	const git_status_entry *status_entry;
	size_t i;
	int error = 0;

	if ((error = git_status_list_new(&status, repo, opts)) < 0) {
		return error;
	}

	git_vector_foreach(&status->paired, i, status_entry) {
		const char *path = status_entry->head_to_index ?
			status_entry->head_to_index->old_file.path :
			status_entry->index_to_workdir->old_file.path;

		if ((error = cb(path, status_entry->status, payload)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	git_status_list_free(status);

	return error;
}

int git_status_foreach(git_repository *repo, git_status_cb cb, void *payload)
{
	return git_status_foreach_ext(repo, NULL, cb, payload);
}

struct status_file_info {
	char *expected;
	unsigned int count;
	unsigned int status;
	int wildmatch_flags;
	int ambiguous;
};

static int get_one_status(const char *path, unsigned int status, void *data)
{
	struct status_file_info *sfi = data;
	int (*strcomp)(const char *a, const char *b);

	sfi->count++;
	sfi->status = status;

	strcomp = (sfi->wildmatch_flags & WM_CASEFOLD) ? git__strcasecmp : git__strcmp;

	if (sfi->count > 1 ||
		(strcomp(sfi->expected, path) != 0 &&
		 wildmatch(sfi->expected, path, sfi->wildmatch_flags) != 0))
	{
		sfi->ambiguous = true;
		return GIT_EAMBIGUOUS; /* git_error_set will be done by caller */
	}

	return 0;
}

int git_status_file(
	unsigned int *status_flags,
	git_repository *repo,
	const char *path)
{
	int error;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	struct status_file_info sfi = {0};
	git_index *index;

	GIT_ASSERT_ARG(status_flags);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(path);

	if ((error = git_repository_index__weakptr(&index, repo)) < 0)
		return error;

	if ((sfi.expected = git__strdup(path)) == NULL)
		return -1;
	if (index->ignore_case)
		sfi.wildmatch_flags = WM_CASEFOLD;

	opts.show = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
	opts.flags = GIT_STATUS_OPT_INCLUDE_IGNORED |
		GIT_STATUS_OPT_RECURSE_IGNORED_DIRS |
		GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS |
		GIT_STATUS_OPT_INCLUDE_UNMODIFIED |
		GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH;
	opts.pathspec.count = 1;
	opts.pathspec.strings = &sfi.expected;

	error = git_status_foreach_ext(repo, &opts, get_one_status, &sfi);

	if (error < 0 && sfi.ambiguous) {
		git_error_set(GIT_ERROR_INVALID,
			"ambiguous path '%s' given to git_status_file", sfi.expected);
		error = GIT_EAMBIGUOUS;
	}

	if (!error && !sfi.count) {
		git_error_set(GIT_ERROR_INVALID,
			"attempt to get status of nonexistent file '%s'", path);
		error = GIT_ENOTFOUND;
	}

	*status_flags = sfi.status;

	git__free(sfi.expected);

	return error;
}

int git_status_should_ignore(
	int *ignored,
	git_repository *repo,
	const char *path)
{
	return git_ignore_path_is_ignored(ignored, repo, path);
}

int git_status_options_init(git_status_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_status_options, GIT_STATUS_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_status_init_options(git_status_options *opts, unsigned int version)
{
	return git_status_options_init(opts, version);
}
#endif

int git_status_list_get_perfdata(
	git_diff_perfdata *out, const git_status_list *status)
{
	GIT_ASSERT_ARG(out);

	GIT_ERROR_CHECK_VERSION(out, GIT_DIFF_PERFDATA_VERSION, "git_diff_perfdata");

	out->stat_calls = 0;
	out->oid_calculations = 0;

	if (status->head2idx) {
		out->stat_calls += status->head2idx->perf.stat_calls;
		out->oid_calculations += status->head2idx->perf.oid_calculations;
	}
	if (status->idx2wd) {
		out->stat_calls += status->idx2wd->perf.stat_calls;
		out->oid_calculations += status->idx2wd->perf.oid_calculations;
	}

	return 0;
}

