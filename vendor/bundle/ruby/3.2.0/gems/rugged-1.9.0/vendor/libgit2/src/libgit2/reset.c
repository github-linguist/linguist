/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "commit.h"
#include "tag.h"
#include "merge.h"
#include "diff.h"
#include "annotated_commit.h"
#include "git2/reset.h"
#include "git2/checkout.h"
#include "git2/merge.h"
#include "git2/refs.h"

#define ERROR_MSG "Cannot perform reset"

int git_reset_default(
	git_repository *repo,
	const git_object *target,
	const git_strarray *pathspecs)
{
	git_object *commit = NULL;
	git_tree *tree = NULL;
	git_diff *diff = NULL;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	size_t i, max_i;
	git_index_entry entry;
	int error;
	git_index *index = NULL;

	GIT_ASSERT_ARG(pathspecs && pathspecs->count > 0);

	memset(&entry, 0, sizeof(git_index_entry));

	if ((error = git_repository_index(&index, repo)) < 0)
		goto cleanup;

	if (target) {
		if (git_object_owner(target) != repo) {
			git_error_set(GIT_ERROR_OBJECT,
				"%s_default - The given target does not belong to this repository.", ERROR_MSG);
			return -1;
		}

		if ((error = git_object_peel(&commit, target, GIT_OBJECT_COMMIT)) < 0 ||
			(error = git_commit_tree(&tree, (git_commit *)commit)) < 0)
			goto cleanup;
	}

	opts.pathspec = *pathspecs;
	opts.flags = GIT_DIFF_REVERSE;

	if ((error = git_diff_tree_to_index(
		&diff, repo, tree, index, &opts)) < 0)
			goto cleanup;

	for (i = 0, max_i = git_diff_num_deltas(diff); i < max_i; ++i) {
		const git_diff_delta *delta = git_diff_get_delta(diff, i);

		GIT_ASSERT(delta->status == GIT_DELTA_ADDED ||
		           delta->status == GIT_DELTA_MODIFIED ||
		           delta->status == GIT_DELTA_CONFLICTED ||
		           delta->status == GIT_DELTA_DELETED);

		error = git_index_conflict_remove(index, delta->old_file.path);
		if (error < 0) {
			if (delta->status == GIT_DELTA_ADDED && error == GIT_ENOTFOUND)
				git_error_clear();
			else
				goto cleanup;
		}

		if (delta->status == GIT_DELTA_DELETED) {
			if ((error = git_index_remove(index, delta->old_file.path, 0)) < 0)
				goto cleanup;
		} else {
			entry.mode = delta->new_file.mode;
			git_oid_cpy(&entry.id, &delta->new_file.id);
			entry.path = (char *)delta->new_file.path;

			if ((error = git_index_add(index, &entry)) < 0)
				goto cleanup;
		}
	}

	error = git_index_write(index);

cleanup:
	git_object_free(commit);
	git_tree_free(tree);
	git_index_free(index);
	git_diff_free(diff);

	return error;
}

static int reset(
	git_repository *repo,
	const git_object *target,
	const char *to,
	git_reset_t reset_type,
	const git_checkout_options *checkout_opts)
{
	git_object *commit = NULL;
	git_index *index = NULL;
	git_tree *tree = NULL;
	int error = 0;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_str log_message = GIT_STR_INIT;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(target);

	if (checkout_opts)
		opts = *checkout_opts;

	if (git_object_owner(target) != repo) {
		git_error_set(GIT_ERROR_OBJECT,
			"%s - The given target does not belong to this repository.", ERROR_MSG);
		return -1;
	}

	if (reset_type != GIT_RESET_SOFT &&
		(error = git_repository__ensure_not_bare(repo,
			reset_type == GIT_RESET_MIXED ? "reset mixed" : "reset hard")) < 0)
		return error;

	if ((error = git_object_peel(&commit, target, GIT_OBJECT_COMMIT)) < 0 ||
		(error = git_repository_index(&index, repo)) < 0 ||
		(error = git_commit_tree(&tree, (git_commit *)commit)) < 0)
		goto cleanup;

	if (reset_type == GIT_RESET_SOFT &&
		(git_repository_state(repo) == GIT_REPOSITORY_STATE_MERGE ||
		 git_index_has_conflicts(index)))
	{
		git_error_set(GIT_ERROR_OBJECT, "%s (soft) in the middle of a merge", ERROR_MSG);
		error = GIT_EUNMERGED;
		goto cleanup;
	}

	if ((error = git_str_printf(&log_message, "reset: moving to %s", to)) < 0)
		return error;

	if (reset_type == GIT_RESET_HARD) {
		/* overwrite working directory with the new tree */
		opts.checkout_strategy = GIT_CHECKOUT_FORCE;

		if ((error = git_checkout_tree(repo, (git_object *)tree, &opts)) < 0)
			goto cleanup;
	}

	/* move HEAD to the new target */
	if ((error = git_reference__update_terminal(repo, GIT_HEAD_FILE,
		git_object_id(commit), NULL, git_str_cstr(&log_message))) < 0)
		goto cleanup;

	if (reset_type > GIT_RESET_SOFT) {
		/* reset index to the target content */

		if ((error = git_index_read_tree(index, tree)) < 0 ||
			(error = git_index_write(index)) < 0)
			goto cleanup;

		if ((error = git_repository_state_cleanup(repo)) < 0) {
			git_error_set(GIT_ERROR_INDEX, "%s - failed to clean up merge data", ERROR_MSG);
			goto cleanup;
		}
	}

cleanup:
	git_object_free(commit);
	git_index_free(index);
	git_tree_free(tree);
	git_str_dispose(&log_message);

	return error;
}

int git_reset(
	git_repository *repo,
	const git_object *target,
	git_reset_t reset_type,
	const git_checkout_options *checkout_opts)
{
	char to[GIT_OID_MAX_HEXSIZE + 1];

	git_oid_tostr(to, GIT_OID_MAX_HEXSIZE + 1, git_object_id(target));
	return reset(repo, target, to, reset_type, checkout_opts);
}

int git_reset_from_annotated(
	git_repository *repo,
	const git_annotated_commit *commit,
	git_reset_t reset_type,
	const git_checkout_options *checkout_opts)
{
	return reset(repo, (git_object *) commit->commit, commit->description, reset_type, checkout_opts);
}
