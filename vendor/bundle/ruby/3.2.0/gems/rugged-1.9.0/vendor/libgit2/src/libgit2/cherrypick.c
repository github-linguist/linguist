/*
* Copyright (C) the libgit2 contributors. All rights reserved.
*
* This file is part of libgit2, distributed under the GNU GPL v2 with
* a Linking Exception. For full terms see the included COPYING file.
*/

#include "common.h"

#include "repository.h"
#include "filebuf.h"
#include "merge.h"
#include "vector.h"
#include "index.h"

#include "git2/types.h"
#include "git2/merge.h"
#include "git2/cherrypick.h"
#include "git2/commit.h"
#include "git2/sys/commit.h"

#define GIT_CHERRYPICK_FILE_MODE		0666

static int write_cherrypick_head(
	git_repository *repo,
	const char *commit_oidstr)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	git_str file_path = GIT_STR_INIT;
	int error = 0;

	if ((error = git_str_joinpath(&file_path, repo->gitdir, GIT_CHERRYPICK_HEAD_FILE)) >= 0 &&
		(error = git_filebuf_open(&file, file_path.ptr, GIT_FILEBUF_CREATE_LEADING_DIRS, GIT_CHERRYPICK_FILE_MODE)) >= 0 &&
		(error = git_filebuf_printf(&file, "%s\n", commit_oidstr)) >= 0)
		error = git_filebuf_commit(&file);

	if (error < 0)
		git_filebuf_cleanup(&file);

	git_str_dispose(&file_path);

	return error;
}

static int write_merge_msg(
	git_repository *repo,
	const char *commit_msg)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	git_str file_path = GIT_STR_INIT;
	int error = 0;

	if ((error = git_str_joinpath(&file_path, repo->gitdir, GIT_MERGE_MSG_FILE)) < 0 ||
		(error = git_filebuf_open(&file, file_path.ptr, GIT_FILEBUF_CREATE_LEADING_DIRS, GIT_CHERRYPICK_FILE_MODE)) < 0 ||
		(error = git_filebuf_printf(&file, "%s", commit_msg)) < 0)
		goto cleanup;

	error = git_filebuf_commit(&file);

cleanup:
	if (error < 0)
		git_filebuf_cleanup(&file);

	git_str_dispose(&file_path);

	return error;
}

static int cherrypick_normalize_opts(
	git_repository *repo,
	git_cherrypick_options *opts,
	const git_cherrypick_options *given,
	const char *their_label)
{
	int error = 0;
	unsigned int default_checkout_strategy = GIT_CHECKOUT_ALLOW_CONFLICTS;

	GIT_UNUSED(repo);

	if (given != NULL)
		memcpy(opts, given, sizeof(git_cherrypick_options));
	else {
		git_cherrypick_options default_opts = GIT_CHERRYPICK_OPTIONS_INIT;
		memcpy(opts, &default_opts, sizeof(git_cherrypick_options));
	}

	if (!opts->checkout_opts.checkout_strategy)
		opts->checkout_opts.checkout_strategy = default_checkout_strategy;

	if (!opts->checkout_opts.our_label)
		opts->checkout_opts.our_label = "HEAD";

	if (!opts->checkout_opts.their_label)
		opts->checkout_opts.their_label = their_label;

	return error;
}

static int cherrypick_state_cleanup(git_repository *repo)
{
	const char *state_files[] = { GIT_CHERRYPICK_HEAD_FILE, GIT_MERGE_MSG_FILE };

	return git_repository__cleanup_files(repo, state_files, ARRAY_SIZE(state_files));
}

static int cherrypick_seterr(git_commit *commit, const char *fmt)
{
	char commit_oidstr[GIT_OID_MAX_HEXSIZE + 1];

	git_error_set(GIT_ERROR_CHERRYPICK, fmt,
		git_oid_tostr(commit_oidstr, GIT_OID_MAX_HEXSIZE + 1, git_commit_id(commit)));

	return -1;
}

int git_cherrypick_commit(
	git_index **out,
	git_repository *repo,
	git_commit *cherrypick_commit,
	git_commit *our_commit,
	unsigned int mainline,
	const git_merge_options *merge_opts)
{
	git_commit *parent_commit = NULL;
	git_tree *parent_tree = NULL, *our_tree = NULL, *cherrypick_tree = NULL;
	int parent = 0, error = 0;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(cherrypick_commit);
	GIT_ASSERT_ARG(our_commit);

	if (git_commit_parentcount(cherrypick_commit) > 1) {
		if (!mainline)
			return cherrypick_seterr(cherrypick_commit,
				"mainline branch is not specified but %s is a merge commit");

		parent = mainline;
	} else {
		if (mainline)
			return cherrypick_seterr(cherrypick_commit,
				"mainline branch specified but %s is not a merge commit");

		parent = git_commit_parentcount(cherrypick_commit);
	}

	if (parent &&
		((error = git_commit_parent(&parent_commit, cherrypick_commit, (parent - 1))) < 0 ||
		(error = git_commit_tree(&parent_tree, parent_commit)) < 0))
		goto done;

	if ((error = git_commit_tree(&cherrypick_tree, cherrypick_commit)) < 0 ||
		(error = git_commit_tree(&our_tree, our_commit)) < 0)
		goto done;

	error = git_merge_trees(out, repo, parent_tree, our_tree, cherrypick_tree, merge_opts);

done:
	git_tree_free(parent_tree);
	git_tree_free(our_tree);
	git_tree_free(cherrypick_tree);
	git_commit_free(parent_commit);

	return error;
}

int git_cherrypick(
	git_repository *repo,
	git_commit *commit,
	const git_cherrypick_options *given_opts)
{
	git_cherrypick_options opts;
	git_reference *our_ref = NULL;
	git_commit *our_commit = NULL;
	char commit_oidstr[GIT_OID_MAX_HEXSIZE + 1];
	const char *commit_msg, *commit_summary;
	git_str their_label = GIT_STR_INIT;
	git_index *index = NULL;
	git_indexwriter indexwriter = GIT_INDEXWRITER_INIT;
	int error = 0;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(commit);

	GIT_ERROR_CHECK_VERSION(given_opts, GIT_CHERRYPICK_OPTIONS_VERSION, "git_cherrypick_options");

	if ((error = git_repository__ensure_not_bare(repo, "cherry-pick")) < 0)
		return error;

	if ((commit_msg = git_commit_message(commit)) == NULL ||
		(commit_summary = git_commit_summary(commit)) == NULL) {
		error = -1;
		goto on_error;
	}

	git_oid_nfmt(commit_oidstr, sizeof(commit_oidstr), git_commit_id(commit));

	if ((error = write_merge_msg(repo, commit_msg)) < 0 ||
		(error = git_str_printf(&their_label, "%.7s... %s", commit_oidstr, commit_summary)) < 0 ||
		(error = cherrypick_normalize_opts(repo, &opts, given_opts, git_str_cstr(&their_label))) < 0 ||
		(error = git_indexwriter_init_for_operation(&indexwriter, repo, &opts.checkout_opts.checkout_strategy)) < 0 ||
		(error = write_cherrypick_head(repo, commit_oidstr)) < 0 ||
		(error = git_repository_head(&our_ref, repo)) < 0 ||
		(error = git_reference_peel((git_object **)&our_commit, our_ref, GIT_OBJECT_COMMIT)) < 0 ||
		(error = git_cherrypick_commit(&index, repo, commit, our_commit, opts.mainline, &opts.merge_opts)) < 0 ||
		(error = git_merge__check_result(repo, index)) < 0 ||
		(error = git_merge__append_conflicts_to_merge_msg(repo, index)) < 0 ||
		(error = git_checkout_index(repo, index, &opts.checkout_opts)) < 0 ||
		(error = git_indexwriter_commit(&indexwriter)) < 0)
		goto on_error;

	goto done;

on_error:
	cherrypick_state_cleanup(repo);

done:
	git_indexwriter_cleanup(&indexwriter);
	git_index_free(index);
	git_commit_free(our_commit);
	git_reference_free(our_ref);
	git_str_dispose(&their_label);

	return error;
}

int git_cherrypick_options_init(
	git_cherrypick_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_cherrypick_options, GIT_CHERRYPICK_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_cherrypick_init_options(
	git_cherrypick_options *opts, unsigned int version)
{
	return git_cherrypick_options_init(opts, version);
}
#endif
