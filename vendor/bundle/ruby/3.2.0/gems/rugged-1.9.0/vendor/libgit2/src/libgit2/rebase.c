/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "str.h"
#include "repository.h"
#include "posix.h"
#include "filebuf.h"
#include "commit.h"
#include "merge.h"
#include "array.h"
#include "config.h"
#include "annotated_commit.h"
#include "index.h"

#include <git2/types.h>
#include <git2/annotated_commit.h>
#include <git2/rebase.h>
#include <git2/commit.h>
#include <git2/reset.h>
#include <git2/revwalk.h>
#include <git2/notes.h>

#define REBASE_APPLY_DIR    "rebase-apply"
#define REBASE_MERGE_DIR    "rebase-merge"

#define HEAD_NAME_FILE      "head-name"
#define ORIG_HEAD_FILE      "orig-head"
#define HEAD_FILE           "head"
#define ONTO_FILE           "onto"
#define ONTO_NAME_FILE      "onto_name"
#define QUIET_FILE          "quiet"
#define INTERACTIVE_FILE    "interactive"

#define MSGNUM_FILE         "msgnum"
#define END_FILE            "end"
#define CMT_FILE_FMT        "cmt.%" PRIuZ
#define CURRENT_FILE        "current"
#define REWRITTEN_FILE      "rewritten"

#define ORIG_DETACHED_HEAD  "detached HEAD"

#define NOTES_DEFAULT_REF   NULL

#define REBASE_DIR_MODE     0777
#define REBASE_FILE_MODE    0666

typedef enum {
	GIT_REBASE_NONE = 0,
	GIT_REBASE_APPLY = 1,
	GIT_REBASE_MERGE = 2,
	GIT_REBASE_INTERACTIVE = 3
} git_rebase_t;

struct git_rebase {
	git_repository *repo;

	git_rebase_options options;

	git_rebase_t type;
	char *state_path;

	/* Temporary buffer for paths within the state path. */
	git_str state_filename;

	unsigned int head_detached:1,
	             inmemory:1,
	             quiet:1,
	             started:1;

	git_array_t(git_rebase_operation) operations;
	size_t current;

	/* Used by in-memory rebase */
	git_index *index;
	git_commit *last_commit;

	/* Used by regular (not in-memory) merge-style rebase */
	git_oid orig_head_id;
	char *orig_head_name;

	git_oid onto_id;
	char *onto_name;
};

#define GIT_REBASE_STATE_INIT {0}

static int rebase_state_type(
	git_rebase_t *type_out,
	char **path_out,
	git_repository *repo)
{
	git_str path = GIT_STR_INIT;
	git_str interactive_path = GIT_STR_INIT;
	git_rebase_t type = GIT_REBASE_NONE;

	if (git_str_joinpath(&path, repo->gitdir, REBASE_APPLY_DIR) < 0)
		return -1;

	if (git_fs_path_isdir(git_str_cstr(&path))) {
		type = GIT_REBASE_APPLY;
		goto done;
	}

	git_str_clear(&path);
	if (git_str_joinpath(&path, repo->gitdir, REBASE_MERGE_DIR) < 0)
		return -1;

	if (git_fs_path_isdir(git_str_cstr(&path))) {
		if (git_str_joinpath(&interactive_path, path.ptr, INTERACTIVE_FILE) < 0)
			return -1;

		if (git_fs_path_isfile(interactive_path.ptr))
			type = GIT_REBASE_INTERACTIVE;
		else
			type = GIT_REBASE_MERGE;

		goto done;
	}

done:
	*type_out = type;

	if (type != GIT_REBASE_NONE && path_out)
		*path_out = git_str_detach(&path);

	git_str_dispose(&path);
	git_str_dispose(&interactive_path);

	return 0;
}

GIT_INLINE(int) rebase_readfile(
	git_str *out,
	git_rebase *rebase,
	const char *filename)
{
	/*
	 * `rebase->state_filename` is a temporary buffer to avoid
	 * unnecessary allocations and copies of `rebase->state_path`.
	 * At the start and end of this function it always contains the
	 * contents of `rebase->state_path` itself.
	 */
	size_t state_path_len = rebase->state_filename.size;
	int error;

	git_str_clear(out);

	if ((error = git_str_joinpath(&rebase->state_filename, rebase->state_filename.ptr, filename)) < 0 ||
	    (error = git_futils_readbuffer(out, rebase->state_filename.ptr)) < 0)
		goto done;

	git_str_rtrim(out);

done:
	git_str_truncate(&rebase->state_filename, state_path_len);
	return error;
}

GIT_INLINE(int) rebase_readint(
	size_t *out,
	git_str *asc_out,
	git_rebase *rebase,
	const char *filename)
{
	int32_t num;
	const char *eol;
	int error = 0;

	if ((error = rebase_readfile(asc_out, rebase, filename)) < 0)
		return error;

	if (git__strntol32(&num, asc_out->ptr, asc_out->size, &eol, 10) < 0 || num < 0 || *eol) {
		git_error_set(GIT_ERROR_REBASE, "the file '%s' contains an invalid numeric value", filename);
		return -1;
	}

	*out = (size_t) num;

	return 0;
}

GIT_INLINE(int) rebase_readoid(
	git_oid *out,
	git_str *str_out,
	git_rebase *rebase,
	const char *filename)
{
	int error;

	if ((error = rebase_readfile(str_out, rebase, filename)) < 0)
		return error;

	if (str_out->size != git_oid_hexsize(rebase->repo->oid_type) ||
	    git_oid__fromstr(out, str_out->ptr, rebase->repo->oid_type) < 0) {
		git_error_set(GIT_ERROR_REBASE, "the file '%s' contains an invalid object ID", filename);
		return -1;
	}

	return 0;
}

static git_rebase_operation *rebase_operation_alloc(
	git_rebase *rebase,
	git_rebase_operation_t type,
	git_oid *id,
	const char *exec)
{
	git_rebase_operation *operation;

	GIT_ASSERT_WITH_RETVAL((type == GIT_REBASE_OPERATION_EXEC) == !id, NULL);
	GIT_ASSERT_WITH_RETVAL((type == GIT_REBASE_OPERATION_EXEC) == !!exec, NULL);

	if ((operation = git_array_alloc(rebase->operations)) == NULL)
		return NULL;

	operation->type = type;
	git_oid_cpy((git_oid *)&operation->id, id);
	operation->exec = exec;

	return operation;
}

static int rebase_open_merge(git_rebase *rebase)
{
	git_str buf = GIT_STR_INIT, cmt = GIT_STR_INIT;
	git_oid id;
	git_rebase_operation *operation;
	size_t i, msgnum = 0, end;
	int error;

	/* Read 'msgnum' if it exists (otherwise, let msgnum = 0) */
	if ((error = rebase_readint(&msgnum, &buf, rebase, MSGNUM_FILE)) < 0 &&
		error != GIT_ENOTFOUND)
		goto done;

	if (msgnum) {
		rebase->started = 1;
		rebase->current = msgnum - 1;
	}

	/* Read 'end' */
	if ((error = rebase_readint(&end, &buf, rebase, END_FILE)) < 0)
		goto done;

	/* Read 'current' if it exists */
	if ((error = rebase_readoid(&id, &buf, rebase, CURRENT_FILE)) < 0 &&
		error != GIT_ENOTFOUND)
		goto done;

	/* Read cmt.* */
	git_array_init_to_size(rebase->operations, end);
	GIT_ERROR_CHECK_ARRAY(rebase->operations);

	for (i = 0; i < end; i++) {
		git_str_clear(&cmt);

		if ((error = git_str_printf(&cmt, "cmt.%" PRIuZ, (i+1))) < 0 ||
			(error = rebase_readoid(&id, &buf, rebase, cmt.ptr)) < 0)
			goto done;

		operation = rebase_operation_alloc(rebase, GIT_REBASE_OPERATION_PICK, &id, NULL);
		GIT_ERROR_CHECK_ALLOC(operation);
	}

	/* Read 'onto_name' */
	if ((error = rebase_readfile(&buf, rebase, ONTO_NAME_FILE)) < 0)
		goto done;

	rebase->onto_name = git_str_detach(&buf);

done:
	git_str_dispose(&cmt);
	git_str_dispose(&buf);

	return error;
}

static int rebase_alloc(git_rebase **out, const git_rebase_options *rebase_opts)
{
	git_rebase *rebase = git__calloc(1, sizeof(git_rebase));
	GIT_ERROR_CHECK_ALLOC(rebase);

	*out = NULL;

	if (rebase_opts)
		memcpy(&rebase->options, rebase_opts, sizeof(git_rebase_options));
	else
		git_rebase_options_init(&rebase->options, GIT_REBASE_OPTIONS_VERSION);

	if (rebase_opts && rebase_opts->rewrite_notes_ref) {
		rebase->options.rewrite_notes_ref = git__strdup(rebase_opts->rewrite_notes_ref);
		GIT_ERROR_CHECK_ALLOC(rebase->options.rewrite_notes_ref);
	}

	*out = rebase;

	return 0;
}

static int rebase_check_versions(const git_rebase_options *given_opts)
{
	GIT_ERROR_CHECK_VERSION(given_opts, GIT_REBASE_OPTIONS_VERSION, "git_rebase_options");

	if (given_opts)
		GIT_ERROR_CHECK_VERSION(&given_opts->checkout_options, GIT_CHECKOUT_OPTIONS_VERSION, "git_checkout_options");

	return 0;
}

int git_rebase_open(
	git_rebase **out,
	git_repository *repo,
	const git_rebase_options *given_opts)
{
	git_rebase *rebase;
	git_str orig_head_name = GIT_STR_INIT,
		orig_head_id = GIT_STR_INIT,
		onto_id = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(repo);

	if ((error = rebase_check_versions(given_opts)) < 0)
		return error;

	if (rebase_alloc(&rebase, given_opts) < 0)
		return -1;

	rebase->repo = repo;

	if ((error = rebase_state_type(&rebase->type, &rebase->state_path, repo)) < 0)
		goto done;

	if (rebase->type == GIT_REBASE_NONE) {
		git_error_set(GIT_ERROR_REBASE, "there is no rebase in progress");
		error = GIT_ENOTFOUND;
		goto done;
	}

	if ((error = git_str_puts(&rebase->state_filename, rebase->state_path)) < 0)
		goto done;

	if ((error = rebase_readfile(&orig_head_name, rebase, HEAD_NAME_FILE)) < 0)
		goto done;

	git_str_rtrim(&orig_head_name);

	if (strcmp(ORIG_DETACHED_HEAD, orig_head_name.ptr) == 0)
		rebase->head_detached = 1;

	if ((error = rebase_readoid(&rebase->orig_head_id, &orig_head_id, rebase, ORIG_HEAD_FILE)) < 0) {
		/* Previous versions of git.git used 'head' here; support that. */
		if (error == GIT_ENOTFOUND)
			error = rebase_readoid(&rebase->orig_head_id, &orig_head_id, rebase, HEAD_FILE);

		if (error < 0)
			goto done;
	}

	if ((error = rebase_readoid(&rebase->onto_id, &onto_id, rebase, ONTO_FILE)) < 0)
		goto done;

	if (!rebase->head_detached)
		rebase->orig_head_name = git_str_detach(&orig_head_name);

	switch (rebase->type) {
	case GIT_REBASE_INTERACTIVE:
		git_error_set(GIT_ERROR_REBASE, "interactive rebase is not supported");
		error = -1;
		break;
	case GIT_REBASE_MERGE:
		error = rebase_open_merge(rebase);
		break;
	case GIT_REBASE_APPLY:
		git_error_set(GIT_ERROR_REBASE, "patch application rebase is not supported");
		error = -1;
		break;
	default:
		abort();
	}

done:
	if (error == 0)
		*out = rebase;
	else
		git_rebase_free(rebase);

	git_str_dispose(&orig_head_name);
	git_str_dispose(&orig_head_id);
	git_str_dispose(&onto_id);
	return error;
}

static int rebase_cleanup(git_rebase *rebase)
{
	if (!rebase || rebase->inmemory)
		return 0;

	return git_fs_path_isdir(rebase->state_path) ?
		git_futils_rmdir_r(rebase->state_path, NULL, GIT_RMDIR_REMOVE_FILES) :
		0;
}

static int rebase_setupfile(git_rebase *rebase, const char *filename, int flags, const char *fmt, ...)
{
	git_str path = GIT_STR_INIT,
		contents = GIT_STR_INIT;
	va_list ap;
	int error;

	va_start(ap, fmt);
	git_str_vprintf(&contents, fmt, ap);
	va_end(ap);

	if ((error = git_str_joinpath(&path, rebase->state_path, filename)) == 0)
		error = git_futils_writebuffer(&contents, path.ptr, flags, REBASE_FILE_MODE);

	git_str_dispose(&path);
	git_str_dispose(&contents);

	return error;
}

static const char *rebase_onto_name(const git_annotated_commit *onto)
{
	if (onto->ref_name && git__strncmp(onto->ref_name, "refs/heads/", 11) == 0)
		return onto->ref_name + 11;
	else if (onto->ref_name)
		return onto->ref_name;
	else
		return onto->id_str;
}

static int rebase_setupfiles_merge(git_rebase *rebase)
{
	git_str commit_filename = GIT_STR_INIT;
	char id_str[GIT_OID_MAX_HEXSIZE + 1];
	git_rebase_operation *operation;
	size_t i;
	int error = 0;

	if ((error = rebase_setupfile(rebase, END_FILE, 0, "%" PRIuZ "\n", git_array_size(rebase->operations))) < 0 ||
	    (error = rebase_setupfile(rebase, ONTO_NAME_FILE, 0, "%s\n", rebase->onto_name)) < 0)
		goto done;

	for (i = 0; i < git_array_size(rebase->operations); i++) {
		operation = git_array_get(rebase->operations, i);

		git_str_clear(&commit_filename);
		git_str_printf(&commit_filename, CMT_FILE_FMT, i+1);

		git_oid_tostr(id_str, GIT_OID_MAX_HEXSIZE + 1, &operation->id);

		if ((error = rebase_setupfile(rebase, commit_filename.ptr, 0, "%s\n", id_str)) < 0)
			goto done;
	}

done:
	git_str_dispose(&commit_filename);
	return error;
}

static int rebase_setupfiles(git_rebase *rebase)
{
	char onto[GIT_OID_MAX_HEXSIZE + 1], orig_head[GIT_OID_MAX_HEXSIZE + 1];
	const char *orig_head_name;

	git_oid_tostr(onto, GIT_OID_MAX_HEXSIZE + 1, &rebase->onto_id);
	git_oid_tostr(orig_head, GIT_OID_MAX_HEXSIZE + 1, &rebase->orig_head_id);

	if (p_mkdir(rebase->state_path, REBASE_DIR_MODE) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to create rebase directory '%s'", rebase->state_path);
		return -1;
	}

	orig_head_name = rebase->head_detached ? ORIG_DETACHED_HEAD :
		rebase->orig_head_name;

	if (git_repository__set_orig_head(rebase->repo, &rebase->orig_head_id) < 0 ||
		rebase_setupfile(rebase, HEAD_NAME_FILE, 0, "%s\n", orig_head_name) < 0 ||
		rebase_setupfile(rebase, ONTO_FILE, 0, "%s\n", onto) < 0 ||
		rebase_setupfile(rebase, ORIG_HEAD_FILE, 0, "%s\n", orig_head) < 0 ||
		rebase_setupfile(rebase, QUIET_FILE, 0, rebase->quiet ? "t\n" : "\n") < 0)
		return -1;

	return rebase_setupfiles_merge(rebase);
}

int git_rebase_options_init(git_rebase_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_rebase_options, GIT_REBASE_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_rebase_init_options(git_rebase_options *opts, unsigned int version)
{
	return git_rebase_options_init(opts, version);
}
#endif

static int rebase_ensure_not_in_progress(git_repository *repo)
{
	int error;
	git_rebase_t type;

	if ((error = rebase_state_type(&type, NULL, repo)) < 0)
		return error;

	if (type != GIT_REBASE_NONE) {
		git_error_set(GIT_ERROR_REBASE, "there is an existing rebase in progress");
		return -1;
	}

	return 0;
}

static int rebase_ensure_not_dirty(
	git_repository *repo,
	bool check_index,
	bool check_workdir,
	int fail_with)
{
	git_tree *head = NULL;
	git_index *index = NULL;
	git_diff *diff = NULL;
	int error = 0;

	if (check_index) {
		if ((error = git_repository_head_tree(&head, repo)) < 0 ||
			(error = git_repository_index(&index, repo)) < 0 ||
			(error = git_diff_tree_to_index(&diff, repo, head, index, NULL)) < 0)
			goto done;

		if (git_diff_num_deltas(diff) > 0) {
			git_error_set(GIT_ERROR_REBASE, "uncommitted changes exist in index");
			error = fail_with;
			goto done;
		}

		git_diff_free(diff);
		diff = NULL;
	}

	if (check_workdir) {
		git_diff_options diff_opts = GIT_DIFF_OPTIONS_INIT;
		diff_opts.ignore_submodules = GIT_SUBMODULE_IGNORE_UNTRACKED;
		if ((error = git_diff_index_to_workdir(&diff, repo, index, &diff_opts)) < 0)
			goto done;

		if (git_diff_num_deltas(diff) > 0) {
			git_error_set(GIT_ERROR_REBASE, "unstaged changes exist in workdir");
			error = fail_with;
			goto done;
		}
	}

done:
	git_diff_free(diff);
	git_index_free(index);
	git_tree_free(head);

	return error;
}

static int rebase_init_operations(
	git_rebase *rebase,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto)
{
	git_revwalk *revwalk = NULL;
	git_commit *commit;
	git_oid id;
	bool merge;
	git_rebase_operation *operation;
	int error;

	if (!upstream)
		upstream = onto;

	if ((error = git_revwalk_new(&revwalk, rebase->repo)) < 0 ||
		(error = git_revwalk_push(revwalk, git_annotated_commit_id(branch))) < 0 ||
		(error = git_revwalk_hide(revwalk, git_annotated_commit_id(upstream))) < 0)
		goto done;

	git_revwalk_sorting(revwalk, GIT_SORT_REVERSE);

	while ((error = git_revwalk_next(&id, revwalk)) == 0) {
		if ((error = git_commit_lookup(&commit, repo, &id)) < 0)
			goto done;

		merge = (git_commit_parentcount(commit) > 1);
		git_commit_free(commit);

		if (merge)
			continue;

		operation = rebase_operation_alloc(rebase, GIT_REBASE_OPERATION_PICK, &id, NULL);
		GIT_ERROR_CHECK_ALLOC(operation);
	}

	error = 0;

done:
	git_revwalk_free(revwalk);
	return error;
}

static int rebase_init_merge(
	git_rebase *rebase,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto)
{
	git_reference *head_ref = NULL;
	git_commit *onto_commit = NULL;
	git_str reflog = GIT_STR_INIT;
	git_str state_path = GIT_STR_INIT;
	int error;

	GIT_UNUSED(upstream);

	if ((error = git_str_joinpath(&state_path, repo->gitdir, REBASE_MERGE_DIR)) < 0 ||
	    (error = git_str_put(&rebase->state_filename, state_path.ptr, state_path.size)) < 0)
		goto done;

	rebase->state_path = git_str_detach(&state_path);
	GIT_ERROR_CHECK_ALLOC(rebase->state_path);

	if (branch->ref_name && strcmp(branch->ref_name, "HEAD")) {
		rebase->orig_head_name = git__strdup(branch->ref_name);
		GIT_ERROR_CHECK_ALLOC(rebase->orig_head_name);
	} else {
		rebase->head_detached = 1;
	}

	rebase->onto_name = git__strdup(rebase_onto_name(onto));
	GIT_ERROR_CHECK_ALLOC(rebase->onto_name);

	rebase->quiet = rebase->options.quiet;

	git_oid_cpy(&rebase->orig_head_id, git_annotated_commit_id(branch));
	git_oid_cpy(&rebase->onto_id, git_annotated_commit_id(onto));

	if ((error = rebase_setupfiles(rebase)) < 0 ||
		(error = git_str_printf(&reflog,
			"rebase: checkout %s", rebase_onto_name(onto))) < 0 ||
		(error = git_commit_lookup(
			&onto_commit, repo, git_annotated_commit_id(onto))) < 0 ||
		(error = git_checkout_tree(repo,
			(git_object *)onto_commit, &rebase->options.checkout_options)) < 0 ||
		(error = git_reference_create(&head_ref, repo, GIT_HEAD_FILE,
			git_annotated_commit_id(onto), 1, reflog.ptr)) < 0)
		goto done;

done:
	git_reference_free(head_ref);
	git_commit_free(onto_commit);
	git_str_dispose(&reflog);
	git_str_dispose(&state_path);

	return error;
}

static int rebase_init_inmemory(
	git_rebase *rebase,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto)
{
	GIT_UNUSED(branch);
	GIT_UNUSED(upstream);

	return git_commit_lookup(
		&rebase->last_commit, repo, git_annotated_commit_id(onto));
}

int git_rebase_init(
	git_rebase **out,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto,
	const git_rebase_options *given_opts)
{
	git_rebase *rebase = NULL;
	git_annotated_commit *head_branch = NULL;
	git_reference *head_ref = NULL;
	bool inmemory = (given_opts && given_opts->inmemory);
	int error;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(upstream || onto);

	*out = NULL;

	if (!onto)
		onto = upstream;

	if ((error = rebase_check_versions(given_opts)) < 0)
		goto done;

	if (!inmemory) {
		if ((error = git_repository__ensure_not_bare(repo, "rebase")) < 0 ||
			(error = rebase_ensure_not_in_progress(repo)) < 0 ||
			(error = rebase_ensure_not_dirty(repo, true, true, GIT_ERROR)) < 0)
			goto done;
	}

	if (!branch) {
		if ((error = git_repository_head(&head_ref, repo)) < 0 ||
			(error = git_annotated_commit_from_ref(&head_branch, repo, head_ref)) < 0)
			goto done;

		branch = head_branch;
	}

	if (rebase_alloc(&rebase, given_opts) < 0)
		return -1;

	rebase->repo = repo;
	rebase->inmemory = inmemory;
	rebase->type = GIT_REBASE_MERGE;

	if ((error = rebase_init_operations(rebase, repo, branch, upstream, onto)) < 0)
		goto done;

	if (inmemory)
		error = rebase_init_inmemory(rebase, repo, branch, upstream, onto);
	else
		error = rebase_init_merge(rebase, repo, branch ,upstream, onto);

	if (error == 0)
		*out = rebase;

done:
	git_reference_free(head_ref);
	git_annotated_commit_free(head_branch);

	if (error < 0) {
		rebase_cleanup(rebase);
		git_rebase_free(rebase);
	}

	return error;
}

static void normalize_checkout_options_for_apply(
	git_checkout_options *checkout_opts,
	git_rebase *rebase,
	git_commit *current_commit)
{
	memcpy(checkout_opts, &rebase->options.checkout_options, sizeof(git_checkout_options));

	if (!checkout_opts->ancestor_label)
		checkout_opts->ancestor_label = "ancestor";

	if (rebase->type == GIT_REBASE_MERGE) {
		if (!checkout_opts->our_label)
			checkout_opts->our_label = rebase->onto_name;

		if (!checkout_opts->their_label)
			checkout_opts->their_label = git_commit_summary(current_commit);
	} else {
		abort();
	}
}

GIT_INLINE(int) rebase_movenext(git_rebase *rebase)
{
	size_t next = rebase->started ? rebase->current + 1 : 0;

	if (next == git_array_size(rebase->operations))
		return GIT_ITEROVER;

	rebase->started = 1;
	rebase->current = next;

	return 0;
}

static int rebase_next_merge(
	git_rebase_operation **out,
	git_rebase *rebase)
{
	git_str path = GIT_STR_INIT;
	git_commit *current_commit = NULL, *parent_commit = NULL;
	git_tree *current_tree = NULL, *head_tree = NULL, *parent_tree = NULL;
	git_index *index = NULL;
	git_indexwriter indexwriter = GIT_INDEXWRITER_INIT;
	git_rebase_operation *operation;
	git_checkout_options checkout_opts;
	char current_idstr[GIT_OID_MAX_HEXSIZE + 1];
	unsigned int parent_count;
	int error;

	*out = NULL;

	operation = git_array_get(rebase->operations, rebase->current);

	if ((error = git_commit_lookup(&current_commit, rebase->repo, &operation->id)) < 0 ||
		(error = git_commit_tree(&current_tree, current_commit)) < 0 ||
		(error = git_repository_head_tree(&head_tree, rebase->repo)) < 0)
		goto done;

	if ((parent_count = git_commit_parentcount(current_commit)) > 1) {
		git_error_set(GIT_ERROR_REBASE, "cannot rebase a merge commit");
		error = -1;
		goto done;
	} else if (parent_count) {
		if ((error = git_commit_parent(&parent_commit, current_commit, 0)) < 0 ||
			(error = git_commit_tree(&parent_tree, parent_commit)) < 0)
			goto done;
	}

	git_oid_tostr(current_idstr, GIT_OID_MAX_HEXSIZE + 1, &operation->id);

	normalize_checkout_options_for_apply(&checkout_opts, rebase, current_commit);

	if ((error = git_indexwriter_init_for_operation(&indexwriter, rebase->repo, &checkout_opts.checkout_strategy)) < 0 ||
		(error = rebase_setupfile(rebase, MSGNUM_FILE, 0, "%" PRIuZ "\n", rebase->current+1)) < 0 ||
		(error = rebase_setupfile(rebase, CURRENT_FILE, 0, "%s\n", current_idstr)) < 0 ||
		(error = git_merge_trees(&index, rebase->repo, parent_tree, head_tree, current_tree, &rebase->options.merge_options)) < 0 ||
		(error = git_merge__check_result(rebase->repo, index)) < 0 ||
		(error = git_checkout_index(rebase->repo, index, &checkout_opts)) < 0 ||
		(error = git_indexwriter_commit(&indexwriter)) < 0)
		goto done;

	*out = operation;

done:
	git_indexwriter_cleanup(&indexwriter);
	git_index_free(index);
	git_tree_free(current_tree);
	git_tree_free(head_tree);
	git_tree_free(parent_tree);
	git_commit_free(parent_commit);
	git_commit_free(current_commit);
	git_str_dispose(&path);

	return error;
}

static int rebase_next_inmemory(
	git_rebase_operation **out,
	git_rebase *rebase)
{
	git_commit *current_commit = NULL, *parent_commit = NULL;
	git_tree *current_tree = NULL, *head_tree = NULL, *parent_tree = NULL;
	git_rebase_operation *operation;
	git_index *index = NULL;
	unsigned int parent_count;
	int error;

	*out = NULL;

	operation = git_array_get(rebase->operations, rebase->current);

	if ((error = git_commit_lookup(&current_commit, rebase->repo, &operation->id)) < 0 ||
		(error = git_commit_tree(&current_tree, current_commit)) < 0)
		goto done;

	if ((parent_count = git_commit_parentcount(current_commit)) > 1) {
		git_error_set(GIT_ERROR_REBASE, "cannot rebase a merge commit");
		error = -1;
		goto done;
	} else if (parent_count) {
		if ((error = git_commit_parent(&parent_commit, current_commit, 0)) < 0 ||
			(error = git_commit_tree(&parent_tree, parent_commit)) < 0)
			goto done;
	}

	if ((error = git_commit_tree(&head_tree, rebase->last_commit)) < 0 ||
		(error = git_merge_trees(&index, rebase->repo, parent_tree, head_tree, current_tree, &rebase->options.merge_options)) < 0)
		goto done;

	if (!rebase->index) {
		rebase->index = index;
		index = NULL;
	} else {
		if ((error = git_index_read_index(rebase->index, index)) < 0)
			goto done;
	}

	*out = operation;

done:
	git_commit_free(current_commit);
	git_commit_free(parent_commit);
	git_tree_free(current_tree);
	git_tree_free(head_tree);
	git_tree_free(parent_tree);
	git_index_free(index);

	return error;
}

int git_rebase_next(
	git_rebase_operation **out,
	git_rebase *rebase)
{
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(rebase);

	if ((error = rebase_movenext(rebase)) < 0)
		return error;

	if (rebase->inmemory)
		error = rebase_next_inmemory(out, rebase);
	else if (rebase->type == GIT_REBASE_MERGE)
		error = rebase_next_merge(out, rebase);
	else
		abort();

	return error;
}

int git_rebase_inmemory_index(
	git_index **out,
	git_rebase *rebase)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(rebase);
	GIT_ASSERT_ARG(rebase->index);

	GIT_REFCOUNT_INC(rebase->index);
	*out = rebase->index;

	return 0;
}

#ifndef GIT_DEPRECATE_HARD
static int create_signed(
	git_oid *out,
	git_rebase *rebase,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	git_tree *tree,
	size_t parent_count,
	const git_commit **parents)
{
	git_str commit_content = GIT_STR_INIT;
	git_buf commit_signature = { NULL, 0, 0 },
	        signature_field = { NULL, 0, 0 };
	int error;

	git_error_clear();

	if ((error = git_commit__create_buffer(&commit_content,
		rebase->repo, author, committer, message_encoding,
		message, tree, parent_count, parents)) < 0)
		goto done;

	error = rebase->options.signing_cb(&commit_signature,
		&signature_field, commit_content.ptr,
		rebase->options.payload);

	if (error) {
		if (error != GIT_PASSTHROUGH)
			git_error_set_after_callback_function(error, "signing_cb");

		goto done;
	}

	error = git_commit_create_with_signature(out, rebase->repo,
		commit_content.ptr,
		commit_signature.size > 0 ? commit_signature.ptr : NULL,
		signature_field.size > 0 ? signature_field.ptr : NULL);

done:
	git_buf_dispose(&commit_signature);
	git_buf_dispose(&signature_field);
	git_str_dispose(&commit_content);
	return error;
}
#endif

static int rebase_commit__create(
	git_commit **out,
	git_rebase *rebase,
	git_index *index,
	git_commit *parent_commit,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	git_rebase_operation *operation;
	git_commit *current_commit = NULL, *commit = NULL;
	git_tree *parent_tree = NULL, *tree = NULL;
	git_oid tree_id, commit_id;
	int error;

	operation = git_array_get(rebase->operations, rebase->current);

	if (git_index_has_conflicts(index)) {
		git_error_set(GIT_ERROR_REBASE, "conflicts have not been resolved");
		error = GIT_EUNMERGED;
		goto done;
	}

	if ((error = git_commit_lookup(&current_commit, rebase->repo, &operation->id)) < 0 ||
		(error = git_commit_tree(&parent_tree, parent_commit)) < 0 ||
		(error = git_index_write_tree_to(&tree_id, index, rebase->repo)) < 0 ||
		(error = git_tree_lookup(&tree, rebase->repo, &tree_id)) < 0)
		goto done;

	if (git_oid_equal(&tree_id, git_tree_id(parent_tree))) {
		git_error_set(GIT_ERROR_REBASE, "this patch has already been applied");
		error = GIT_EAPPLIED;
		goto done;
	}

	if (!author)
		author = git_commit_author(current_commit);

	if (!message) {
		message_encoding = git_commit_message_encoding(current_commit);
		message = git_commit_message(current_commit);
	}

	git_error_clear();
	error = GIT_PASSTHROUGH;

	if (rebase->options.commit_create_cb) {
		error = rebase->options.commit_create_cb(&commit_id,
			author, committer, message_encoding, message,
			tree, 1, (const git_commit **)&parent_commit,
			rebase->options.payload);

		git_error_set_after_callback_function(error,
			"commit_create_cb");
	}
#ifndef GIT_DEPRECATE_HARD
	else if (rebase->options.signing_cb) {
		error = create_signed(&commit_id, rebase, author,
			committer, message_encoding, message, tree,
			1, (const git_commit **)&parent_commit);
	}
#endif

	if (error == GIT_PASSTHROUGH)
		error = git_commit_create(&commit_id, rebase->repo, NULL,
			author, committer, message_encoding, message,
			tree, 1, (const git_commit **)&parent_commit);

	if (error)
		goto done;

	if ((error = git_commit_lookup(&commit, rebase->repo, &commit_id)) < 0)
		goto done;

	*out = commit;

done:
	if (error < 0)
		git_commit_free(commit);

	git_commit_free(current_commit);
	git_tree_free(parent_tree);
	git_tree_free(tree);

	return error;
}

static int rebase_commit_merge(
	git_oid *commit_id,
	git_rebase *rebase,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	git_rebase_operation *operation;
	git_reference *head = NULL;
	git_commit *head_commit = NULL, *commit = NULL;
	git_index *index = NULL;
	char old_idstr[GIT_OID_MAX_HEXSIZE + 1], new_idstr[GIT_OID_MAX_HEXSIZE + 1];
	int error;

	operation = git_array_get(rebase->operations, rebase->current);
	GIT_ASSERT(operation);

	if ((error = rebase_ensure_not_dirty(rebase->repo, false, true, GIT_EUNMERGED)) < 0 ||
		(error = git_repository_head(&head, rebase->repo)) < 0 ||
		(error = git_reference_peel((git_object **)&head_commit, head, GIT_OBJECT_COMMIT)) < 0 ||
		(error = git_repository_index(&index, rebase->repo)) < 0 ||
		(error = rebase_commit__create(&commit, rebase, index, head_commit,
			author, committer, message_encoding, message)) < 0 ||
		(error = git_reference__update_for_commit(
			rebase->repo, NULL, "HEAD", git_commit_id(commit), "rebase")) < 0)
		goto done;

	git_oid_tostr(old_idstr, GIT_OID_MAX_HEXSIZE + 1, &operation->id);
	git_oid_tostr(new_idstr, GIT_OID_MAX_HEXSIZE + 1, git_commit_id(commit));

	if ((error = rebase_setupfile(rebase, REWRITTEN_FILE, O_CREAT|O_WRONLY|O_APPEND,
			"%s %s\n", old_idstr, new_idstr)) < 0)
		goto done;

	git_oid_cpy(commit_id, git_commit_id(commit));

done:
	git_index_free(index);
	git_reference_free(head);
	git_commit_free(head_commit);
	git_commit_free(commit);
	return error;
}

static int rebase_commit_inmemory(
	git_oid *commit_id,
	git_rebase *rebase,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	git_commit *commit = NULL;
	int error = 0;

	GIT_ASSERT_ARG(rebase->index);
	GIT_ASSERT_ARG(rebase->last_commit);
	GIT_ASSERT_ARG(rebase->current < rebase->operations.size);

	if ((error = rebase_commit__create(&commit, rebase, rebase->index,
		rebase->last_commit, author, committer, message_encoding, message)) < 0)
		goto done;

	git_commit_free(rebase->last_commit);
	rebase->last_commit = commit;

	git_oid_cpy(commit_id, git_commit_id(commit));

done:
	if (error < 0)
		git_commit_free(commit);

	return error;
}

int git_rebase_commit(
	git_oid *id,
	git_rebase *rebase,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	int error;

	GIT_ASSERT_ARG(rebase);
	GIT_ASSERT_ARG(committer);

	if (rebase->inmemory)
		error = rebase_commit_inmemory(
			id, rebase, author, committer, message_encoding, message);
	else if (rebase->type == GIT_REBASE_MERGE)
		error = rebase_commit_merge(
			id, rebase, author, committer, message_encoding, message);
	else
		abort();

	return error;
}

int git_rebase_abort(git_rebase *rebase)
{
	git_reference *orig_head_ref = NULL;
	git_commit *orig_head_commit = NULL;
	int error;

	GIT_ASSERT_ARG(rebase);

	if (rebase->inmemory)
		return 0;

	error = rebase->head_detached ?
		git_reference_create(&orig_head_ref, rebase->repo, GIT_HEAD_FILE,
			 &rebase->orig_head_id, 1, "rebase: aborting") :
		git_reference_symbolic_create(
			&orig_head_ref, rebase->repo, GIT_HEAD_FILE, rebase->orig_head_name, 1,
			"rebase: aborting");

	if (error < 0)
		goto done;

	if ((error = git_commit_lookup(
			&orig_head_commit, rebase->repo, &rebase->orig_head_id)) < 0 ||
		(error = git_reset(rebase->repo, (git_object *)orig_head_commit,
			GIT_RESET_HARD, &rebase->options.checkout_options)) < 0)
		goto done;

	error = rebase_cleanup(rebase);

done:
	git_commit_free(orig_head_commit);
	git_reference_free(orig_head_ref);

	return error;
}

static int notes_ref_lookup(git_str *out, git_rebase *rebase)
{
	git_config *config = NULL;
	int do_rewrite, error;

	if (rebase->options.rewrite_notes_ref) {
		git_str_attach_notowned(out,
			rebase->options.rewrite_notes_ref,
			strlen(rebase->options.rewrite_notes_ref));
		return 0;
	}

	if ((error = git_repository_config(&config, rebase->repo)) < 0 ||
		(error = git_config_get_bool(&do_rewrite, config, "notes.rewrite.rebase")) < 0) {

		if (error != GIT_ENOTFOUND)
			goto done;

		git_error_clear();
		do_rewrite = 1;
	}

	error = do_rewrite ?
		git_config__get_string_buf(out, config, "notes.rewriteref") :
		GIT_ENOTFOUND;

done:
	git_config_free(config);
	return error;
}

static int rebase_copy_note(
	git_rebase *rebase,
	const char *notes_ref,
	git_oid *from,
	git_oid *to,
	const git_signature *committer)
{
	git_note *note = NULL;
	git_oid note_id;
	git_signature *who = NULL;
	int error;

	if ((error = git_note_read(&note, rebase->repo, notes_ref, from)) < 0) {
		if (error == GIT_ENOTFOUND) {
			git_error_clear();
			error = 0;
		}

		goto done;
	}

	if (!committer) {
		if((error = git_signature_default(&who, rebase->repo)) < 0) {
			if (error != GIT_ENOTFOUND ||
				(error = git_signature_now(&who, "unknown", "unknown")) < 0)
				goto done;

			git_error_clear();
		}

		committer = who;
	}

	error = git_note_create(&note_id, rebase->repo, notes_ref,
		git_note_author(note), committer, to, git_note_message(note), 0);

done:
	git_note_free(note);
	git_signature_free(who);

	return error;
}

static int rebase_copy_notes(
	git_rebase *rebase,
	const git_signature *committer)
{
	git_str path = GIT_STR_INIT,
	        rewritten = GIT_STR_INIT,
	        notes_ref = GIT_STR_INIT;
	char *pair_list, *fromstr, *tostr, *end;
	git_oid from, to;
	unsigned int linenum = 1;
	int error = 0;

	if ((error = notes_ref_lookup(&notes_ref, rebase)) < 0) {
		if (error == GIT_ENOTFOUND) {
			git_error_clear();
			error = 0;
		}

		goto done;
	}

	if ((error = git_str_joinpath(&path, rebase->state_path, REWRITTEN_FILE)) < 0 ||
		(error = git_futils_readbuffer(&rewritten, path.ptr)) < 0)
		goto done;

	pair_list = rewritten.ptr;

	while (*pair_list) {
		fromstr = pair_list;

		if ((end = strchr(fromstr, '\n')) == NULL)
			goto on_error;

		pair_list = end+1;
		*end = '\0';

		if ((end = strchr(fromstr, ' ')) == NULL)
			goto on_error;

		tostr = end+1;
		*end = '\0';

		if (strlen(fromstr) != git_oid_hexsize(rebase->repo->oid_type) ||
		    strlen(tostr) != git_oid_hexsize(rebase->repo->oid_type) ||
		    git_oid__fromstr(&from, fromstr, rebase->repo->oid_type) < 0 ||
		    git_oid__fromstr(&to, tostr, rebase->repo->oid_type) < 0)
			goto on_error;

		if ((error = rebase_copy_note(rebase, notes_ref.ptr, &from, &to, committer)) < 0)
			goto done;

		linenum++;
	}

	goto done;

on_error:
	git_error_set(GIT_ERROR_REBASE, "invalid rewritten file at line %d", linenum);
	error = -1;

done:
	git_str_dispose(&rewritten);
	git_str_dispose(&path);
	git_str_dispose(&notes_ref);

	return error;
}

static int return_to_orig_head(git_rebase *rebase)
{
	git_reference *terminal_ref = NULL, *branch_ref = NULL, *head_ref = NULL;
	git_commit *terminal_commit = NULL;
	git_str branch_msg = GIT_STR_INIT, head_msg = GIT_STR_INIT;
	char onto[GIT_OID_MAX_HEXSIZE + 1];
	int error = 0;

	git_oid_tostr(onto, GIT_OID_MAX_HEXSIZE + 1, &rebase->onto_id);

	if ((error = git_str_printf(&branch_msg,
			"rebase finished: %s onto %s", rebase->orig_head_name, onto)) == 0 &&
		(error = git_str_printf(&head_msg,
			"rebase finished: returning to %s", rebase->orig_head_name)) == 0 &&
		(error = git_repository_head(&terminal_ref, rebase->repo)) == 0 &&
		(error = git_reference_peel((git_object **)&terminal_commit,
			terminal_ref, GIT_OBJECT_COMMIT)) == 0 &&
		(error = git_reference_create_matching(&branch_ref,
			rebase->repo, rebase->orig_head_name,
			git_commit_id(terminal_commit), 1,
			&rebase->orig_head_id, branch_msg.ptr)) == 0)
		error = git_reference_symbolic_create(&head_ref,
			rebase->repo, GIT_HEAD_FILE, rebase->orig_head_name, 1,
			head_msg.ptr);

	git_str_dispose(&head_msg);
	git_str_dispose(&branch_msg);
	git_commit_free(terminal_commit);
	git_reference_free(head_ref);
	git_reference_free(branch_ref);
	git_reference_free(terminal_ref);

	return error;
}

int git_rebase_finish(
	git_rebase *rebase,
	const git_signature *signature)
{
	int error = 0;

	GIT_ASSERT_ARG(rebase);

	if (rebase->inmemory)
		return 0;

	if (!rebase->head_detached)
		error = return_to_orig_head(rebase);

	if (error == 0 && (error = rebase_copy_notes(rebase, signature)) == 0)
		error = rebase_cleanup(rebase);

	return error;
}

const char *git_rebase_orig_head_name(git_rebase *rebase) {
	GIT_ASSERT_ARG_WITH_RETVAL(rebase, NULL);
	return rebase->orig_head_name;
}

const git_oid *git_rebase_orig_head_id(git_rebase *rebase) {
	GIT_ASSERT_ARG_WITH_RETVAL(rebase, NULL);
	return &rebase->orig_head_id;
}

const char *git_rebase_onto_name(git_rebase *rebase) {
	GIT_ASSERT_ARG_WITH_RETVAL(rebase, NULL);
	return rebase->onto_name;
}

const git_oid *git_rebase_onto_id(git_rebase *rebase) {
	return &rebase->onto_id;
}

size_t git_rebase_operation_entrycount(git_rebase *rebase)
{
	GIT_ASSERT_ARG_WITH_RETVAL(rebase, 0);

	return git_array_size(rebase->operations);
}

size_t git_rebase_operation_current(git_rebase *rebase)
{
	GIT_ASSERT_ARG_WITH_RETVAL(rebase, 0);

	return rebase->started ? rebase->current : GIT_REBASE_NO_OPERATION;
}

git_rebase_operation *git_rebase_operation_byindex(git_rebase *rebase, size_t idx)
{
	GIT_ASSERT_ARG_WITH_RETVAL(rebase, NULL);

	return git_array_get(rebase->operations, idx);
}

void git_rebase_free(git_rebase *rebase)
{
	if (rebase == NULL)
		return;

	git_index_free(rebase->index);
	git_commit_free(rebase->last_commit);
	git__free(rebase->onto_name);
	git__free(rebase->orig_head_name);
	git__free(rebase->state_path);
	git_str_dispose(&rebase->state_filename);
	git_array_clear(rebase->operations);
	git__free((char *)rebase->options.rewrite_notes_ref);
	git__free(rebase);
}
