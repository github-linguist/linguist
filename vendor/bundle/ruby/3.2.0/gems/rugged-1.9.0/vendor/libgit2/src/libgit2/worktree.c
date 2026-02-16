/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "worktree.h"

#include "buf.h"
#include "repository.h"
#include "path.h"

#include "git2/branch.h"
#include "git2/commit.h"
#include "git2/worktree.h"

static bool is_worktree_dir(const char *dir)
{
	git_str buf = GIT_STR_INIT;
	int error;

	if (git_str_sets(&buf, dir) < 0)
		return -1;

	error = git_fs_path_contains_file(&buf, "commondir")
		&& git_fs_path_contains_file(&buf, "gitdir")
		&& git_fs_path_contains_file(&buf, "HEAD");

	git_str_dispose(&buf);
	return error;
}

int git_worktree_list(git_strarray *wts, git_repository *repo)
{
	git_vector worktrees = GIT_VECTOR_INIT;
	git_str path = GIT_STR_INIT;
	char *worktree;
	size_t i, len;
	int error;

	GIT_ASSERT_ARG(wts);
	GIT_ASSERT_ARG(repo);

	wts->count = 0;
	wts->strings = NULL;

	if ((error = git_str_joinpath(&path, repo->commondir, "worktrees/")) < 0)
		goto exit;
	if (!git_fs_path_exists(path.ptr) || git_fs_path_is_empty_dir(path.ptr))
		goto exit;
	if ((error = git_fs_path_dirload(&worktrees, path.ptr, path.size, 0x0)) < 0)
		goto exit;

	len = path.size;

	git_vector_foreach(&worktrees, i, worktree) {
		git_str_truncate(&path, len);
		git_str_puts(&path, worktree);

		if (!is_worktree_dir(path.ptr)) {
			git_vector_remove(&worktrees, i);
			git__free(worktree);
		}
	}

	wts->strings = (char **)git_vector_detach(&wts->count, NULL, &worktrees);

exit:
	git_str_dispose(&path);

	return error;
}

char *git_worktree__read_link(const char *base, const char *file)
{
	git_str path = GIT_STR_INIT, buf = GIT_STR_INIT;

	GIT_ASSERT_ARG_WITH_RETVAL(base, NULL);
	GIT_ASSERT_ARG_WITH_RETVAL(file, NULL);

	if (git_str_joinpath(&path, base, file) < 0)
		goto err;
	if (git_futils_readbuffer(&buf, path.ptr) < 0)
		goto err;
	git_str_dispose(&path);

	git_str_rtrim(&buf);

	if (!git_fs_path_is_relative(buf.ptr))
		return git_str_detach(&buf);

	if (git_str_sets(&path, base) < 0)
		goto err;
	if (git_fs_path_apply_relative(&path, buf.ptr) < 0)
		goto err;
	git_str_dispose(&buf);

	return git_str_detach(&path);

err:
	git_str_dispose(&buf);
	git_str_dispose(&path);

	return NULL;
}

static int write_wtfile(const char *base, const char *file, const git_str *buf)
{
	git_str path = GIT_STR_INIT;
	int err;

	GIT_ASSERT_ARG(base);
	GIT_ASSERT_ARG(file);
	GIT_ASSERT_ARG(buf);

	if ((err = git_str_joinpath(&path, base, file)) < 0)
		goto out;

	if ((err = git_futils_writebuffer(buf, path.ptr, O_CREAT|O_EXCL|O_WRONLY, 0644)) < 0)
		goto out;

out:
	git_str_dispose(&path);

	return err;
}

static int open_worktree_dir(git_worktree **out, const char *parent, const char *dir, const char *name)
{
	git_str gitdir = GIT_STR_INIT;
	git_worktree *wt = NULL;
	int error = 0;

	if (!is_worktree_dir(dir)) {
		error = -1;
		goto out;
	}

	if ((error = git_path_validate_length(NULL, dir)) < 0)
		goto out;

	if ((wt = git__calloc(1, sizeof(*wt))) == NULL) {
		error = -1;
		goto out;
	}

	if ((wt->name = git__strdup(name)) == NULL ||
	    (wt->commondir_path = git_worktree__read_link(dir, "commondir")) == NULL ||
	    (wt->gitlink_path = git_worktree__read_link(dir, "gitdir")) == NULL ||
	    (parent && (wt->parent_path = git__strdup(parent)) == NULL) ||
	    (wt->worktree_path = git_fs_path_dirname(wt->gitlink_path)) == NULL) {
		error = -1;
		goto out;
	}

	if ((error = git_fs_path_prettify_dir(&gitdir, dir, NULL)) < 0)
		goto out;
	wt->gitdir_path = git_str_detach(&gitdir);

	if ((error = git_worktree_is_locked(NULL, wt)) < 0)
		goto out;
	wt->locked = !!error;
	error = 0;

	*out = wt;

out:
	if (error)
		git_worktree_free(wt);
	git_str_dispose(&gitdir);

	return error;
}

int git_worktree_lookup(git_worktree **out, git_repository *repo, const char *name)
{
	git_str path = GIT_STR_INIT;
	git_worktree *wt = NULL;
	int error;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(name);

	*out = NULL;

	if ((error = git_str_join3(&path, '/', repo->commondir, "worktrees", name)) < 0)
		goto out;

	if (!git_fs_path_isdir(path.ptr)) {
		error = GIT_ENOTFOUND;
		goto out;
	}

	if ((error = (open_worktree_dir(out, git_repository_workdir(repo), path.ptr, name))) < 0)
		goto out;

out:
	git_str_dispose(&path);

	if (error)
		git_worktree_free(wt);

	return error;
}

int git_worktree_open_from_repository(git_worktree **out, git_repository *repo)
{
	git_str parent = GIT_STR_INIT;
	const char *gitdir, *commondir;
	char *name = NULL;
	int error = 0;

	if (!git_repository_is_worktree(repo)) {
		git_error_set(GIT_ERROR_WORKTREE, "cannot open worktree of a non-worktree repo");
		error = -1;
		goto out;
	}

	gitdir = git_repository_path(repo);
	commondir = git_repository_commondir(repo);

	if ((error = git_fs_path_prettify_dir(&parent, "..", commondir)) < 0)
		goto out;

	/* The name is defined by the last component in '.git/worktree/%s' */
	name = git_fs_path_basename(gitdir);

	if ((error = open_worktree_dir(out, parent.ptr, gitdir, name)) < 0)
		goto out;

out:
	git__free(name);
	git_str_dispose(&parent);

	return error;
}

void git_worktree_free(git_worktree *wt)
{
	if (!wt)
		return;

	git__free(wt->commondir_path);
	git__free(wt->worktree_path);
	git__free(wt->gitlink_path);
	git__free(wt->gitdir_path);
	git__free(wt->parent_path);
	git__free(wt->name);
	git__free(wt);
}

int git_worktree_validate(const git_worktree *wt)
{
	GIT_ASSERT_ARG(wt);

	if (!is_worktree_dir(wt->gitdir_path)) {
		git_error_set(GIT_ERROR_WORKTREE,
			"worktree gitdir ('%s') is not valid",
			wt->gitlink_path);
		return GIT_ERROR;
	}

	if (wt->parent_path && !git_fs_path_exists(wt->parent_path)) {
		git_error_set(GIT_ERROR_WORKTREE,
			"worktree parent directory ('%s') does not exist ",
			wt->parent_path);
		return GIT_ERROR;
	}

	if (!git_fs_path_exists(wt->commondir_path)) {
		git_error_set(GIT_ERROR_WORKTREE,
			"worktree common directory ('%s') does not exist ",
			wt->commondir_path);
		return GIT_ERROR;
	}

	if (!git_fs_path_exists(wt->worktree_path)) {
		git_error_set(GIT_ERROR_WORKTREE,
			"worktree directory '%s' does not exist",
			wt->worktree_path);
		return GIT_ERROR;
	}

	return 0;
}

int git_worktree_add_options_init(git_worktree_add_options *opts,
	unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(opts, version,
		git_worktree_add_options, GIT_WORKTREE_ADD_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_worktree_add_init_options(git_worktree_add_options *opts,
	unsigned int version)
{
	return git_worktree_add_options_init(opts, version);
}
#endif

int git_worktree_add(git_worktree **out, git_repository *repo,
	const char *name, const char *worktree,
	const git_worktree_add_options *opts)
{
	git_str gitdir = GIT_STR_INIT, wddir = GIT_STR_INIT, buf = GIT_STR_INIT;
	git_reference *ref = NULL, *head = NULL;
	git_commit *commit = NULL;
	git_repository *wt = NULL;
	git_checkout_options coopts;
	git_worktree_add_options wtopts = GIT_WORKTREE_ADD_OPTIONS_INIT;
	int err;

	GIT_ERROR_CHECK_VERSION(
		opts, GIT_WORKTREE_ADD_OPTIONS_VERSION, "git_worktree_add_options");

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(name);
	GIT_ASSERT_ARG(worktree);

	*out = NULL;

	if (opts)
		memcpy(&wtopts, opts, sizeof(wtopts));

	memcpy(&coopts, &wtopts.checkout_options, sizeof(coopts));

	if (wtopts.ref) {
		if (!git_reference_is_branch(wtopts.ref)) {
			git_error_set(GIT_ERROR_WORKTREE, "reference is not a branch");
			err = -1;
			goto out;
		}

		if ((err = git_reference_dup(&ref, wtopts.ref)) < 0)
			goto out;
	} else if (wtopts.checkout_existing && git_branch_lookup(&ref, repo, name, GIT_BRANCH_LOCAL) == 0) {
		/* Do nothing */
	} else if ((err = git_repository_head(&head, repo)) < 0 ||
		   (err = git_commit_lookup(&commit, repo, &head->target.oid)) < 0 ||
		   (err = git_branch_create(&ref, repo, name, commit, false)) < 0) {
			goto out;
	}

	if (git_branch_is_checked_out(ref)) {
		git_error_set(GIT_ERROR_WORKTREE, "reference %s is already checked out",
			      git_reference_name(ref));
		err = -1;
		goto out;
	}

	/* Create gitdir directory ".git/worktrees/<name>" */
	if ((err = git_str_joinpath(&gitdir, repo->commondir, "worktrees")) < 0)
		goto out;
	if (!git_fs_path_exists(gitdir.ptr))
		if ((err = git_futils_mkdir(gitdir.ptr, 0755, GIT_MKDIR_EXCL)) < 0)
			goto out;
	if ((err = git_str_joinpath(&gitdir, gitdir.ptr, name)) < 0)
		goto out;
	if ((err = git_futils_mkdir(gitdir.ptr, 0755, GIT_MKDIR_EXCL)) < 0)
		goto out;
	if ((err = git_fs_path_prettify_dir(&gitdir, gitdir.ptr, NULL)) < 0)
		goto out;

	/* Create worktree work dir */
	if ((err = git_futils_mkdir(worktree, 0755, GIT_MKDIR_EXCL)) < 0)
		goto out;
	if ((err = git_fs_path_prettify_dir(&wddir, worktree, NULL)) < 0)
		goto out;

	if (wtopts.lock) {
		int fd;

		if ((err = git_str_joinpath(&buf, gitdir.ptr, "locked")) < 0)
			goto out;

		if ((fd = p_creat(buf.ptr, 0644)) < 0) {
			err = fd;
			goto out;
		}

		p_close(fd);
		git_str_clear(&buf);
	}

	/* Create worktree .git file */
	if ((err = git_str_printf(&buf, "gitdir: %s\n", gitdir.ptr)) < 0)
		goto out;
	if ((err = write_wtfile(wddir.ptr, ".git", &buf)) < 0)
		goto out;

	/* Create gitdir files */
	if ((err = git_fs_path_prettify_dir(&buf, repo->commondir, NULL) < 0)
	    || (err = git_str_putc(&buf, '\n')) < 0
	    || (err = write_wtfile(gitdir.ptr, "commondir", &buf)) < 0)
		goto out;
	if ((err = git_str_joinpath(&buf, wddir.ptr, ".git")) < 0
	    || (err = git_str_putc(&buf, '\n')) < 0
	    || (err = write_wtfile(gitdir.ptr, "gitdir", &buf)) < 0)
		goto out;

	/* Set worktree's HEAD */
	if ((err = git_repository_create_head(gitdir.ptr, git_reference_name(ref))) < 0)
		goto out;
	if ((err = git_repository_open(&wt, wddir.ptr)) < 0)
		goto out;

	/* Checkout worktree's HEAD */
	if ((err = git_checkout_head(wt, &coopts)) < 0)
		goto out;

	/* Load result */
	if ((err = git_worktree_lookup(out, repo, name)) < 0)
		goto out;

out:
	git_str_dispose(&gitdir);
	git_str_dispose(&wddir);
	git_str_dispose(&buf);
	git_reference_free(ref);
	git_reference_free(head);
	git_commit_free(commit);
	git_repository_free(wt);

	return err;
}

int git_worktree_lock(git_worktree *wt, const char *reason)
{
	git_str buf = GIT_STR_INIT, path = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(wt);

	if ((error = git_worktree_is_locked(NULL, wt)) < 0)
		goto out;
	if (error) {
		error = GIT_ELOCKED;
		goto out;
	}

	if ((error = git_str_joinpath(&path, wt->gitdir_path, "locked")) < 0)
		goto out;

	if (reason)
		git_str_attach_notowned(&buf, reason, strlen(reason));

	if ((error = git_futils_writebuffer(&buf, path.ptr, O_CREAT|O_EXCL|O_WRONLY, 0644)) < 0)
		goto out;

	wt->locked = 1;

out:
	git_str_dispose(&path);

	return error;
}

int git_worktree_unlock(git_worktree *wt)
{
	git_str path = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(wt);

	if ((error = git_worktree_is_locked(NULL, wt)) < 0)
		return error;
	if (!error)
		return 1;

	if (git_str_joinpath(&path, wt->gitdir_path, "locked") < 0)
		return -1;

	if (p_unlink(path.ptr) != 0) {
		git_str_dispose(&path);
		return -1;
	}

	wt->locked = 0;

	git_str_dispose(&path);

	return 0;
}

static int git_worktree__is_locked(git_str *reason, const git_worktree *wt)
{
	git_str path = GIT_STR_INIT;
	int error, locked;

	GIT_ASSERT_ARG(wt);

	if (reason)
		git_str_clear(reason);

	if ((error = git_str_joinpath(&path, wt->gitdir_path, "locked")) < 0)
		goto out;
	locked = git_fs_path_exists(path.ptr);
	if (locked && reason &&
	    (error = git_futils_readbuffer(reason, path.ptr)) < 0)
		goto out;

	error = locked;
out:
	git_str_dispose(&path);

	return error;
}

int git_worktree_is_locked(git_buf *reason, const git_worktree *wt)
{
	git_str str = GIT_STR_INIT;
	int error = 0;

	if (reason && (error = git_buf_tostr(&str, reason)) < 0)
		return error;

	error = git_worktree__is_locked(reason ? &str : NULL, wt);

	if (error >= 0 && reason) {
		if (git_buf_fromstr(reason, &str) < 0)
			error = -1;
	}

	git_str_dispose(&str);
	return error;
}

const char *git_worktree_name(const git_worktree *wt)
{
	GIT_ASSERT_ARG_WITH_RETVAL(wt, NULL);
	return wt->name;
}

const char *git_worktree_path(const git_worktree *wt)
{
	GIT_ASSERT_ARG_WITH_RETVAL(wt, NULL);
	return wt->worktree_path;
}

int git_worktree_prune_options_init(
	git_worktree_prune_options *opts,
	unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(opts, version,
		git_worktree_prune_options, GIT_WORKTREE_PRUNE_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_worktree_prune_init_options(git_worktree_prune_options *opts,
	unsigned int version)
{
	return git_worktree_prune_options_init(opts, version);
}
#endif

int git_worktree_is_prunable(git_worktree *wt,
	git_worktree_prune_options *opts)
{
	git_worktree_prune_options popts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_str path = GIT_STR_INIT;
	int ret = 0;

	GIT_ERROR_CHECK_VERSION(
		opts, GIT_WORKTREE_PRUNE_OPTIONS_VERSION,
		"git_worktree_prune_options");

	if (opts)
		memcpy(&popts, opts, sizeof(popts));

	if ((popts.flags & GIT_WORKTREE_PRUNE_LOCKED) == 0) {
		git_str reason = GIT_STR_INIT;

		if ((ret = git_worktree__is_locked(&reason, wt)) < 0)
			goto out;

		if (ret) {
			git_error_set(GIT_ERROR_WORKTREE,
				"not pruning locked working tree: '%s'",
				reason.size ?  reason.ptr : "is locked");

			git_str_dispose(&reason);
			ret = 0;
			goto out;
		}
	}

	if ((popts.flags & GIT_WORKTREE_PRUNE_VALID) == 0 &&
	    git_worktree_validate(wt) == 0) {
		git_error_set(GIT_ERROR_WORKTREE, "not pruning valid working tree");
		goto out;
	}

	if ((ret = git_str_printf(&path, "%s/worktrees/%s", wt->commondir_path, wt->name) < 0))
		goto out;

	if (!git_fs_path_exists(path.ptr)) {
		git_error_set(GIT_ERROR_WORKTREE, "worktree gitdir ('%s') does not exist", path.ptr);
		goto out;
	}

	ret = 1;

out:
	git_str_dispose(&path);
	return ret;
}

int git_worktree_prune(git_worktree *wt,
	git_worktree_prune_options *opts)
{
	git_worktree_prune_options popts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_str path = GIT_STR_INIT;
	char *wtpath;
	int err;

	GIT_ERROR_CHECK_VERSION(
		opts, GIT_WORKTREE_PRUNE_OPTIONS_VERSION,
		"git_worktree_prune_options");

	if (opts)
		memcpy(&popts, opts, sizeof(popts));

	if (!git_worktree_is_prunable(wt, &popts)) {
		err = -1;
		goto out;
	}

	/* Delete gitdir in parent repository */
	if ((err = git_str_join3(&path, '/', wt->commondir_path, "worktrees", wt->name)) < 0)
		goto out;
	if (!git_fs_path_exists(path.ptr))
	{
		git_error_set(GIT_ERROR_WORKTREE, "worktree gitdir '%s' does not exist", path.ptr);
		err = -1;
		goto out;
	}
	if ((err = git_futils_rmdir_r(path.ptr, NULL, GIT_RMDIR_REMOVE_FILES)) < 0)
		goto out;

	/* Skip deletion of the actual working tree if it does
	 * not exist or deletion was not requested */
	if ((popts.flags & GIT_WORKTREE_PRUNE_WORKING_TREE) == 0 ||
		!git_fs_path_exists(wt->gitlink_path))
	{
		goto out;
	}

	if ((wtpath = git_fs_path_dirname(wt->gitlink_path)) == NULL)
		goto out;
	git_str_attach(&path, wtpath, 0);
	if (!git_fs_path_exists(path.ptr))
	{
		git_error_set(GIT_ERROR_WORKTREE, "working tree '%s' does not exist", path.ptr);
		err = -1;
		goto out;
	}
	if ((err = git_futils_rmdir_r(path.ptr, NULL, GIT_RMDIR_REMOVE_FILES)) < 0)
		goto out;

out:
	git_str_dispose(&path);

	return err;
}
