/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "clone.h"

#include "git2/clone.h"
#include "git2/remote.h"
#include "git2/revparse.h"
#include "git2/branch.h"
#include "git2/config.h"
#include "git2/checkout.h"
#include "git2/commit.h"
#include "git2/tree.h"

#include "checkout.h"
#include "remote.h"
#include "futils.h"
#include "refs.h"
#include "fs_path.h"
#include "repository.h"
#include "odb.h"
#include "net.h"

static int create_branch(
	git_reference **branch,
	git_repository *repo,
	const git_oid *target,
	const char *name,
	const char *log_message)
{
	git_commit *head_obj = NULL;
	git_reference *branch_ref = NULL;
	git_str refname = GIT_STR_INIT;
	int error;

	/* Find the target commit */
	if ((error = git_commit_lookup(&head_obj, repo, target)) < 0)
		return error;

	/* Create the new branch */
	if ((error = git_str_printf(&refname, GIT_REFS_HEADS_DIR "%s", name)) < 0)
		return error;

	error = git_reference_create(&branch_ref, repo, git_str_cstr(&refname), target, 0, log_message);
	git_str_dispose(&refname);
	git_commit_free(head_obj);

	if (!error)
		*branch = branch_ref;
	else
		git_reference_free(branch_ref);

	return error;
}

static int setup_tracking_config(
	git_repository *repo,
	const char *branch_name,
	const char *remote_name,
	const char *merge_target)
{
	git_config *cfg;
	git_str remote_key = GIT_STR_INIT, merge_key = GIT_STR_INIT;
	int error = -1;

	if (git_repository_config__weakptr(&cfg, repo) < 0)
		return -1;

	if (git_str_printf(&remote_key, "branch.%s.remote", branch_name) < 0)
		goto cleanup;

	if (git_str_printf(&merge_key, "branch.%s.merge", branch_name) < 0)
		goto cleanup;

	if (git_config_set_string(cfg, git_str_cstr(&remote_key), remote_name) < 0)
		goto cleanup;

	if (git_config_set_string(cfg, git_str_cstr(&merge_key), merge_target) < 0)
		goto cleanup;

	error = 0;

cleanup:
	git_str_dispose(&remote_key);
	git_str_dispose(&merge_key);
	return error;
}

static int create_tracking_branch(
	git_reference **branch,
	git_repository *repo,
	const git_oid *target,
	const char *branch_name,
	const char *log_message)
{
	int error;

	if ((error = create_branch(branch, repo, target, branch_name, log_message)) < 0)
		return error;

	return setup_tracking_config(
		repo,
		branch_name,
		GIT_REMOTE_ORIGIN,
		git_reference_name(*branch));
}

static int update_head_to_new_branch(
	git_repository *repo,
	const git_oid *target,
	const char *name,
	const char *reflog_message)
{
	git_reference *tracking_branch = NULL;
	int error;

	if (!git__prefixcmp(name, GIT_REFS_HEADS_DIR))
		name += strlen(GIT_REFS_HEADS_DIR);

	error = create_tracking_branch(&tracking_branch, repo, target, name,
			reflog_message);

	if (!error)
		error = git_repository_set_head(
			repo, git_reference_name(tracking_branch));

	git_reference_free(tracking_branch);

	/* if it already existed, then the user's refspec created it for us, ignore it' */
	if (error == GIT_EEXISTS)
		error = 0;

	return error;
}

static int update_head_to_default(git_repository *repo)
{
	git_str initialbranch = GIT_STR_INIT;
	const char *branch_name;
	int error = 0;

	if ((error = git_repository_initialbranch(&initialbranch, repo)) < 0)
		goto done;

	if (git__prefixcmp(initialbranch.ptr, GIT_REFS_HEADS_DIR) != 0) {
		git_error_set(GIT_ERROR_INVALID, "invalid initial branch '%s'", initialbranch.ptr);
		error = -1;
		goto done;
	}

	branch_name = initialbranch.ptr + strlen(GIT_REFS_HEADS_DIR);

	error = setup_tracking_config(repo, branch_name, GIT_REMOTE_ORIGIN,
		initialbranch.ptr);

done:
	git_str_dispose(&initialbranch);
	return error;
}

static int update_remote_head(
	git_repository *repo,
	git_remote *remote,
	git_str *target,
	const char *reflog_message)
{
	git_refspec *refspec;
	git_reference *remote_head = NULL;
	git_str remote_head_name = GIT_STR_INIT;
	git_str remote_branch_name = GIT_STR_INIT;
	int error;

	/* Determine the remote tracking ref name from the local branch */
	refspec = git_remote__matching_refspec(remote, git_str_cstr(target));

	if (refspec == NULL) {
		git_error_set(GIT_ERROR_NET, "the remote's default branch does not fit the refspec configuration");
		error = GIT_EINVALIDSPEC;
		goto cleanup;
	}

	if ((error = git_refspec__transform(
		&remote_branch_name,
		refspec,
		git_str_cstr(target))) < 0)
		goto cleanup;

	if ((error = git_str_printf(&remote_head_name,
		"%s%s/%s",
		GIT_REFS_REMOTES_DIR,
		git_remote_name(remote),
		GIT_HEAD_FILE)) < 0)
		goto cleanup;

	error = git_reference_symbolic_create(
		&remote_head,
		repo,
		git_str_cstr(&remote_head_name),
		git_str_cstr(&remote_branch_name),
		true,
		reflog_message);

cleanup:
	git_reference_free(remote_head);
	git_str_dispose(&remote_branch_name);
	git_str_dispose(&remote_head_name);
	return error;
}

static int update_head_to_remote(
		git_repository *repo,
		git_remote *remote,
		const char *reflog_message)
{
	int error = 0;
	size_t refs_len;
	const git_remote_head *remote_head, **refs;
	const git_oid *remote_head_id;
	git_str branch = GIT_STR_INIT;

	if ((error = git_remote_ls(&refs, &refs_len, remote)) < 0)
		return error;

	/* We cloned an empty repository or one with an unborn HEAD */
	if (refs_len == 0 || strcmp(refs[0]->name, GIT_HEAD_FILE))
		return update_head_to_default(repo);

	/* We know we have HEAD, let's see where it points */
	remote_head = refs[0];
	GIT_ASSERT(remote_head);

	remote_head_id = &remote_head->oid;

	error = git_remote__default_branch(&branch, remote);
	if (error == GIT_ENOTFOUND) {
		error = git_repository_set_head_detached(
			repo, remote_head_id);
		goto cleanup;
	}

	if ((error = update_remote_head(repo, remote, &branch, reflog_message)) < 0)
		goto cleanup;

	error = update_head_to_new_branch(
		repo,
		remote_head_id,
		git_str_cstr(&branch),
		reflog_message);

cleanup:
	git_str_dispose(&branch);

	return error;
}

static int update_head_to_branch(
		git_repository *repo,
		git_remote *remote,
		const char *branch,
		const char *reflog_message)
{
	int retcode;
	git_str remote_branch_name = GIT_STR_INIT;
	git_reference *remote_ref = NULL;
	git_str default_branch = GIT_STR_INIT;

	GIT_ASSERT_ARG(remote);
	GIT_ASSERT_ARG(branch);

	if ((retcode = git_str_printf(&remote_branch_name, GIT_REFS_REMOTES_DIR "%s/%s",
		git_remote_name(remote), branch)) < 0 )
		goto cleanup;

	if ((retcode = git_reference_lookup(&remote_ref, repo, git_str_cstr(&remote_branch_name))) < 0)
		goto cleanup;

	if ((retcode = update_head_to_new_branch(repo, git_reference_target(remote_ref), branch,
			reflog_message)) < 0)
		goto cleanup;

	retcode = git_remote__default_branch(&default_branch, remote);

	if (retcode == GIT_ENOTFOUND)
		retcode = 0;
	else if (retcode)
		goto cleanup;

	if (!git_remote__matching_refspec(remote, git_str_cstr(&default_branch)))
		goto cleanup;

	retcode = update_remote_head(repo, remote, &default_branch, reflog_message);

cleanup:
	git_reference_free(remote_ref);
	git_str_dispose(&remote_branch_name);
	git_str_dispose(&default_branch);
	return retcode;
}

static int default_repository_create(git_repository **out, const char *path, int bare, void *payload)
{
	GIT_UNUSED(payload);

	return git_repository_init(out, path, bare);
}

static int default_remote_create(
		git_remote **out,
		git_repository *repo,
		const char *name,
		const char *url,
		void *payload)
{
	GIT_UNUSED(payload);

	return git_remote_create(out, repo, name, url);
}

/*
 * submodules?
 */

static int create_and_configure_origin(
		git_remote **out,
		git_repository *repo,
		const char *url,
		const git_clone_options *options)
{
	int error;
	git_remote *origin = NULL;
	char buf[GIT_PATH_MAX];
	git_remote_create_cb remote_create = options->remote_cb;
	void *payload = options->remote_cb_payload;

	/* If the path is local and exists it should be the absolute path. */
	if (!git_net_str_is_url(url) && git_fs_path_root(url) < 0 &&
	    git_fs_path_exists(url)) {
		if (p_realpath(url, buf) == NULL)
			return -1;

		url = buf;
	}

	if (!remote_create) {
		remote_create = default_remote_create;
		payload = NULL;
	}

	if ((error = remote_create(&origin, repo, "origin", url, payload)) < 0)
		goto on_error;

	*out = origin;
	return 0;

on_error:
	git_remote_free(origin);
	return error;
}

static int should_checkout(
	bool *out,
	git_repository *repo,
	bool is_bare,
	const git_clone_options *opts)
{
	int error;

	if (!opts || is_bare ||
	    opts->checkout_opts.checkout_strategy == GIT_CHECKOUT_NONE) {
		*out = false;
		return 0;
	}

	if ((error = git_repository_head_unborn(repo)) < 0)
		return error;

	*out = !error;
	return 0;
}

static int checkout_branch(
	git_repository *repo,
	git_remote *remote,
	const git_clone_options *opts,
	const char *reflog_message)
{
	bool checkout;
	int error;

	if (opts->checkout_branch)
		error = update_head_to_branch(repo, remote, opts->checkout_branch, reflog_message);
	/* Point HEAD to the same ref as the remote's head */
	else
		error = update_head_to_remote(repo, remote, reflog_message);

	if (error < 0)
		return error;

	if ((error = should_checkout(&checkout, repo, git_repository_is_bare(repo), opts)) < 0)
		return error;

	if (checkout)
		error = git_checkout_head(repo, &opts->checkout_opts);

	return error;
}

static int clone_into(
	git_repository *repo,
	git_remote *_remote,
	const git_clone_options *opts)
{
	git_str reflog_message = GIT_STR_INIT;
	git_remote_connect_options connect_opts = GIT_REMOTE_CONNECT_OPTIONS_INIT;
	git_remote *remote;
	git_oid_t oid_type;
	int error;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(_remote);

	if (!git_repository_is_empty(repo)) {
		git_error_set(GIT_ERROR_INVALID, "the repository is not empty");
		return -1;
	}

	if ((error = git_remote_dup(&remote, _remote)) < 0)
		return error;

	if ((error = git_remote_connect_options__from_fetch_opts(&connect_opts, remote, &opts->fetch_opts)) < 0)
		goto cleanup;

	git_str_printf(&reflog_message, "clone: from %s", git_remote_url(remote));

	/*
	 * Connect to the server so that we can identify the remote
	 * object format.
	 */

	if ((error = git_remote_connect_ext(remote, GIT_DIRECTION_FETCH,
			&connect_opts)) < 0)
		goto cleanup;

	if ((error = git_remote_oid_type(&oid_type, remote)) < 0 ||
	    (error = git_repository__set_objectformat(repo, oid_type)) < 0)
		goto cleanup;

	if ((error = git_remote_fetch(remote, NULL, &opts->fetch_opts, git_str_cstr(&reflog_message))) != 0)
		goto cleanup;

	error = checkout_branch(repo, remote, opts, git_str_cstr(&reflog_message));

cleanup:
	git_remote_free(remote);
	git_remote_connect_options_dispose(&connect_opts);
	git_str_dispose(&reflog_message);

	return error;
}

static bool can_link(const char *src, const char *dst, int link)
{
#ifdef GIT_WIN32
	GIT_UNUSED(src);
	GIT_UNUSED(dst);
	GIT_UNUSED(link);
	return false;
#else

	struct stat st_src, st_dst;

	if (!link)
		return false;

	if (p_stat(src, &st_src) < 0)
		return false;

	if (p_stat(dst, &st_dst) < 0)
		return false;

	return st_src.st_dev == st_dst.st_dev;
#endif
}

static int clone_local_into(
	git_repository *repo,
	git_remote *remote,
	const git_clone_options *opts)
{
	int error, flags;
	git_repository *src;
	git_str src_odb = GIT_STR_INIT, dst_odb = GIT_STR_INIT, src_path = GIT_STR_INIT;
	git_str reflog_message = GIT_STR_INIT;
	bool link = (opts && opts->local != GIT_CLONE_LOCAL_NO_LINKS);

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(remote);

	if (!git_repository_is_empty(repo)) {
		git_error_set(GIT_ERROR_INVALID, "the repository is not empty");
		return -1;
	}

	/*
	 * Let's figure out what path we should use for the source
	 * repo, if it's not rooted, the path should be relative to
	 * the repository's worktree/gitdir.
	 */
	if ((error = git_fs_path_from_url_or_path(&src_path, git_remote_url(remote))) < 0)
		return error;

	/* Copy .git/objects/ from the source to the target */
	if ((error = git_repository_open(&src, git_str_cstr(&src_path))) < 0) {
		git_str_dispose(&src_path);
		return error;
	}

	if (git_repository__item_path(&src_odb, src, GIT_REPOSITORY_ITEM_OBJECTS) < 0 ||
	    git_repository__item_path(&dst_odb, repo, GIT_REPOSITORY_ITEM_OBJECTS) < 0) {
		error = -1;
		goto cleanup;
	}

	flags = 0;
	if (can_link(git_repository_path(src), git_repository_path(repo), link))
		flags |= GIT_CPDIR_LINK_FILES;

	error = git_futils_cp_r(git_str_cstr(&src_odb), git_str_cstr(&dst_odb),
				flags, GIT_OBJECT_DIR_MODE);

	/*
	 * can_link() doesn't catch all variations, so if we hit an
	 * error and did want to link, let's try again without trying
	 * to link.
	 */
	if (error < 0 && link) {
		flags &= ~GIT_CPDIR_LINK_FILES;
		error = git_futils_cp_r(git_str_cstr(&src_odb), git_str_cstr(&dst_odb),
					flags, GIT_OBJECT_DIR_MODE);
	}

	if (error < 0)
		goto cleanup;

	git_str_printf(&reflog_message, "clone: from %s", git_remote_url(remote));

	if ((error = git_remote_fetch(remote, NULL, &opts->fetch_opts, git_str_cstr(&reflog_message))) != 0)
		goto cleanup;

	error = checkout_branch(repo, remote, opts, git_str_cstr(&reflog_message));

cleanup:
	git_str_dispose(&reflog_message);
	git_str_dispose(&src_path);
	git_str_dispose(&src_odb);
	git_str_dispose(&dst_odb);
	git_repository_free(src);
	return error;
}

int git_clone__should_clone_local(
	bool *out,
	const char *url_or_path,
	git_clone_local_t local)
{
	git_str fromurl = GIT_STR_INIT;

	*out = false;

	if (local == GIT_CLONE_NO_LOCAL)
		return 0;

	if (git_net_str_is_url(url_or_path)) {
		/* If GIT_CLONE_LOCAL_AUTO is specified, any url should
		 * be treated as remote */
		if (local == GIT_CLONE_LOCAL_AUTO ||
		    !git_fs_path_is_local_file_url(url_or_path))
			return 0;

		if (git_fs_path_fromurl(&fromurl, url_or_path) < 0)
			return -1;

		*out = git_fs_path_isdir(git_str_cstr(&fromurl));
		git_str_dispose(&fromurl);
	} else {
		*out = git_fs_path_isdir(url_or_path);
	}

	return 0;
}

static int clone_repo(
	git_repository **out,
	const char *url,
	const char *local_path,
	const git_clone_options *given_opts,
	int use_existing)
{
	int error = 0;
	git_repository *repo = NULL;
	git_remote *origin;
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;
	uint32_t rmdir_flags = GIT_RMDIR_REMOVE_FILES;
	git_repository_create_cb repository_cb;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(url);
	GIT_ASSERT_ARG(local_path);

	if (given_opts)
		memcpy(&options, given_opts, sizeof(git_clone_options));

	GIT_ERROR_CHECK_VERSION(&options, GIT_CLONE_OPTIONS_VERSION, "git_clone_options");

	/* enforce some behavior on fetch */
	options.fetch_opts.update_fetchhead = 0;

	if (!options.fetch_opts.depth)
		options.fetch_opts.download_tags = GIT_REMOTE_DOWNLOAD_TAGS_ALL;

	/* Only clone to a new directory or an empty directory */
	if (git_fs_path_exists(local_path) && !use_existing && !git_fs_path_is_empty_dir(local_path)) {
		git_error_set(GIT_ERROR_INVALID,
			"'%s' exists and is not an empty directory", local_path);
		return GIT_EEXISTS;
	}

	/* Only remove the root directory on failure if we create it */
	if (git_fs_path_exists(local_path))
		rmdir_flags |= GIT_RMDIR_SKIP_ROOT;

	if (options.repository_cb)
		repository_cb = options.repository_cb;
	else
		repository_cb = default_repository_create;

	if ((error = repository_cb(&repo, local_path, options.bare, options.repository_cb_payload)) < 0)
		return error;

	if (!(error = create_and_configure_origin(&origin, repo, url, &options))) {
		bool clone_local;

		if ((error = git_clone__should_clone_local(&clone_local, url, options.local)) < 0) {
			git_remote_free(origin);
			return error;
		}

		if (clone_local)
			error = clone_local_into(repo, origin, &options);
		else
			error = clone_into(repo, origin, &options);

		git_remote_free(origin);
	}

	if (error != 0) {
		git_error *last_error;
		git_error_save(&last_error);

		git_repository_free(repo);
		repo = NULL;

		(void)git_futils_rmdir_r(local_path, NULL, rmdir_flags);

		git_error_restore(last_error);
	}

	*out = repo;
	return error;
}

int git_clone(
	git_repository **out,
	const char *url,
	const char *local_path,
	const git_clone_options *options)
{
	return clone_repo(out, url, local_path, options, 0);
}

int git_clone__submodule(
	git_repository **out,
	const char *url,
	const char *local_path,
	const git_clone_options *options)
{
	return clone_repo(out, url, local_path, options, 1);
}

int git_clone_options_init(git_clone_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_clone_options, GIT_CLONE_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_clone_init_options(git_clone_options *opts, unsigned int version)
{
	return git_clone_options_init(opts, version);
}
#endif
