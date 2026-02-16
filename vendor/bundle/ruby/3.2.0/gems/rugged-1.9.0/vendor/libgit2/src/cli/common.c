/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <git2.h>
#include <git2/sys/config.h>

#include "git2_util.h"
#include "vector.h"
#include "fs_path.h"

#include "common.h"
#include "error.h"

static int parse_option(cli_opt *opt, void *data)
{
	git_str kv = GIT_STR_INIT, env = GIT_STR_INIT;
	git_vector *cmdline_config = data;
	int error = 0;

	if (opt->spec && opt->spec->alias == 'c') {
		if (git_str_puts(&kv, opt->value) < 0) {
			error = cli_error_git();
			goto done;
		}
	}

	else if (opt->spec && !strcmp(opt->spec->name, "config-env")) {
		char *val = strchr(opt->value, '=');

		if (val == NULL || *(val + 1) == '\0') {
			error = cli_error("invalid config format: '%s'", opt->value);
			goto done;
		}

		if (git_str_put(&kv, opt->value, (val - opt->value)) < 0) {
			error = cli_error_git();
			goto done;
		}

		val++;

		if ((error = git__getenv(&env, val)) == GIT_ENOTFOUND) {
			error = cli_error("missing environment variable '%s' for configuration '%s'", val, kv.ptr);
			goto done;
		} else if (error) {
			error = cli_error_git();
			goto done;
		}

		if (git_str_putc(&kv, '=') < 0 ||
		    git_str_puts(&kv, env.ptr) < 0) {
			error = cli_error_git();
			goto done;
		}
	}

	if (kv.size > 0 &&
	    git_vector_insert(cmdline_config, git_str_detach(&kv)) < 0)
		error = cli_error_git();

done:
	git_str_dispose(&env);
	git_str_dispose(&kv);
	return error;
}

static int parse_common_options(
	git_repository *repo,
	cli_repository_open_options *opts)
{
	cli_opt_spec common_opts[] = {
		{ CLI_COMMON_OPT_CONFIG },
		{ CLI_COMMON_OPT_CONFIG_ENV },
		{ 0 }
	};
	git_config_backend_memory_options config_opts =
		GIT_CONFIG_BACKEND_MEMORY_OPTIONS_INIT;
	git_vector cmdline = GIT_VECTOR_INIT;
	git_config *config = NULL;
	git_config_backend *backend = NULL;
	int error = 0;

	config_opts.backend_type = "command line";

	if ((error = cli_opt_foreach(common_opts, opts->args,
			opts->args_len, CLI_OPT_PARSE_GNU, parse_option,
			&cmdline)) < 0)
		goto done;

	if (git_vector_length(&cmdline) == 0)
		goto done;

	if (git_repository_config(&config, repo) < 0 ||
	    git_config_backend_from_values(&backend,
			(const char **)cmdline.contents, cmdline.length,
			&config_opts) < 0 ||
	    git_config_add_backend(config, backend, GIT_CONFIG_LEVEL_APP,
			repo, 0) < 0)
		error = cli_error_git();

done:
	if (error && backend)
		backend->free(backend);
	git_config_free(config);
	git_vector_dispose_deep(&cmdline);
	return error;
}

int cli_repository_open(
	git_repository **out,
	cli_repository_open_options *opts)
{
	git_repository *repo;

	if (git_repository_open_ext(&repo, ".", GIT_REPOSITORY_OPEN_FROM_ENV, NULL) < 0)
		return -1;

	if (opts && parse_common_options(repo, opts) < 0)
		return -1;

	*out = repo;
	return 0;
}

/*
 * This resolves paths - not _pathspecs_ like git - it accepts an absolute
 * path (to a path within the repository working directory) or a path
 * relative to the current directory.
 */
int cli_resolve_path(git_str *out, git_repository *repo, const char *given_path)
{
	git_str path = GIT_STR_INIT;
	int error = 0;

	if (!git_fs_path_is_absolute(given_path)) {
		char cwd[GIT_PATH_MAX];

		if (p_getcwd(cwd, GIT_PATH_MAX) < 0)
			error = cli_error_os();
		else if (git_str_puts(&path, cwd) < 0 ||
		         git_fs_path_apply_relative(&path, given_path) < 0)
			error = cli_error_git();

		if (error)
			goto done;
	} else if (git_str_puts(&path, given_path) < 0) {
		error = cli_error_git();
		goto done;
	}

	error = git_fs_path_make_relative(&path, git_repository_workdir(repo));

	if (error == GIT_ENOTFOUND)
		error = cli_error("path '%s' is not inside the git repository '%s'",
			given_path, git_repository_workdir(repo));
	else if (error < 0)
		error = cli_error_git();
	else
		git_str_swap(out, &path);

done:
	git_str_dispose(&path);
	return error;
}
