/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_config_file_h__
#define INCLUDE_config_file_h__

#include "common.h"

#include "git2/sys/config.h"
#include "git2/config.h"

/**
 * Create a configuration file backend for ondisk files
 *
 * These are the normal `.gitconfig` files that Core Git
 * processes. Note that you first have to add this file to a
 * configuration object before you can query it for configuration
 * variables.
 *
 * @param out the new backend
 * @param path where the config file is located
 */
extern int git_config_backend_from_file(git_config_backend **out, const char *path);

/**
 * Create a readonly configuration file backend from another backend
 *
 * This copies the complete contents of the source backend to the
 * new backend. The new backend will be completely read-only and
 * cannot be modified.
 *
 * @param out the new snapshotted backend
 * @param source the backend to copy
 */
extern int git_config_backend_snapshot(git_config_backend **out, git_config_backend *source);

GIT_INLINE(int) git_config_backend_open(git_config_backend *cfg, unsigned int level, const git_repository *repo)
{
	return cfg->open(cfg, level, repo);
}

GIT_INLINE(void) git_config_backend_free(git_config_backend *cfg)
{
	if (cfg)
		cfg->free(cfg);
}

GIT_INLINE(int) git_config_backend_get_string(
	git_config_entry **out, git_config_backend *cfg, const char *name)
{
	git_config_backend_entry *be;
	int error;

	if ((error = cfg->get(cfg, name, &be)) < 0)
		return error;

	*out = &be->entry;
	return 0;
}

GIT_INLINE(int) git_config_backend_set_string(
	git_config_backend *cfg, const char *name, const char *value)
{
	return cfg->set(cfg, name, value);
}

GIT_INLINE(int) git_config_backend_delete(
	git_config_backend *cfg, const char *name)
{
	return cfg->del(cfg, name);
}

GIT_INLINE(int) git_config_backend_foreach(
	git_config_backend *cfg,
	int (*fn)(const git_config_entry *entry, void *data),
	void *data)
{
	return git_config_backend_foreach_match(cfg, NULL, fn, data);
}

GIT_INLINE(int) git_config_backend_lock(git_config_backend *cfg)
{
	return cfg->lock(cfg);
}

GIT_INLINE(int) git_config_backend_unlock(git_config_backend *cfg, int success)
{
	return cfg->unlock(cfg, success);
}

#endif
