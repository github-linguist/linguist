/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config_backend.h"

#include "config.h"
#include "config_list.h"

typedef struct {
	git_config_backend parent;
	git_mutex values_mutex;
	git_config_list *config_list;
	git_config_backend *source;
} config_snapshot_backend;

static int config_error_readonly(void)
{
	git_error_set(GIT_ERROR_CONFIG, "this backend is read-only");
	return -1;
}

static int config_snapshot_iterator(
	git_config_iterator **iter,
	struct git_config_backend *backend)
{
	config_snapshot_backend *b = GIT_CONTAINER_OF(backend, config_snapshot_backend, parent);
	git_config_list *config_list = NULL;
	int error;

	if ((error = git_config_list_dup(&config_list, b->config_list)) < 0 ||
	    (error = git_config_list_iterator_new(iter, config_list)) < 0)
		goto out;

out:
	/* Let iterator delete duplicated config_list when it's done */
	git_config_list_free(config_list);
	return error;
}

static int config_snapshot_get(
	git_config_backend *cfg,
	const char *key,
	git_config_backend_entry **out)
{
	config_snapshot_backend *b = GIT_CONTAINER_OF(cfg, config_snapshot_backend, parent);
	git_config_list *config_list = NULL;
	git_config_list_entry *entry;
	int error = 0;

	if (git_mutex_lock(&b->values_mutex) < 0) {
	    git_error_set(GIT_ERROR_OS, "failed to lock config backend");
	    return -1;
	}

	config_list = b->config_list;
	git_config_list_incref(config_list);
	git_mutex_unlock(&b->values_mutex);

	if ((error = (git_config_list_get(&entry, config_list, key))) < 0) {
		git_config_list_free(config_list);
		return error;
	}

	*out = &entry->base;
	return 0;
}

static int config_snapshot_set(git_config_backend *cfg, const char *name, const char *value)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);
	GIT_UNUSED(value);

	return config_error_readonly();
}

static int config_snapshot_set_multivar(
	git_config_backend *cfg, const char *name, const char *regexp, const char *value)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);
	GIT_UNUSED(value);

	return config_error_readonly();
}

static int config_snapshot_delete_multivar(git_config_backend *cfg, const char *name, const char *regexp)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);

	return config_error_readonly();
}

static int config_snapshot_delete(git_config_backend *cfg, const char *name)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);

	return config_error_readonly();
}

static int config_snapshot_lock(git_config_backend *_cfg)
{
	GIT_UNUSED(_cfg);

	return config_error_readonly();
}

static int config_snapshot_unlock(git_config_backend *_cfg, int success)
{
	GIT_UNUSED(_cfg);
	GIT_UNUSED(success);

	return config_error_readonly();
}

static void config_snapshot_free(git_config_backend *_backend)
{
	config_snapshot_backend *backend = GIT_CONTAINER_OF(_backend, config_snapshot_backend, parent);

	if (backend == NULL)
		return;

	git_config_list_free(backend->config_list);
	git_mutex_free(&backend->values_mutex);
	git__free(backend);
}

static int config_snapshot_open(git_config_backend *cfg, git_config_level_t level, const git_repository *repo)
{
	config_snapshot_backend *b = GIT_CONTAINER_OF(cfg, config_snapshot_backend, parent);
	git_config_list *config_list = NULL;
	git_config_iterator *it = NULL;
	git_config_entry *entry;
	int error;

	/* We're just copying data, don't care about the level or repo*/
	GIT_UNUSED(level);
	GIT_UNUSED(repo);

	if ((error = git_config_list_new(&config_list)) < 0 ||
	    (error = b->source->iterator(&it, b->source)) < 0)
		goto out;

	while ((error = git_config_next(&entry, it)) == 0)
		if ((error = git_config_list_dup_entry(config_list, entry)) < 0)
			goto out;

	if (error < 0) {
		if (error != GIT_ITEROVER)
			goto out;
		error = 0;
	}

	b->config_list = config_list;

out:
	git_config_iterator_free(it);
	if (error)
		git_config_list_free(config_list);
	return error;
}

int git_config_backend_snapshot(git_config_backend **out, git_config_backend *source)
{
	config_snapshot_backend *backend;

	backend = git__calloc(1, sizeof(config_snapshot_backend));
	GIT_ERROR_CHECK_ALLOC(backend);

	backend->parent.version = GIT_CONFIG_BACKEND_VERSION;
	git_mutex_init(&backend->values_mutex);

	backend->source = source;

	backend->parent.readonly = 1;
	backend->parent.version = GIT_CONFIG_BACKEND_VERSION;
	backend->parent.open = config_snapshot_open;
	backend->parent.get = config_snapshot_get;
	backend->parent.set = config_snapshot_set;
	backend->parent.set_multivar = config_snapshot_set_multivar;
	backend->parent.snapshot = git_config_backend_snapshot;
	backend->parent.del = config_snapshot_delete;
	backend->parent.del_multivar = config_snapshot_delete_multivar;
	backend->parent.iterator = config_snapshot_iterator;
	backend->parent.lock = config_snapshot_lock;
	backend->parent.unlock = config_snapshot_unlock;
	backend->parent.free = config_snapshot_free;

	*out = &backend->parent;

	return 0;
}
