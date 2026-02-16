/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "merge_driver.h"

#include "vector.h"
#include "runtime.h"
#include "merge.h"
#include "git2/merge.h"
#include "git2/sys/merge.h"

static const char *merge_driver_name__text = "text";
static const char *merge_driver_name__union = "union";
static const char *merge_driver_name__binary = "binary";

struct merge_driver_registry {
	git_rwlock lock;
	git_vector drivers;
};

typedef struct {
	git_merge_driver *driver;
	int initialized;
	char name[GIT_FLEX_ARRAY];
} git_merge_driver_entry;

static struct merge_driver_registry merge_driver_registry;

static void git_merge_driver_global_shutdown(void);

git_repository *git_merge_driver_source_repo(
	const git_merge_driver_source *src)
{
	GIT_ASSERT_ARG_WITH_RETVAL(src, NULL);
	return src->repo;
}

const git_index_entry *git_merge_driver_source_ancestor(
	const git_merge_driver_source *src)
{
	GIT_ASSERT_ARG_WITH_RETVAL(src, NULL);
	return src->ancestor;
}

const git_index_entry *git_merge_driver_source_ours(
	const git_merge_driver_source *src)
{
	GIT_ASSERT_ARG_WITH_RETVAL(src, NULL);
	return src->ours;
}

const git_index_entry *git_merge_driver_source_theirs(
	const git_merge_driver_source *src)
{
	GIT_ASSERT_ARG_WITH_RETVAL(src, NULL);
	return src->theirs;
}

const git_merge_file_options *git_merge_driver_source_file_options(
	const git_merge_driver_source *src)
{
	GIT_ASSERT_ARG_WITH_RETVAL(src, NULL);
	return src->file_opts;
}

int git_merge_driver__builtin_apply(
	git_merge_driver *self,
	const char **path_out,
	uint32_t *mode_out,
	git_buf *merged_out,
	const char *filter_name,
	const git_merge_driver_source *src)
{
	git_merge_driver__builtin *driver = (git_merge_driver__builtin *)self;
	git_merge_file_options file_opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};
	int error;

	GIT_UNUSED(filter_name);

	if (src->file_opts)
		memcpy(&file_opts, src->file_opts, sizeof(git_merge_file_options));

	if (driver->favor)
		file_opts.favor = driver->favor;

	if ((error = git_merge_file_from_index(&result, src->repo,
		src->ancestor, src->ours, src->theirs, &file_opts)) < 0)
		goto done;

	if (!result.automergeable &&
		!(file_opts.flags & GIT_MERGE_FILE_ACCEPT_CONFLICTS)) {
		error = GIT_EMERGECONFLICT;
		goto done;
	}

	*path_out = git_merge_file__best_path(
		src->ancestor ? src->ancestor->path : NULL,
		src->ours ? src->ours->path : NULL,
		src->theirs ? src->theirs->path : NULL);

	*mode_out = git_merge_file__best_mode(
		src->ancestor ? src->ancestor->mode : 0,
		src->ours ? src->ours->mode : 0,
		src->theirs ? src->theirs->mode : 0);

	merged_out->ptr = (char *)result.ptr;
	merged_out->size = result.len;
	merged_out->reserved = 0;
	result.ptr = NULL;

done:
	git_merge_file_result_free(&result);
	return error;
}

static int merge_driver_binary_apply(
	git_merge_driver *self,
	const char **path_out,
	uint32_t *mode_out,
	git_buf *merged_out,
	const char *filter_name,
	const git_merge_driver_source *src)
{
	GIT_UNUSED(self);
	GIT_UNUSED(path_out);
	GIT_UNUSED(mode_out);
	GIT_UNUSED(merged_out);
	GIT_UNUSED(filter_name);
	GIT_UNUSED(src);

	return GIT_EMERGECONFLICT;
}

static int merge_driver_entry_cmp(const void *a, const void *b)
{
	const git_merge_driver_entry *entry_a = a;
	const git_merge_driver_entry *entry_b = b;

	return strcmp(entry_a->name, entry_b->name);
}

static int merge_driver_entry_search(const void *a, const void *b)
{
	const char *name_a = a;
	const git_merge_driver_entry *entry_b = b;

	return strcmp(name_a, entry_b->name);
}

git_merge_driver__builtin git_merge_driver__text = {
	{
		GIT_MERGE_DRIVER_VERSION,
		NULL,
		NULL,
		git_merge_driver__builtin_apply,
	},
	GIT_MERGE_FILE_FAVOR_NORMAL
};

git_merge_driver__builtin git_merge_driver__union = {
	{
		GIT_MERGE_DRIVER_VERSION,
		NULL,
		NULL,
		git_merge_driver__builtin_apply,
	},
	GIT_MERGE_FILE_FAVOR_UNION
};

git_merge_driver git_merge_driver__binary = {
	GIT_MERGE_DRIVER_VERSION,
	NULL,
	NULL,
	merge_driver_binary_apply
};

/* Note: callers must lock the registry before calling this function */
static int merge_driver_registry_insert(
	const char *name, git_merge_driver *driver)
{
	git_merge_driver_entry *entry;

	entry = git__calloc(1, sizeof(git_merge_driver_entry) + strlen(name) + 1);
	GIT_ERROR_CHECK_ALLOC(entry);

	strcpy(entry->name, name);
	entry->driver = driver;

	return git_vector_insert_sorted(
		&merge_driver_registry.drivers, entry, NULL);
}

int git_merge_driver_global_init(void)
{
	int error;

	if (git_rwlock_init(&merge_driver_registry.lock) < 0)
		return -1;

	if ((error = git_vector_init(&merge_driver_registry.drivers, 3,
		merge_driver_entry_cmp)) < 0)
		goto done;

	if ((error = merge_driver_registry_insert(
			merge_driver_name__text, &git_merge_driver__text.base)) < 0 ||
		(error = merge_driver_registry_insert(
			merge_driver_name__union, &git_merge_driver__union.base)) < 0 ||
		(error = merge_driver_registry_insert(
			merge_driver_name__binary, &git_merge_driver__binary)) < 0)
		goto done;

	error = git_runtime_shutdown_register(git_merge_driver_global_shutdown);

done:
	if (error < 0)
		git_vector_dispose_deep(&merge_driver_registry.drivers);

	return error;
}

static void git_merge_driver_global_shutdown(void)
{
	git_merge_driver_entry *entry;
	size_t i;

	if (git_rwlock_wrlock(&merge_driver_registry.lock) < 0)
		return;

	git_vector_foreach(&merge_driver_registry.drivers, i, entry) {
		if (entry->driver->shutdown)
			entry->driver->shutdown(entry->driver);

		git__free(entry);
	}

	git_vector_dispose(&merge_driver_registry.drivers);

	git_rwlock_wrunlock(&merge_driver_registry.lock);
	git_rwlock_free(&merge_driver_registry.lock);
}

/* Note: callers must lock the registry before calling this function */
static int merge_driver_registry_find(size_t *pos, const char *name)
{
	return git_vector_search2(pos, &merge_driver_registry.drivers,
		merge_driver_entry_search, name);
}

/* Note: callers must lock the registry before calling this function */
static git_merge_driver_entry *merge_driver_registry_lookup(
	size_t *pos, const char *name)
{
	git_merge_driver_entry *entry = NULL;

	if (!merge_driver_registry_find(pos, name))
		entry = git_vector_get(&merge_driver_registry.drivers, *pos);

	return entry;
}

int git_merge_driver_register(const char *name, git_merge_driver *driver)
{
	int error;

	GIT_ASSERT_ARG(name);
	GIT_ASSERT_ARG(driver);

	if (git_rwlock_wrlock(&merge_driver_registry.lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock merge driver registry");
		return -1;
	}

	if (!merge_driver_registry_find(NULL, name)) {
		git_error_set(GIT_ERROR_MERGE, "attempt to reregister existing driver '%s'",
			name);
		error = GIT_EEXISTS;
		goto done;
	}

	error = merge_driver_registry_insert(name, driver);

done:
	git_rwlock_wrunlock(&merge_driver_registry.lock);
	return error;
}

int git_merge_driver_unregister(const char *name)
{
	git_merge_driver_entry *entry;
	size_t pos;
	int error = 0;

	if (git_rwlock_wrlock(&merge_driver_registry.lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock merge driver registry");
		return -1;
	}

	if ((entry = merge_driver_registry_lookup(&pos, name)) == NULL) {
		git_error_set(GIT_ERROR_MERGE, "cannot find merge driver '%s' to unregister",
			name);
		error = GIT_ENOTFOUND;
		goto done;
	}

	git_vector_remove(&merge_driver_registry.drivers, pos);

	if (entry->initialized && entry->driver->shutdown) {
		entry->driver->shutdown(entry->driver);
		entry->initialized = false;
	}

	git__free(entry);

done:
	git_rwlock_wrunlock(&merge_driver_registry.lock);
	return error;
}

git_merge_driver *git_merge_driver_lookup(const char *name)
{
	git_merge_driver_entry *entry;
	size_t pos;
	int error;

	/* If we've decided the merge driver to use internally - and not
	 * based on user configuration (in merge_driver_name_for_path)
	 * then we can use a hardcoded name to compare instead of bothering
	 * to take a lock and look it up in the vector.
	 */
	if (name == merge_driver_name__text)
		return &git_merge_driver__text.base;
	else if (name == merge_driver_name__binary)
		return &git_merge_driver__binary;

	if (git_rwlock_rdlock(&merge_driver_registry.lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock merge driver registry");
		return NULL;
	}

	entry = merge_driver_registry_lookup(&pos, name);

	git_rwlock_rdunlock(&merge_driver_registry.lock);

	if (entry == NULL) {
		git_error_set(GIT_ERROR_MERGE, "cannot use an unregistered filter");
		return NULL;
	}

	if (!entry->initialized) {
		if (entry->driver->initialize &&
			(error = entry->driver->initialize(entry->driver)) < 0)
			return NULL;

		entry->initialized = 1;
	}

	return entry->driver;
}

static int merge_driver_name_for_path(
	const char **out,
	git_repository *repo,
	const char *path,
	const char *default_driver)
{
	const char *value;
	int error;

	*out = NULL;

	if ((error = git_attr_get(&value, repo, 0, path, "merge")) < 0)
		return error;

	/* set: use the built-in 3-way merge driver ("text") */
	if (GIT_ATTR_IS_TRUE(value))
		*out = merge_driver_name__text;

	/* unset: do not merge ("binary") */
	else if (GIT_ATTR_IS_FALSE(value))
		*out = merge_driver_name__binary;

	else if (GIT_ATTR_IS_UNSPECIFIED(value) && default_driver)
		*out = default_driver;

	else if (GIT_ATTR_IS_UNSPECIFIED(value))
		*out = merge_driver_name__text;

	else
		*out = value;

	return 0;
}


GIT_INLINE(git_merge_driver *) merge_driver_lookup_with_wildcard(
	const char *name)
{
	git_merge_driver *driver = git_merge_driver_lookup(name);

	if (driver == NULL)
		driver = git_merge_driver_lookup("*");

	return driver;
}

int git_merge_driver_for_source(
	const char **name_out,
	git_merge_driver **driver_out,
	const git_merge_driver_source *src)
{
	const char *path, *driver_name;
	int error = 0;

	path = git_merge_file__best_path(
		src->ancestor ? src->ancestor->path : NULL,
		src->ours ? src->ours->path : NULL,
		src->theirs ? src->theirs->path : NULL);

	if ((error = merge_driver_name_for_path(
			&driver_name, src->repo, path, src->default_driver)) < 0)
		return error;

	*name_out = driver_name;
	*driver_out = merge_driver_lookup_with_wildcard(driver_name);
	return error;
}

