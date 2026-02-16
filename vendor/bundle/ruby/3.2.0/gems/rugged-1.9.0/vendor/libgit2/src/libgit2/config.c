/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config.h"

#include "git2/config.h"
#include "git2/sys/config.h"

#include "buf.h"
#include "config_backend.h"
#include "regexp.h"
#include "sysdir.h"
#include "transaction.h"
#include "vector.h"
#if GIT_WIN32
# include <windows.h>
#endif

#include <ctype.h>

/*
 * A refcounted instance of a config_backend that can be shared across
 * a configuration instance, any snapshots, and individual configuration
 * levels (from `git_config_open_level`).
 */
typedef struct {
	git_refcount rc;
	git_config_backend *backend;
} backend_instance;

/*
 * An entry in the readers or writers vector in the configuration.
 * This is kept separate from the refcounted instance so that different
 * views of the configuration can have different notions of levels or
 * write orders.
 *
 * (eg, a standard configuration has a priority ordering of writers, a
 * snapshot has *no* writers, and an individual level has a single
 * writer.)
 */
typedef struct {
	backend_instance *instance;
	git_config_level_t level;
	int write_order;
} backend_entry;

void git_config_entry_free(git_config_entry *entry)
{
	git_config_backend_entry *be;

	if (!entry)
		return;

	be = (git_config_backend_entry *)entry;
	be->free(be);
}

static void backend_instance_free(backend_instance *instance)
{
	git_config_backend *backend;

	backend = instance->backend;
	backend->free(backend);
	git__free(instance);
}

static void config_free(git_config *config)
{
	size_t i;
	backend_entry *entry;

	git_vector_foreach(&config->readers, i, entry) {
		GIT_REFCOUNT_DEC(entry->instance, backend_instance_free);
		git__free(entry);
	}

	git_vector_dispose(&config->readers);
	git_vector_dispose(&config->writers);
	git__free(config);
}

void git_config_free(git_config *config)
{
	if (config == NULL)
		return;

	GIT_REFCOUNT_DEC(config, config_free);
}

static int reader_cmp(const void *_a, const void *_b)
{
	const backend_entry *a = _a;
	const backend_entry *b = _b;

	return b->level - a->level;
}

static int writer_cmp(const void *_a, const void *_b)
{
	const backend_entry *a = _a;
	const backend_entry *b = _b;

	return b->write_order - a->write_order;
}

int git_config_new(git_config **out)
{
	git_config *config;

	config = git__calloc(1, sizeof(git_config));
	GIT_ERROR_CHECK_ALLOC(config);

	if (git_vector_init(&config->readers, 8, reader_cmp) < 0 ||
	    git_vector_init(&config->writers, 8, writer_cmp) < 0) {
		config_free(config);
		return -1;
	}

	GIT_REFCOUNT_INC(config);

	*out = config;
	return 0;
}

int git_config_add_file_ondisk(
	git_config *config,
	const char *path,
	git_config_level_t level,
	const git_repository *repo,
	int force)
{
	git_config_backend *file = NULL;
	struct stat st;
	int res;

	GIT_ASSERT_ARG(config);
	GIT_ASSERT_ARG(path);

	res = p_stat(path, &st);
	if (res < 0 && errno != ENOENT && errno != ENOTDIR) {
		git_error_set(GIT_ERROR_CONFIG, "failed to stat '%s'", path);
		return -1;
	}

	if (git_config_backend_from_file(&file, path) < 0)
		return -1;

	if ((res = git_config_add_backend(config, file, level, repo, force)) < 0) {
		/*
		 * free manually; the file is not owned by the config
		 * instance yet and will not be freed on cleanup
		 */
		file->free(file);
		return res;
	}

	return 0;
}

int git_config_open_ondisk(git_config **out, const char *path)
{
	int error;
	git_config *config;

	*out = NULL;

	if (git_config_new(&config) < 0)
		return -1;

	if ((error = git_config_add_file_ondisk(config, path, GIT_CONFIG_LEVEL_LOCAL, NULL, 0)) < 0)
		git_config_free(config);
	else
		*out = config;

	return error;
}

int git_config_snapshot(git_config **out, git_config *in)
{
	int error = 0;
	size_t i;
	backend_entry *entry;
	git_config *config;

	*out = NULL;

	if (git_config_new(&config) < 0)
		return -1;

	git_vector_foreach(&in->readers, i, entry) {
		git_config_backend *b;

		if ((error = entry->instance->backend->snapshot(&b, entry->instance->backend)) < 0)
			break;

		if ((error = git_config_add_backend(config, b, entry->level, NULL, 0)) < 0) {
			b->free(b);
			break;
		}
	}

	git_config_set_writeorder(config, NULL, 0);

	if (error < 0)
		git_config_free(config);
	else
		*out = config;

	return error;
}

static int find_backend_by_level(
	backend_instance **out,
	const git_config *config,
	git_config_level_t level)
{
	backend_entry *entry, *found = NULL;
	size_t i;

	/*
	 * when passing GIT_CONFIG_HIGHEST_LEVEL, the idea is to get the
	 * config backend which has the highest level. As config backends
	 * are stored in a vector sorted by decreasing order of level,
	 * getting the backend at position 0 will do the job.
	 */
	if (level == GIT_CONFIG_HIGHEST_LEVEL) {
		found = git_vector_get(&config->readers, 0);
	} else {
		git_vector_foreach(&config->readers, i, entry) {
			if (entry->level == level) {
				found = entry;
				break;
			}
		}
	}

	if (!found) {
		git_error_set(GIT_ERROR_CONFIG,
			"no configuration exists for the given level '%d'", level);
		return GIT_ENOTFOUND;
	}

	*out = found->instance;
	return 0;
}

static int duplicate_level(void **_old, void *_new)
{
	backend_entry **old = (backend_entry **)_old;

	GIT_UNUSED(_new);

	git_error_set(GIT_ERROR_CONFIG, "configuration at level %d already exists", (*old)->level);
	return GIT_EEXISTS;
}

static void try_remove_existing_backend(
	git_config *config,
	git_config_level_t level)
{
	backend_entry *entry, *found = NULL;
	size_t i;

	git_vector_foreach(&config->readers, i, entry) {
		if (entry->level == level) {
			git_vector_remove(&config->readers, i);
			found = entry;
			break;
		}
	}

	if (!found)
		return;

	git_vector_foreach(&config->writers, i, entry) {
		if (entry->level == level) {
			git_vector_remove(&config->writers, i);
			break;
		}
	}

	GIT_REFCOUNT_DEC(found->instance, backend_instance_free);
	git__free(found);
}

static int git_config__add_instance(
	git_config *config,
	backend_instance *instance,
	git_config_level_t level,
	int force)
{
	backend_entry *entry;
	int result;

	/* delete existing config backend for level if it exists */
	if (force)
		try_remove_existing_backend(config, level);

	entry = git__malloc(sizeof(backend_entry));
	GIT_ERROR_CHECK_ALLOC(entry);

	entry->instance = instance;
	entry->level = level;
	entry->write_order = level;

	if ((result = git_vector_insert_sorted(&config->readers,
			entry, &duplicate_level)) < 0 ||
	    (result = git_vector_insert_sorted(&config->writers,
			entry, NULL)) < 0) {
		git__free(entry);
		return result;
	}

	GIT_REFCOUNT_INC(entry->instance);

	return 0;
}

int git_config_open_global(git_config **out, git_config *config)
{
	int error;

	error = git_config_open_level(out, config, GIT_CONFIG_LEVEL_XDG);

	if (error == 0)
		return 0;
	else if (error != GIT_ENOTFOUND)
		return error;

	return git_config_open_level(out, config, GIT_CONFIG_LEVEL_GLOBAL);
}

int git_config_open_level(
	git_config **out,
	const git_config *parent,
	git_config_level_t level)
{
	git_config *config;
	backend_instance *instance;
	int res;

	if ((res = find_backend_by_level(&instance, parent, level)) < 0)
		return res;

	if ((res = git_config_new(&config)) < 0)
		return res;

	if ((res = git_config__add_instance(config, instance, level, true)) < 0) {
		git_config_free(config);
		return res;
	}

	*out = config;

	return 0;
}

int git_config_add_backend(
	git_config *config,
	git_config_backend *backend,
	git_config_level_t level,
	const git_repository *repo,
	int force)
{
	backend_instance *instance;
	int result;

	GIT_ASSERT_ARG(config);
	GIT_ASSERT_ARG(backend);

	GIT_ERROR_CHECK_VERSION(backend, GIT_CONFIG_BACKEND_VERSION, "git_config_backend");

	if ((result = backend->open(backend, level, repo)) < 0)
		return result;

	instance = git__calloc(1, sizeof(backend_instance));
	GIT_ERROR_CHECK_ALLOC(instance);

	instance->backend = backend;
	instance->backend->cfg = config;

	if ((result = git_config__add_instance(config, instance, level, force)) < 0) {
		git__free(instance);
		return result;
	}

	return 0;
}

int git_config_set_writeorder(
	git_config *config,
	git_config_level_t *levels,
	size_t len)
{
	backend_entry *entry;
	size_t i, j;

	GIT_ASSERT(len < INT_MAX);

	git_vector_foreach(&config->readers, i, entry) {
		bool found = false;

		for (j = 0; j < len; j++) {
			if (levels[j] == entry->level) {
				entry->write_order = (int)j;
				found = true;
				break;
			}
		}

		if (!found)
			entry->write_order = -1;
	}

	git_vector_sort(&config->writers);

	return 0;
}

/*
 * Loop over all the variables
 */

typedef struct {
	git_config_iterator parent;
	git_config_iterator *current;
	const git_config *config;
	git_regexp regex;
	size_t i;
} all_iter;

static int all_iter_next(
	git_config_backend_entry **out,
	git_config_iterator *_iter)
{
	all_iter *iter = (all_iter *) _iter;
	backend_entry *entry;
	git_config_backend *backend;
	git_config_backend_entry *be;
	int error = 0;

	if (iter->current != NULL &&
	    (error = iter->current->next(&be, iter->current)) == 0) {
		*out = be;
		return 0;
	}

	if (error < 0 && error != GIT_ITEROVER)
		return error;

	do {
		if (iter->i == 0)
			return GIT_ITEROVER;

		entry = git_vector_get(&iter->config->readers, iter->i - 1);
		GIT_ASSERT(entry && entry->instance && entry->instance->backend);

		backend = entry->instance->backend;
		iter->i--;

		if (iter->current)
			iter->current->free(iter->current);

		iter->current = NULL;
		error = backend->iterator(&iter->current, backend);

		if (error == GIT_ENOTFOUND)
			continue;

		if (error < 0)
			return error;

		if ((error = iter->current->next(&be, iter->current)) == 0) {
			*out = be;
			return 0;
		}

		/* If this backend is empty, then keep going */
		if (error == GIT_ITEROVER)
			continue;

		return error;

	} while(1);

	return GIT_ITEROVER;
}

static int all_iter_glob_next(
	git_config_backend_entry **entry,
	git_config_iterator *_iter)
{
	int error;
	all_iter *iter = (all_iter *) _iter;

	/*
	 * We use the "normal" function to grab the next one across
	 * readers and then apply the regex
	 */
	while ((error = all_iter_next(entry, _iter)) == 0) {
		/* skip non-matching keys if regexp was provided */
		if (git_regexp_match(&iter->regex, (*entry)->entry.name) != 0)
			continue;

		/* and simply return if we like the entry's name */
		return 0;
	}

	return error;
}

static void all_iter_free(git_config_iterator *_iter)
{
	all_iter *iter = (all_iter *) _iter;

	if (iter->current)
		iter->current->free(iter->current);

	git__free(iter);
}

static void all_iter_glob_free(git_config_iterator *_iter)
{
	all_iter *iter = (all_iter *) _iter;

	git_regexp_dispose(&iter->regex);
	all_iter_free(_iter);
}

int git_config_iterator_new(git_config_iterator **out, const git_config *config)
{
	all_iter *iter;

	iter = git__calloc(1, sizeof(all_iter));
	GIT_ERROR_CHECK_ALLOC(iter);

	iter->parent.free = all_iter_free;
	iter->parent.next = all_iter_next;

	iter->i = config->readers.length;
	iter->config = config;

	*out = (git_config_iterator *) iter;

	return 0;
}

int git_config_iterator_glob_new(git_config_iterator **out, const git_config *config, const char *regexp)
{
	all_iter *iter;
	int result;

	if (regexp == NULL)
		return git_config_iterator_new(out, config);

	iter = git__calloc(1, sizeof(all_iter));
	GIT_ERROR_CHECK_ALLOC(iter);

	if ((result = git_regexp_compile(&iter->regex, regexp, 0)) < 0) {
		git__free(iter);
		return -1;
	}

	iter->parent.next = all_iter_glob_next;
	iter->parent.free = all_iter_glob_free;
	iter->i = config->readers.length;
	iter->config = config;

	*out = (git_config_iterator *) iter;

	return 0;
}

int git_config_foreach(
	const git_config *config, git_config_foreach_cb cb, void *payload)
{
	return git_config_foreach_match(config, NULL, cb, payload);
}

int git_config_backend_foreach_match(
	git_config_backend *backend,
	const char *regexp,
	git_config_foreach_cb cb,
	void *payload)
{
	git_config_backend_entry *entry;
	git_config_iterator *iter;
	git_regexp regex;
	int error = 0;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(cb);

	if (regexp && git_regexp_compile(&regex, regexp, 0) < 0)
		return -1;

	if ((error = backend->iterator(&iter, backend)) < 0) {
		iter = NULL;
		return -1;
	}

	while (!(iter->next(&entry, iter) < 0)) {
		/* skip non-matching keys if regexp was provided */
		if (regexp && git_regexp_match(&regex, entry->entry.name) != 0)
			continue;

		/* abort iterator on non-zero return value */
		if ((error = cb(&entry->entry, payload)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	if (regexp != NULL)
		git_regexp_dispose(&regex);

	iter->free(iter);

	return error;
}

int git_config_foreach_match(
	const git_config *config,
	const char *regexp,
	git_config_foreach_cb cb,
	void *payload)
{
	int error;
	git_config_iterator *iter;
	git_config_entry *entry;

	if ((error = git_config_iterator_glob_new(&iter, config, regexp)) < 0)
		return error;

	while (!(error = git_config_next(&entry, iter))) {
		if ((error = cb(entry, payload)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	git_config_iterator_free(iter);

	if (error == GIT_ITEROVER)
		error = 0;

	return error;
}

/**************
 * Setters
 **************/

 static backend_instance *get_writer_instance(git_config *config)
 {
	backend_entry *entry;
	size_t i;

	git_vector_foreach(&config->writers, i, entry) {
		if (entry->instance->backend->readonly)
			continue;

		if (entry->write_order < 0)
			continue;

		return entry->instance;
	}

	return NULL;
 }

static git_config_backend *get_writer(git_config *config)
{
	backend_instance *instance = get_writer_instance(config);

	return instance ? instance->backend : NULL;
}

int git_config_delete_entry(git_config *config, const char *name)
{
	git_config_backend *backend;

	if ((backend = get_writer(config)) == NULL)
		return GIT_EREADONLY;

	return backend->del(backend, name);
}

int git_config_set_int64(git_config *config, const char *name, int64_t value)
{
	char str_value[32]; /* All numbers should fit in here */
	p_snprintf(str_value, sizeof(str_value), "%" PRId64, value);
	return git_config_set_string(config, name, str_value);
}

int git_config_set_int32(git_config *config, const char *name, int32_t value)
{
	return git_config_set_int64(config, name, (int64_t)value);
}

int git_config_set_bool(git_config *config, const char *name, int value)
{
	return git_config_set_string(config, name, value ? "true" : "false");
}

int git_config_set_string(git_config *config, const char *name, const char *value)
{
	int error;
	git_config_backend *backend;

	if (!value) {
		git_error_set(GIT_ERROR_CONFIG, "the value to set cannot be NULL");
		return -1;
	}

	if ((backend = get_writer(config)) == NULL) {
		git_error_set(GIT_ERROR_CONFIG, "cannot set '%s': the configuration is read-only", name);
		return GIT_EREADONLY;
	}

	error = backend->set(backend, name, value);

	if (!error && GIT_REFCOUNT_OWNER(config) != NULL)
		git_repository__configmap_lookup_cache_clear(GIT_REFCOUNT_OWNER(config));

	return error;
}

int git_config__update_entry(
	git_config *config,
	const char *key,
	const char *value,
	bool overwrite_existing,
	bool only_if_existing)
{
	int error = 0;
	git_config_entry *ce = NULL;

	if ((error = git_config__lookup_entry(&ce, config, key, false)) < 0)
		return error;

	if (!ce && only_if_existing) /* entry doesn't exist */
		return 0;
	if (ce && !overwrite_existing) /* entry would be overwritten */
		return 0;
	if (value && ce && ce->value && !strcmp(ce->value, value)) /* no change */
		return 0;
	if (!value && (!ce || !ce->value)) /* asked to delete absent entry */
		return 0;

	if (!value)
		error = git_config_delete_entry(config, key);
	else
		error = git_config_set_string(config, key, value);

	git_config_entry_free(ce);
	return error;
}

/***********
 * Getters
 ***********/

static int config_error_notfound(const char *name)
{
	git_error_set(GIT_ERROR_CONFIG, "config value '%s' was not found", name);
	return GIT_ENOTFOUND;
}

enum {
	GET_ALL_ERRORS = 0,
	GET_NO_MISSING = 1,
	GET_NO_ERRORS  = 2
};

static int get_entry(
	git_config_entry **out,
	const git_config *config,
	const char *name,
	bool normalize_name,
	int want_errors)
{
	backend_entry *entry;
	git_config_backend *backend;
	git_config_backend_entry *be;
	int res = GIT_ENOTFOUND;
	const char *key = name;
	char *normalized = NULL;
	size_t i;

	*out = NULL;

	if (normalize_name) {
		if ((res = git_config__normalize_name(name, &normalized)) < 0)
			goto cleanup;
		key = normalized;
	}

	res = GIT_ENOTFOUND;
	git_vector_foreach(&config->readers, i, entry) {
		GIT_ASSERT(entry->instance && entry->instance->backend);

		backend = entry->instance->backend;
		res = backend->get(backend, key, &be);

		if (res != GIT_ENOTFOUND) {
			*out = &be->entry;
			break;
		}
	}

	git__free(normalized);

cleanup:
	if (res == GIT_ENOTFOUND) {
		res = (want_errors > GET_ALL_ERRORS) ? 0 : config_error_notfound(name);
	} else if (res && (want_errors == GET_NO_ERRORS)) {
		git_error_clear();
		res = 0;
	}

	return res;
}

int git_config_get_entry(
	git_config_entry **out, const git_config *config, const char *name)
{
	return get_entry(out, config, name, true, GET_ALL_ERRORS);
}

int git_config__lookup_entry(
	git_config_entry **out,
	const git_config *config,
	const char *key,
	bool no_errors)
{
	return get_entry(
		out, config, key, false, no_errors ? GET_NO_ERRORS : GET_NO_MISSING);
}

int git_config_get_mapped(
	int *out,
	const git_config *config,
	const char *name,
	const git_configmap *maps,
	size_t map_n)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, config, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_lookup_map_value(out, maps, map_n, entry->value);
	git_config_entry_free(entry);

	return ret;
}

int git_config_get_int64(int64_t *out, const git_config *config, const char *name)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, config, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_parse_int64(out, entry->value);
	git_config_entry_free(entry);

	return ret;
}

int git_config_get_int32(int32_t *out, const git_config *config, const char *name)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, config, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_parse_int32(out, entry->value);
	git_config_entry_free(entry);

	return ret;
}

int git_config_get_bool(int *out, const git_config *config, const char *name)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, config, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_parse_bool(out, entry->value);
	git_config_entry_free(entry);

	return ret;
}

static int is_readonly(const git_config *config)
{
	backend_entry *entry;
	size_t i;

	git_vector_foreach(&config->writers, i, entry) {
		GIT_ASSERT(entry->instance && entry->instance->backend);

		if (!entry->instance->backend->readonly)
			return 0;
	}

	return 1;
}

static int git_config__parse_path(git_str *out, const char *value)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(value);

	if (value[0] == '~') {
		if (value[1] != '\0' && value[1] != '/') {
			git_error_set(GIT_ERROR_CONFIG, "retrieving a homedir by name is not supported");
			return -1;
		}

		return git_sysdir_expand_homedir_file(out, value[1] ? &value[2] : NULL);
	}

	return git_str_sets(out, value);
}

int git_config_parse_path(git_buf *out, const char *value)
{
	GIT_BUF_WRAP_PRIVATE(out, git_config__parse_path, value);
}

int git_config_get_path(
	git_buf *out,
	const git_config *config,
	const char *name)
{
	GIT_BUF_WRAP_PRIVATE(out, git_config__get_path, config, name);
}

int git_config__get_path(
	git_str *out,
	const git_config *config,
	const char *name)
{
	git_config_entry *entry;
	int error;

	if ((error = get_entry(&entry, config, name, true, GET_ALL_ERRORS)) < 0)
		return error;

	 error = git_config__parse_path(out, entry->value);
	 git_config_entry_free(entry);

	 return error;
}

int git_config_get_string(
	const char **out, const git_config *config, const char *name)
{
	git_config_entry *entry;
	int ret;

	if (!is_readonly(config)) {
		git_error_set(GIT_ERROR_CONFIG, "get_string called on a live config object");
		return -1;
	}

	ret = get_entry(&entry, config, name, true, GET_ALL_ERRORS);
	*out = !ret ? (entry->value ? entry->value : "") : NULL;

	git_config_entry_free(entry);

	return ret;
}

int git_config_get_string_buf(
	git_buf *out, const git_config *config, const char *name)
{
	GIT_BUF_WRAP_PRIVATE(out, git_config__get_string_buf, config, name);
}

int git_config__get_string_buf(
	git_str *out, const git_config *config, const char *name)
{
	git_config_entry *entry;
	int ret;
	const char *str;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(config);

	ret  = get_entry(&entry, config, name, true, GET_ALL_ERRORS);
	str = !ret ? (entry->value ? entry->value : "") : NULL;

	if (str)
		ret = git_str_puts(out, str);

	git_config_entry_free(entry);

	return ret;
}

char *git_config__get_string_force(
	const git_config *config, const char *key, const char *fallback_value)
{
	git_config_entry *entry;
	char *ret;

	get_entry(&entry, config, key, false, GET_NO_ERRORS);
	ret = (entry && entry->value) ? git__strdup(entry->value) : fallback_value ? git__strdup(fallback_value) : NULL;
	git_config_entry_free(entry);

	return ret;
}

int git_config__get_bool_force(
	const git_config *config, const char *key, int fallback_value)
{
	int val = fallback_value;
	git_config_entry *entry;

	get_entry(&entry, config, key, false, GET_NO_ERRORS);

	if (entry && git_config_parse_bool(&val, entry->value) < 0)
		git_error_clear();

	git_config_entry_free(entry);
	return val;
}

int git_config__get_int_force(
	const git_config *config, const char *key, int fallback_value)
{
	int32_t val = (int32_t)fallback_value;
	git_config_entry *entry;

	get_entry(&entry, config, key, false, GET_NO_ERRORS);

	if (entry && git_config_parse_int32(&val, entry->value) < 0)
		git_error_clear();

	git_config_entry_free(entry);
	return (int)val;
}

int git_config_get_multivar_foreach(
	const git_config *config, const char *name, const char *regexp,
	git_config_foreach_cb cb, void *payload)
{
	int err, found;
	git_config_iterator *iter;
	git_config_backend_entry *be;

	if ((err = git_config_multivar_iterator_new(&iter, config, name, regexp)) < 0)
		return err;

	found = 0;
	while ((err = iter->next(&be, iter)) == 0) {
		found = 1;

		if ((err = cb(&be->entry, payload)) != 0) {
			git_error_set_after_callback(err);
			break;
		}
	}

	iter->free(iter);
	if (err == GIT_ITEROVER)
		err = 0;

	if (found == 0 && err == 0)
		err = config_error_notfound(name);

	return err;
}

typedef struct {
	git_config_iterator parent;
	git_config_iterator *iter;
	char *name;
	git_regexp regex;
	int have_regex;
} multivar_iter;

static int multivar_iter_next(
	git_config_backend_entry **entry,
	git_config_iterator *_iter)
{
	multivar_iter *iter = (multivar_iter *) _iter;
	int error = 0;

	while ((error = iter->iter->next(entry, iter->iter)) == 0) {
		if (git__strcmp(iter->name, (*entry)->entry.name))
			continue;

		if (!iter->have_regex)
			return 0;

		if (git_regexp_match(&iter->regex, (*entry)->entry.value) == 0)
			return 0;
	}

	return error;
}

static void multivar_iter_free(git_config_iterator *_iter)
{
	multivar_iter *iter = (multivar_iter *) _iter;

	iter->iter->free(iter->iter);

	git__free(iter->name);
	if (iter->have_regex)
		git_regexp_dispose(&iter->regex);
	git__free(iter);
}

int git_config_multivar_iterator_new(git_config_iterator **out, const git_config *config, const char *name, const char *regexp)
{
	multivar_iter *iter = NULL;
	git_config_iterator *inner = NULL;
	int error;

	if ((error = git_config_iterator_new(&inner, config)) < 0)
		return error;

	iter = git__calloc(1, sizeof(multivar_iter));
	GIT_ERROR_CHECK_ALLOC(iter);

	if ((error = git_config__normalize_name(name, &iter->name)) < 0)
		goto on_error;

	if (regexp != NULL) {
		if ((error = git_regexp_compile(&iter->regex, regexp, 0)) < 0)
			goto on_error;

		iter->have_regex = 1;
	}

	iter->iter = inner;
	iter->parent.free = multivar_iter_free;
	iter->parent.next = multivar_iter_next;

	*out = (git_config_iterator *) iter;

	return 0;

on_error:

	inner->free(inner);
	git__free(iter);
	return error;
}

int git_config_set_multivar(git_config *config, const char *name, const char *regexp, const char *value)
{
	git_config_backend *backend;

	if ((backend = get_writer(config)) == NULL) {
		git_error_set(GIT_ERROR_CONFIG, "cannot set '%s': the configuration is read-only", name);
		return GIT_EREADONLY;
	}

	return backend->set_multivar(backend, name, regexp, value);
}

int git_config_delete_multivar(git_config *config, const char *name, const char *regexp)
{
	git_config_backend *backend;

	if ((backend = get_writer(config)) == NULL)
		return GIT_EREADONLY;

	return backend->del_multivar(backend, name, regexp);
}

int git_config_next(git_config_entry **entry, git_config_iterator *iter)
{
	git_config_backend_entry *be;
	int error;

	if ((error = iter->next(&be, iter)) != 0)
		return error;

	*entry = &be->entry;
	return 0;
}

void git_config_iterator_free(git_config_iterator *iter)
{
	if (iter == NULL)
		return;

	iter->free(iter);
}

int git_config_find_global(git_buf *path)
{
	GIT_BUF_WRAP_PRIVATE(path, git_sysdir_find_global_file, GIT_CONFIG_FILENAME_GLOBAL);
}

int git_config__find_global(git_str *path)
{
	return git_sysdir_find_global_file(path, GIT_CONFIG_FILENAME_GLOBAL);
}

int git_config_find_xdg(git_buf *path)
{
	GIT_BUF_WRAP_PRIVATE(path, git_sysdir_find_xdg_file, GIT_CONFIG_FILENAME_XDG);
}

int git_config__find_xdg(git_str *path)
{
	return git_sysdir_find_xdg_file(path, GIT_CONFIG_FILENAME_XDG);
}

int git_config_find_system(git_buf *path)
{
	GIT_BUF_WRAP_PRIVATE(path, git_sysdir_find_system_file, GIT_CONFIG_FILENAME_SYSTEM);
}

int git_config__find_system(git_str *path)
{
	return git_sysdir_find_system_file(path, GIT_CONFIG_FILENAME_SYSTEM);
}

int git_config_find_programdata(git_buf *path)
{
	git_str str = GIT_STR_INIT;
	int error;

	if ((error = git_buf_tostr(&str, path)) == 0 &&
	    (error = git_config__find_programdata(&str)) == 0)
		error = git_buf_fromstr(path, &str);

	git_str_dispose(&str);
	return error;
}

int git_config__find_programdata(git_str *path)
{
	git_fs_path_owner_t owner_level =
		GIT_FS_PATH_OWNER_CURRENT_USER |
		GIT_FS_PATH_OWNER_ADMINISTRATOR;
	bool is_safe;
	int error;

	if ((error = git_sysdir_find_programdata_file(path, GIT_CONFIG_FILENAME_PROGRAMDATA)) < 0)
		return error;

	if (git_fs_path_owner_is(&is_safe, path->ptr, owner_level) < 0)
		return -1;

	if (!is_safe) {
		git_error_set(GIT_ERROR_CONFIG, "programdata path has invalid ownership");
		return -1;
	}

	return 0;
}

int git_config__global_location(git_str *buf)
{
	const git_str *paths;
	const char *sep, *start;

	if (git_sysdir_get(&paths, GIT_SYSDIR_GLOBAL) < 0)
		return -1;

	/* no paths, so give up */
	if (!paths || !git_str_len(paths))
		return -1;

	/* find unescaped separator or end of string */
	for (sep = start = git_str_cstr(paths); *sep; ++sep) {
		if (*sep == GIT_PATH_LIST_SEPARATOR &&
			(sep <= start || sep[-1] != '\\'))
			break;
	}

	if (git_str_set(buf, start, (size_t)(sep - start)) < 0)
		return -1;

	return git_str_joinpath(buf, buf->ptr, GIT_CONFIG_FILENAME_GLOBAL);
}

int git_config_open_default(git_config **out)
{
	int error;
	git_config *config = NULL;
	git_str buf = GIT_STR_INIT;

	if ((error = git_config_new(&config)) < 0)
		return error;

	if (!git_config__find_global(&buf) ||
	    !git_config__global_location(&buf)) {
		error = git_config_add_file_ondisk(config, buf.ptr,
			GIT_CONFIG_LEVEL_GLOBAL, NULL, 0);
	}

	if (!error && !git_config__find_xdg(&buf))
		error = git_config_add_file_ondisk(config, buf.ptr,
			GIT_CONFIG_LEVEL_XDG, NULL, 0);

	if (!error && !git_config__find_system(&buf))
		error = git_config_add_file_ondisk(config, buf.ptr,
			GIT_CONFIG_LEVEL_SYSTEM, NULL, 0);

	if (!error && !git_config__find_programdata(&buf))
		error = git_config_add_file_ondisk(config, buf.ptr,
			GIT_CONFIG_LEVEL_PROGRAMDATA, NULL, 0);

	git_str_dispose(&buf);

	if (error) {
		git_config_free(config);
		config = NULL;
	}

	*out = config;

	return error;
}

int git_config_lock(git_transaction **out, git_config *config)
{
	backend_instance *instance;
	int error;

	GIT_ASSERT_ARG(config);

	if ((instance = get_writer_instance(config)) == NULL) {
		git_error_set(GIT_ERROR_CONFIG, "cannot lock: the configuration is read-only");
		return GIT_EREADONLY;
	}

	if ((error = instance->backend->lock(instance->backend)) < 0 ||
	    (error = git_transaction_config_new(out, config, instance)) < 0)
		return error;

	GIT_REFCOUNT_INC(instance);
	return 0;
}

int git_config_unlock(
	git_config *config,
	void *data,
	int commit)
{
	backend_instance *instance = data;
	int error;

	GIT_ASSERT_ARG(config && data);
	GIT_UNUSED(config);

	error = instance->backend->unlock(instance->backend, commit);
	GIT_REFCOUNT_DEC(instance, backend_instance_free);

	return error;
}

/***********
 * Parsers
 ***********/

int git_config_lookup_map_value(
	int *out,
	const git_configmap *maps,
	size_t map_n,
	const char *value)
{
	size_t i;

	for (i = 0; i < map_n; ++i) {
		const git_configmap *m = maps + i;

		switch (m->type) {
		case GIT_CONFIGMAP_FALSE:
		case GIT_CONFIGMAP_TRUE: {
			int bool_val;

			if (git_config_parse_bool(&bool_val, value) == 0 &&
				bool_val == (int)m->type) {
				*out = m->map_value;
				return 0;
			}
			break;
		}

		case GIT_CONFIGMAP_INT32:
			if (git_config_parse_int32(out, value) == 0)
				return 0;
			break;

		case GIT_CONFIGMAP_STRING:
			if (value && strcasecmp(value, m->str_match) == 0) {
				*out = m->map_value;
				return 0;
			}
			break;
		}
	}

	git_error_set(GIT_ERROR_CONFIG, "failed to map '%s'", value);
	return -1;
}

int git_config_lookup_map_enum(git_configmap_t *type_out, const char **str_out,
			       const git_configmap *maps, size_t map_n, int enum_val)
{
	size_t i;

	for (i = 0; i < map_n; i++) {
		const git_configmap *m = &maps[i];

		if (m->map_value != enum_val)
			continue;

		*type_out = m->type;
		*str_out = m->str_match;
		return 0;
	}

	git_error_set(GIT_ERROR_CONFIG, "invalid enum value");
	return GIT_ENOTFOUND;
}

int git_config_parse_bool(int *out, const char *value)
{
	if (git__parse_bool(out, value) == 0)
		return 0;

	if (git_config_parse_int32(out, value) == 0) {
		*out = !!(*out);
		return 0;
	}

	git_error_set(GIT_ERROR_CONFIG, "failed to parse '%s' as a boolean value", value);
	return -1;
}

int git_config_parse_int64(int64_t *out, const char *value)
{
	const char *num_end;
	int64_t num;

	if (!value || git__strntol64(&num, value, strlen(value), &num_end, 0) < 0)
		goto fail_parse;

	switch (*num_end) {
	case 'g':
	case 'G':
		num *= 1024;
		/* fallthrough */

	case 'm':
	case 'M':
		num *= 1024;
		/* fallthrough */

	case 'k':
	case 'K':
		num *= 1024;

		/* check that that there are no more characters after the
		 * given modifier suffix */
		if (num_end[1] != '\0')
			return -1;

		/* fallthrough */

	case '\0':
		*out = num;
		return 0;

	default:
		goto fail_parse;
	}

fail_parse:
	git_error_set(GIT_ERROR_CONFIG, "failed to parse '%s' as an integer", value ? value : "(null)");
	return -1;
}

int git_config_parse_int32(int32_t *out, const char *value)
{
	int64_t tmp;
	int32_t truncate;

	if (git_config_parse_int64(&tmp, value) < 0)
		goto fail_parse;

	truncate = tmp & 0xFFFFFFFF;
	if (truncate != tmp)
		goto fail_parse;

	*out = truncate;
	return 0;

fail_parse:
	git_error_set(GIT_ERROR_CONFIG, "failed to parse '%s' as a 32-bit integer", value ? value : "(null)");
	return -1;
}

static int normalize_section(char *start, char *end)
{
	char *scan;

	if (start == end)
		return GIT_EINVALIDSPEC;

	/* Validate and downcase range */
	for (scan = start; *scan; ++scan) {
		if (end && scan >= end)
			break;
		if (git__isalnum(*scan))
			*scan = (char)git__tolower(*scan);
		else if (*scan != '-' || scan == start)
			return GIT_EINVALIDSPEC;
	}

	if (scan == start)
		return GIT_EINVALIDSPEC;

	return 0;
}


/* Take something the user gave us and make it nice for our hash function */
int git_config__normalize_name(const char *in, char **out)
{
	char *name, *fdot, *ldot;

	GIT_ASSERT_ARG(in);
	GIT_ASSERT_ARG(out);

	name = git__strdup(in);
	GIT_ERROR_CHECK_ALLOC(name);

	fdot = strchr(name, '.');
	ldot = strrchr(name, '.');

	if (fdot == NULL || fdot == name || ldot == NULL || !ldot[1])
		goto invalid;

	/* Validate and downcase up to first dot and after last dot */
	if (normalize_section(name, fdot) < 0 ||
	    normalize_section(ldot + 1, NULL) < 0)
		goto invalid;

	/* If there is a middle range, make sure it doesn't have newlines */
	while (fdot < ldot)
		if (*fdot++ == '\n')
			goto invalid;

	*out = name;
	return 0;

invalid:
	git__free(name);
	git_error_set(GIT_ERROR_CONFIG, "invalid config item name '%s'", in);
	return GIT_EINVALIDSPEC;
}

struct rename_data {
	git_config *config;
	git_str *name;
	size_t old_len;
};

static int rename_config_entries_cb(
	const git_config_entry *entry,
	void *payload)
{
	int error = 0;
	struct rename_data *data = (struct rename_data *)payload;
	size_t base_len = git_str_len(data->name);
	git_str value = GIT_STR_INIT;

	if (base_len > 0) {
		if ((error = git_str_puts(data->name,
			entry->name + data->old_len)) < 0 ||
		    (error = git_config_set_multivar(
			data->config, git_str_cstr(data->name), "^$",
			entry->value)) < 0)
			goto cleanup;
	}

	git_str_putc(&value, '^');
	git_str_puts_escape_regex(&value, entry->value);
	git_str_putc(&value, '$');

	if (git_str_oom(&value)) {
		error = -1;
		goto cleanup;
	}

	error = git_config_delete_multivar(
	        data->config, entry->name, git_str_cstr(&value));

 cleanup:
	git_str_truncate(data->name, base_len);
	git_str_dispose(&value);
	return error;
}

int git_config_rename_section(
	git_repository *repo,
	const char *old_section_name,
	const char *new_section_name)
{
	git_config *config;
	git_str pattern = GIT_STR_INIT, replace = GIT_STR_INIT;
	int error = 0;
	struct rename_data data;

	git_str_puts_escape_regex(&pattern, old_section_name);

	if ((error = git_str_puts(&pattern, "\\..+")) < 0)
		goto cleanup;

	if ((error = git_repository_config__weakptr(&config, repo)) < 0)
		goto cleanup;

	data.config  = config;
	data.name    = &replace;
	data.old_len = strlen(old_section_name) + 1;

	if ((error = git_str_join(&replace, '.', new_section_name, "")) < 0)
		goto cleanup;

	if (new_section_name != NULL &&
	    (error = normalize_section(replace.ptr, strchr(replace.ptr, '.'))) < 0)
	{
		git_error_set(
			GIT_ERROR_CONFIG, "invalid config section '%s'", new_section_name);
		goto cleanup;
	}

	error = git_config_foreach_match(
		config, git_str_cstr(&pattern), rename_config_entries_cb, &data);

cleanup:
	git_str_dispose(&pattern);
	git_str_dispose(&replace);

	return error;
}

int git_config_init_backend(git_config_backend *backend, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		backend, version, git_config_backend, GIT_CONFIG_BACKEND_INIT);
	return 0;
}
