/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config.h"

#include "config_backend.h"
#include "config_parse.h"
#include "config_list.h"
#include "strlist.h"

typedef struct {
	git_config_backend parent;

	char *backend_type;
	char *origin_path;

	git_config_list *config_list;

	/* Configuration data in the config file format */
	git_str cfg;

	/* Array of key=value pairs */
	char **values;
	size_t values_len;
} config_memory_backend;

typedef struct {
	const char *backend_type;
	const char *origin_path;
	git_config_list *config_list;
	git_config_level_t level;
} config_memory_parse_data;

static int config_error_readonly(void)
{
	git_error_set(GIT_ERROR_CONFIG, "this backend is read-only");
	return -1;
}

static int read_variable_cb(
	git_config_parser *reader,
	const char *current_section,
	const char *var_name,
	const char *var_value,
	const char *line,
	size_t line_len,
	void *payload)
{
	config_memory_parse_data *parse_data = (config_memory_parse_data *) payload;
	git_str buf = GIT_STR_INIT;
	git_config_list_entry *entry;
	const char *c;
	int result;

	GIT_UNUSED(reader);
	GIT_UNUSED(line);
	GIT_UNUSED(line_len);

	if (current_section) {
		/* TODO: Once warnings land, we should likely warn
		 * here. Git appears to warn in most cases if it sees
		 * un-namespaced config options.
		 */
		git_str_puts(&buf, current_section);
		git_str_putc(&buf, '.');
	}

	for (c = var_name; *c; c++)
		git_str_putc(&buf, git__tolower(*c));

	if (git_str_oom(&buf))
		return -1;

	entry = git__calloc(1, sizeof(git_config_list_entry));
	GIT_ERROR_CHECK_ALLOC(entry);
	entry->base.entry.name = git_str_detach(&buf);
	entry->base.entry.value = var_value ? git__strdup(var_value) : NULL;
	entry->base.entry.level = parse_data->level;
	entry->base.entry.include_depth = 0;
	entry->base.entry.backend_type = parse_data->backend_type;
	entry->base.entry.origin_path = parse_data->origin_path;
	entry->base.free = git_config_list_entry_free;
	entry->config_list = parse_data->config_list;

	if ((result = git_config_list_append(parse_data->config_list, entry)) < 0)
		return result;

	return result;
}

static int parse_config(
	config_memory_backend *memory_backend,
	git_config_level_t level)
{
	git_config_parser parser = GIT_PARSE_CTX_INIT;
	config_memory_parse_data parse_data;
	int error;

	if ((error = git_config_parser_init(&parser, "in-memory",
		memory_backend->cfg.ptr, memory_backend->cfg.size)) < 0)
		goto out;

	parse_data.backend_type = git_config_list_add_string(
		memory_backend->config_list, memory_backend->backend_type);
	parse_data.origin_path = memory_backend->origin_path ?
		git_config_list_add_string(memory_backend->config_list,
			memory_backend->origin_path) :
		NULL;
	parse_data.config_list = memory_backend->config_list;
	parse_data.level = level;

	if ((error = git_config_parse(&parser, NULL, read_variable_cb,
		NULL, NULL, &parse_data)) < 0)
		goto out;

out:
	git_config_parser_dispose(&parser);
	return error;
}

static int parse_values(
	config_memory_backend *memory_backend,
	git_config_level_t level)
{
	git_config_list_entry *entry;
	const char *eql, *backend_type, *origin_path;
	size_t name_len, i;

	backend_type = git_config_list_add_string(
		memory_backend->config_list, memory_backend->backend_type);
	GIT_ERROR_CHECK_ALLOC(backend_type);

	origin_path = memory_backend->origin_path ?
		git_config_list_add_string(memory_backend->config_list,
			memory_backend->origin_path) :
		NULL;

	for (i = 0; i < memory_backend->values_len; i++) {
		eql = strchr(memory_backend->values[i], '=');
		name_len = eql - memory_backend->values[i];

		if (name_len == 0) {
			git_error_set(GIT_ERROR_CONFIG, "empty config key");
			return -1;
		}

		entry = git__calloc(1, sizeof(git_config_list_entry));
		GIT_ERROR_CHECK_ALLOC(entry);

		entry->base.entry.name = git__strndup(memory_backend->values[i], name_len);
		GIT_ERROR_CHECK_ALLOC(entry->base.entry.name);

		if (eql) {
			entry->base.entry.value = git__strdup(eql + 1);
			GIT_ERROR_CHECK_ALLOC(entry->base.entry.value);
		}

		entry->base.entry.level = level;
		entry->base.entry.include_depth = 0;
		entry->base.entry.backend_type = backend_type;
		entry->base.entry.origin_path = origin_path;
		entry->base.free = git_config_list_entry_free;
		entry->config_list = memory_backend->config_list;

		if (git_config_list_append(memory_backend->config_list, entry) < 0)
			return -1;
	}

	return 0;
}

static int config_memory_open(git_config_backend *backend, git_config_level_t level, const git_repository *repo)
{
	config_memory_backend *memory_backend = (config_memory_backend *) backend;

	GIT_UNUSED(repo);

	if (memory_backend->cfg.size > 0 &&
	    parse_config(memory_backend, level) < 0)
		return -1;

	if (memory_backend->values_len > 0 &&
	    parse_values(memory_backend, level) < 0)
		return -1;

	return 0;
}

static int config_memory_get(git_config_backend *backend, const char *key, git_config_backend_entry **out)
{
	config_memory_backend *memory_backend = (config_memory_backend *) backend;
	git_config_list_entry *entry;
	int error;

	if ((error = git_config_list_get(&entry, memory_backend->config_list, key)) != 0)
		return error;

	*out = &entry->base;
	return 0;
}

static int config_memory_iterator(
	git_config_iterator **iter,
	git_config_backend *backend)
{
	config_memory_backend *memory_backend = (config_memory_backend *) backend;
	git_config_list *config_list;
	int error;

	if ((error = git_config_list_dup(&config_list, memory_backend->config_list)) < 0)
		goto out;

	if ((error = git_config_list_iterator_new(iter, config_list)) < 0)
		goto out;

out:
	/* Let iterator delete duplicated config_list when it's done */
	git_config_list_free(config_list);
	return error;
}

static int config_memory_set(git_config_backend *backend, const char *name, const char *value)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	GIT_UNUSED(value);
	return config_error_readonly();
}

static int config_memory_set_multivar(
	git_config_backend *backend, const char *name, const char *regexp, const char *value)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);
	GIT_UNUSED(value);
	return config_error_readonly();
}

static int config_memory_delete(git_config_backend *backend, const char *name)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	return config_error_readonly();
}

static int config_memory_delete_multivar(git_config_backend *backend, const char *name, const char *regexp)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);
	return config_error_readonly();
}

static int config_memory_lock(git_config_backend *backend)
{
	GIT_UNUSED(backend);
	return config_error_readonly();
}

static int config_memory_unlock(git_config_backend *backend, int success)
{
	GIT_UNUSED(backend);
	GIT_UNUSED(success);
	return config_error_readonly();
}

static void config_memory_free(git_config_backend *_backend)
{
	config_memory_backend *backend = (config_memory_backend *)_backend;

	if (backend == NULL)
		return;

	git__free(backend->origin_path);
	git__free(backend->backend_type);
	git_config_list_free(backend->config_list);
	git_strlist_free(backend->values, backend->values_len);
	git_str_dispose(&backend->cfg);
	git__free(backend);
}

static config_memory_backend *config_backend_new(
	git_config_backend_memory_options *opts)
{
	config_memory_backend *backend;

	if ((backend = git__calloc(1, sizeof(config_memory_backend))) == NULL)
		return NULL;

	if (git_config_list_new(&backend->config_list) < 0)
		goto on_error;

	backend->parent.version = GIT_CONFIG_BACKEND_VERSION;
	backend->parent.readonly = 1;
	backend->parent.open = config_memory_open;
	backend->parent.get = config_memory_get;
	backend->parent.set = config_memory_set;
	backend->parent.set_multivar = config_memory_set_multivar;
	backend->parent.del = config_memory_delete;
	backend->parent.del_multivar = config_memory_delete_multivar;
	backend->parent.iterator = config_memory_iterator;
	backend->parent.lock = config_memory_lock;
	backend->parent.unlock = config_memory_unlock;
	backend->parent.snapshot = git_config_backend_snapshot;
	backend->parent.free = config_memory_free;

	backend->backend_type = git__strdup(opts && opts->backend_type ?
		opts->backend_type : "in-memory");

	if (backend->backend_type == NULL)
		goto on_error;

	if (opts && opts->origin_path &&
	    (backend->origin_path = git__strdup(opts->origin_path)) == NULL)
		goto on_error;

	return backend;

on_error:
	git_config_list_free(backend->config_list);
	git__free(backend->origin_path);
	git__free(backend->backend_type);
	git__free(backend);
	return NULL;
}

int git_config_backend_from_string(
	git_config_backend **out,
	const char *cfg,
	size_t len,
	git_config_backend_memory_options *opts)
{
	config_memory_backend *backend;

	if ((backend = config_backend_new(opts)) == NULL)
		return -1;

	if (git_str_set(&backend->cfg, cfg, len) < 0) {
		git_config_list_free(backend->config_list);
		git__free(backend);
		return -1;
	}

	*out = (git_config_backend *)backend;
	return 0;
}

int git_config_backend_from_values(
	git_config_backend **out,
	const char **values,
	size_t len,
	git_config_backend_memory_options *opts)
{
	config_memory_backend *backend;

	if ((backend = config_backend_new(opts)) == NULL)
		return -1;

	if (git_strlist_copy(&backend->values, values, len) < 0) {
		git_config_list_free(backend->config_list);
		git__free(backend);
		return -1;
	}

	backend->values_len = len;

	*out = (git_config_backend *)backend;
	return 0;
}
