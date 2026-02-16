/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config.h"

#include "git2/config.h"
#include "git2/sys/config.h"

#include "array.h"
#include "str.h"
#include "config_backend.h"
#include "config_list.h"
#include "config_parse.h"
#include "filebuf.h"
#include "regexp.h"
#include "sysdir.h"
#include "wildmatch.h"
#include "hash.h"

/* Max depth for [include] directives */
#define MAX_INCLUDE_DEPTH 10

#define CONFIG_FILE_TYPE "file"

typedef struct config_file {
	git_futils_filestamp stamp;
	unsigned char checksum[GIT_HASH_SHA256_SIZE];
	char *path;
	git_array_t(struct config_file) includes;
} config_file;

typedef struct {
	git_config_backend parent;
	git_mutex values_mutex;
	git_config_list *config_list;
	const git_repository *repo;
	git_config_level_t level;

	git_array_t(git_config_parser) readers;

	bool locked;
	git_filebuf locked_buf;
	git_str locked_content;

	config_file file;
} config_file_backend;

typedef struct {
	const git_repository *repo;
	config_file *file;
	git_config_list *config_list;
	git_config_level_t level;
	unsigned int depth;
} config_file_parse_data;

static int config_file_read(git_config_list *config_list, const git_repository *repo, config_file *file, git_config_level_t level, int depth);
static int config_file_read_buffer(git_config_list *config_list, const git_repository *repo, config_file *file, git_config_level_t level, int depth, const char *buf, size_t buflen);
static int config_file_write(config_file_backend *cfg, const char *orig_key, const char *key, const git_regexp *preg, const char *value);
static char *escape_value(const char *ptr);

/**
 * Take the current values map from the backend and increase its
 * refcount. This is its own function to make sure we use the mutex to
 * avoid the map pointer from changing under us.
 */
static int config_file_take_list(git_config_list **out, config_file_backend *b)
{
	int error;

	if ((error = git_mutex_lock(&b->values_mutex)) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock config backend");
		return error;
	}

	git_config_list_incref(b->config_list);
	*out = b->config_list;

	git_mutex_unlock(&b->values_mutex);

	return 0;
}

static void config_file_clear(config_file *file)
{
	config_file *include;
	uint32_t i;

	if (file == NULL)
		return;

	git_array_foreach(file->includes, i, include) {
		config_file_clear(include);
	}
	git_array_clear(file->includes);

	git__free(file->path);
}

static int config_file_open(git_config_backend *cfg, git_config_level_t level, const git_repository *repo)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	int res;

	b->level = level;
	b->repo = repo;

	if ((res = git_config_list_new(&b->config_list)) < 0)
		return res;

	if (!git_fs_path_exists(b->file.path))
		return 0;

	/*
	 * git silently ignores configuration files that are not
	 * readable.  We emulate that behavior.  This is particularly
	 * important for sandboxed applications on macOS where the
	 * git configuration files may not be readable.
	 */
	if (p_access(b->file.path, R_OK) < 0)
		return GIT_ENOTFOUND;

	if (res < 0 || (res = config_file_read(b->config_list, repo, &b->file, level, 0)) < 0) {
		git_config_list_free(b->config_list);
		b->config_list = NULL;
	}

	return res;
}

static int config_file_is_modified(int *modified, config_file *file)
{
	config_file *include;
	git_str buf = GIT_STR_INIT;
	unsigned char checksum[GIT_HASH_SHA256_SIZE];
	uint32_t i;
	int error = 0;

	*modified = 0;

	if (!git_futils_filestamp_check(&file->stamp, file->path))
		goto check_includes;

	if ((error = git_futils_readbuffer(&buf, file->path)) < 0)
		goto out;

	if ((error = git_hash_buf(checksum, buf.ptr, buf.size, GIT_HASH_ALGORITHM_SHA256)) < 0)
		goto out;

	if (memcmp(checksum, file->checksum, GIT_HASH_SHA256_SIZE) != 0) {
		*modified = 1;
		goto out;
	}

check_includes:
	git_array_foreach(file->includes, i, include) {
		if ((error = config_file_is_modified(modified, include)) < 0 || *modified)
			goto out;
	}

out:
	git_str_dispose(&buf);

	return error;
}

static void config_file_clear_includes(config_file_backend *cfg)
{
	config_file *include;
	uint32_t i;

	git_array_foreach(cfg->file.includes, i, include)
		config_file_clear(include);
	git_array_clear(cfg->file.includes);
}

static int config_file_set_entries(git_config_backend *cfg, git_config_list *config_list)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_config_list *old = NULL;
	int error;

	if (b->parent.readonly) {
		git_error_set(GIT_ERROR_CONFIG, "this backend is read-only");
		return -1;
	}

	if ((error = git_mutex_lock(&b->values_mutex)) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock config backend");
		goto out;
	}

	old = b->config_list;
	b->config_list = config_list;

	git_mutex_unlock(&b->values_mutex);

out:
	git_config_list_free(old);
	return error;
}

static int config_file_refresh_from_buffer(git_config_backend *cfg, const char *buf, size_t buflen)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_config_list *config_list = NULL;
	int error;

	config_file_clear_includes(b);

	if ((error = git_config_list_new(&config_list)) < 0 ||
	    (error = config_file_read_buffer(config_list, b->repo, &b->file,
					     b->level, 0, buf, buflen)) < 0 ||
	    (error = config_file_set_entries(cfg, config_list)) < 0)
		goto out;

	config_list = NULL;
out:
	git_config_list_free(config_list);
	return error;
}

static int config_file_refresh(git_config_backend *cfg)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_config_list *config_list = NULL;
	int error, modified;

	if (cfg->readonly)
		return 0;

	if ((error = config_file_is_modified(&modified, &b->file)) < 0 && error != GIT_ENOTFOUND)
		goto out;

	if (!modified)
		return 0;

	config_file_clear_includes(b);

	if ((error = git_config_list_new(&config_list)) < 0 ||
	    (error = config_file_read(config_list, b->repo, &b->file, b->level, 0)) < 0 ||
	    (error = config_file_set_entries(cfg, config_list)) < 0)
		goto out;

	config_list = NULL;
out:
	git_config_list_free(config_list);

	return (error == GIT_ENOTFOUND) ? 0 : error;
}

static void config_file_free(git_config_backend *_backend)
{
	config_file_backend *backend = GIT_CONTAINER_OF(_backend, config_file_backend, parent);

	if (backend == NULL)
		return;

	config_file_clear(&backend->file);
	git_config_list_free(backend->config_list);
	git_mutex_free(&backend->values_mutex);
	git__free(backend);
}

static int config_file_iterator(
	git_config_iterator **iter,
	struct git_config_backend *backend)
{
	config_file_backend *b = GIT_CONTAINER_OF(backend, config_file_backend, parent);
	git_config_list *dupped = NULL, *config_list = NULL;
	int error;

	if ((error = config_file_refresh(backend)) < 0 ||
	    (error = config_file_take_list(&config_list, b)) < 0 ||
	    (error = git_config_list_dup(&dupped, config_list)) < 0 ||
	    (error = git_config_list_iterator_new(iter, dupped)) < 0)
		goto out;

out:
	/* Let iterator delete duplicated config_list when it's done */
	git_config_list_free(config_list);
	git_config_list_free(dupped);
	return error;
}

static int config_file_snapshot(git_config_backend **out, git_config_backend *backend)
{
	return git_config_backend_snapshot(out, backend);
}

static int config_file_set(git_config_backend *cfg, const char *name, const char *value)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_config_list *config_list;
	git_config_list_entry *existing;
	char *key, *esc_value = NULL;
	int error;

	if ((error = git_config__normalize_name(name, &key)) < 0)
		return error;

	if ((error = config_file_take_list(&config_list, b)) < 0)
		return error;

	/* Check whether we'd be modifying an included or multivar key */
	if ((error = git_config_list_get_unique(&existing, config_list, key)) < 0) {
		if (error != GIT_ENOTFOUND)
			goto out;
		error = 0;
	} else if ((!existing->base.entry.value && !value) ||
		   (existing->base.entry.value && value && !strcmp(existing->base.entry.value, value))) {
		/* don't update if old and new values already match */
		error = 0;
		goto out;
	}

	/* No early returns due to sanity checks, let's write it out and refresh */
	if (value) {
		esc_value = escape_value(value);
		GIT_ERROR_CHECK_ALLOC(esc_value);
	}

	if ((error = config_file_write(b, name, key, NULL, esc_value)) < 0)
		goto out;

out:
	git_config_list_free(config_list);
	git__free(esc_value);
	git__free(key);
	return error;
}

/*
 * Internal function that actually gets the value in string form
 */
static int config_file_get(
	git_config_backend *cfg,
	const char *key,
	git_config_backend_entry **out)
{
	config_file_backend *h = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_config_list *config_list = NULL;
	git_config_list_entry *entry;
	int error = 0;

	if (!h->parent.readonly && ((error = config_file_refresh(cfg)) < 0))
		return error;

	if ((error = config_file_take_list(&config_list, h)) < 0)
		return error;

	if ((error = (git_config_list_get(&entry, config_list, key))) < 0) {
		git_config_list_free(config_list);
		return error;
	}

	*out = &entry->base;

	return 0;
}

static int config_file_set_multivar(
	git_config_backend *cfg, const char *name, const char *regexp, const char *value)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_regexp preg;
	int result;
	char *key;

	GIT_ASSERT_ARG(regexp);

	if ((result = git_config__normalize_name(name, &key)) < 0)
		return result;

	if ((result = git_regexp_compile(&preg, regexp, 0)) < 0)
		goto out;

	/* If we do have it, set call config_file_write() and reload */
	if ((result = config_file_write(b, name, key, &preg, value)) < 0)
		goto out;

out:
	git__free(key);
	git_regexp_dispose(&preg);

	return result;
}

static int config_file_delete(git_config_backend *cfg, const char *name)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_config_list *config_list = NULL;
	git_config_list_entry *entry;
	char *key = NULL;
	int error;

	if ((error = git_config__normalize_name(name, &key)) < 0)
		goto out;

	if ((error = config_file_take_list(&config_list, b)) < 0)
		goto out;

	/* Check whether we'd be modifying an included or multivar key */
	if ((error = git_config_list_get_unique(&entry, config_list, key)) < 0) {
		if (error == GIT_ENOTFOUND)
			git_error_set(GIT_ERROR_CONFIG, "could not find key '%s' to delete", name);
		goto out;
	}

	if ((error = config_file_write(b, name, entry->base.entry.name, NULL, NULL)) < 0)
		goto out;

out:
	git_config_list_free(config_list);
	git__free(key);
	return error;
}

static int config_file_delete_multivar(git_config_backend *cfg, const char *name, const char *regexp)
{
	config_file_backend *b = GIT_CONTAINER_OF(cfg, config_file_backend, parent);
	git_config_list *config_list = NULL;
	git_config_list_entry *entry = NULL;
	git_regexp preg = GIT_REGEX_INIT;
	char *key = NULL;
	int result;

	if ((result = git_config__normalize_name(name, &key)) < 0)
		goto out;

	if ((result = config_file_take_list(&config_list, b)) < 0)
		goto out;

	if ((result = git_config_list_get(&entry, config_list, key)) < 0) {
		if (result == GIT_ENOTFOUND)
			git_error_set(GIT_ERROR_CONFIG, "could not find key '%s' to delete", name);
		goto out;
	}

	if ((result = git_regexp_compile(&preg, regexp, 0)) < 0)
		goto out;

	if ((result = config_file_write(b, name, key, &preg, NULL)) < 0)
		goto out;

out:
	git_config_list_free(config_list);
	git__free(key);
	git_regexp_dispose(&preg);
	return result;
}

static int config_file_lock(git_config_backend *_cfg)
{
	config_file_backend *cfg = GIT_CONTAINER_OF(_cfg, config_file_backend, parent);
	int error;

	if ((error = git_filebuf_open(&cfg->locked_buf, cfg->file.path, 0, GIT_CONFIG_FILE_MODE)) < 0)
		return error;

	error = git_futils_readbuffer(&cfg->locked_content, cfg->file.path);
	if (error < 0 && error != GIT_ENOTFOUND) {
		git_filebuf_cleanup(&cfg->locked_buf);
		return error;
	}

	cfg->locked = true;
	return 0;

}

static int config_file_unlock(git_config_backend *_cfg, int success)
{
	config_file_backend *cfg = GIT_CONTAINER_OF(_cfg, config_file_backend, parent);
	int error = 0;

	if (success) {
		git_filebuf_write(&cfg->locked_buf, cfg->locked_content.ptr, cfg->locked_content.size);
		error = git_filebuf_commit(&cfg->locked_buf);
	}

	git_filebuf_cleanup(&cfg->locked_buf);
	git_str_dispose(&cfg->locked_content);
	cfg->locked = false;

	return error;
}

int git_config_backend_from_file(git_config_backend **out, const char *path)
{
	config_file_backend *backend;

	backend = git__calloc(1, sizeof(config_file_backend));
	GIT_ERROR_CHECK_ALLOC(backend);

	backend->parent.version = GIT_CONFIG_BACKEND_VERSION;
	git_mutex_init(&backend->values_mutex);

	backend->file.path = git__strdup(path);
	GIT_ERROR_CHECK_ALLOC(backend->file.path);
	git_array_init(backend->file.includes);

	backend->parent.open = config_file_open;
	backend->parent.get = config_file_get;
	backend->parent.set = config_file_set;
	backend->parent.set_multivar = config_file_set_multivar;
	backend->parent.del = config_file_delete;
	backend->parent.del_multivar = config_file_delete_multivar;
	backend->parent.iterator = config_file_iterator;
	backend->parent.snapshot = config_file_snapshot;
	backend->parent.lock = config_file_lock;
	backend->parent.unlock = config_file_unlock;
	backend->parent.free = config_file_free;

	*out = (git_config_backend *)backend;

	return 0;
}

static int included_path(git_str *out, const char *dir, const char *path)
{
	/* From the user's home */
	if (path[0] == '~' && path[1] == '/')
		return git_sysdir_expand_homedir_file(out, &path[1]);

	return git_fs_path_join_unrooted(out, path, dir, NULL);
}

/* Escape the values to write them to the file */
static char *escape_value(const char *ptr)
{
	git_str buf;
	size_t len;
	const char *esc;

	GIT_ASSERT_ARG_WITH_RETVAL(ptr, NULL);

	len = strlen(ptr);
	if (!len)
		return git__calloc(1, sizeof(char));

	if (git_str_init(&buf, len) < 0)
		return NULL;

	while (*ptr != '\0') {
		if ((esc = strchr(git_config_escaped, *ptr)) != NULL) {
			git_str_putc(&buf, '\\');
			git_str_putc(&buf, git_config_escapes[esc - git_config_escaped]);
		} else {
			git_str_putc(&buf, *ptr);
		}
		ptr++;
	}

	if (git_str_oom(&buf))
		return NULL;

	return git_str_detach(&buf);
}

static int parse_include(config_file_parse_data *parse_data, const char *file)
{
	config_file *include;
	git_str path = GIT_STR_INIT;
	char *dir;
	int result;

	if (!file)
		return 0;

	if ((result = git_fs_path_dirname_r(&path, parse_data->file->path)) < 0)
		return result;

	dir = git_str_detach(&path);
	result = included_path(&path, dir, file);
	git__free(dir);

	if (result < 0)
		return result;

	include = git_array_alloc(parse_data->file->includes);
	GIT_ERROR_CHECK_ALLOC(include);
	memset(include, 0, sizeof(*include));
	git_array_init(include->includes);
	include->path = git_str_detach(&path);

	result = config_file_read(parse_data->config_list, parse_data->repo, include,
				  parse_data->level, parse_data->depth+1);

	if (result == GIT_ENOTFOUND) {
		git_error_clear();
		result = 0;
	}

	return result;
}

static int do_match_gitdir(
	int *matches,
	const git_repository *repo,
	const char *cfg_file,
	const char *condition,
	bool case_insensitive)
{
	git_str pattern = GIT_STR_INIT, gitdir = GIT_STR_INIT;
	int error;

	if (condition[0] == '.' && git_fs_path_is_dirsep(condition[1])) {
		git_fs_path_dirname_r(&pattern, cfg_file);
		git_str_joinpath(&pattern, pattern.ptr, condition + 2);
	} else if (condition[0] == '~' && git_fs_path_is_dirsep(condition[1]))
		git_sysdir_expand_homedir_file(&pattern, condition + 1);
	else if (!git_fs_path_is_absolute(condition))
		git_str_joinpath(&pattern, "**", condition);
	else
		git_str_sets(&pattern, condition);

	if (git_fs_path_is_dirsep(condition[strlen(condition) - 1]))
		git_str_puts(&pattern, "**");

	if (git_str_oom(&pattern)) {
		error = -1;
		goto out;
	}

	if ((error = git_repository__item_path(&gitdir, repo, GIT_REPOSITORY_ITEM_GITDIR)) < 0)
		goto out;

	if (git_fs_path_is_dirsep(gitdir.ptr[gitdir.size - 1]))
		git_str_truncate(&gitdir, gitdir.size - 1);

	*matches = wildmatch(pattern.ptr, gitdir.ptr,
			     WM_PATHNAME | (case_insensitive ? WM_CASEFOLD : 0)) == WM_MATCH;
out:
	git_str_dispose(&pattern);
	git_str_dispose(&gitdir);
	return error;
}

static int conditional_match_gitdir(
	int *matches,
	const git_repository *repo,
	const char *cfg_file,
	const char *value)
{
	return do_match_gitdir(matches, repo, cfg_file, value, false);
}

static int conditional_match_gitdir_i(
	int *matches,
	const git_repository *repo,
	const char *cfg_file,
	const char *value)
{
	return do_match_gitdir(matches, repo, cfg_file, value, true);
}

static int conditional_match_onbranch(
	int *matches,
	const git_repository *repo,
	const char *cfg_file,
	const char *condition)
{
	git_str reference = GIT_STR_INIT, buf = GIT_STR_INIT;
	int error;

	GIT_UNUSED(cfg_file);

	/*
	 * NOTE: you cannot use `git_repository_head` here. Looking up the
	 * HEAD reference will create the ODB, which causes us to read the
	 * repo's config for keys like core.precomposeUnicode. As we're
	 * just parsing the config right now, though, this would result in
	 * an endless recursion.
	 */

	if ((error = git_str_joinpath(&buf, git_repository_path(repo), GIT_HEAD_FILE)) < 0 ||
	    (error = git_futils_readbuffer(&reference, buf.ptr)) < 0)
		goto out;
	git_str_rtrim(&reference);

	if (git__strncmp(reference.ptr, GIT_SYMREF, strlen(GIT_SYMREF)))
		goto out;
	git_str_consume(&reference, reference.ptr + strlen(GIT_SYMREF));

	if (git__strncmp(reference.ptr, GIT_REFS_HEADS_DIR, strlen(GIT_REFS_HEADS_DIR)))
		goto out;
	git_str_consume(&reference, reference.ptr + strlen(GIT_REFS_HEADS_DIR));

	/*
	 * If the condition ends with a '/', then we should treat it as if
	 * it had '**' appended.
	 */
	if ((error = git_str_sets(&buf, condition)) < 0)
		goto out;
	if (git_fs_path_is_dirsep(condition[strlen(condition) - 1]) &&
	    (error = git_str_puts(&buf, "**")) < 0)
		goto out;

	*matches = wildmatch(buf.ptr, reference.ptr, WM_PATHNAME) == WM_MATCH;
out:
	git_str_dispose(&reference);
	git_str_dispose(&buf);

	return error;

}

static const struct {
	const char *prefix;
	int (*matches)(int *matches, const git_repository *repo, const char *cfg, const char *value);
} conditions[] = {
	{ "gitdir:", conditional_match_gitdir },
	{ "gitdir/i:", conditional_match_gitdir_i },
	{ "onbranch:", conditional_match_onbranch }
};

static int parse_conditional_include(config_file_parse_data *parse_data, const char *section, const char *file)
{
	char *condition;
	size_t section_len, i;
	int error = 0, matches;

	if (!parse_data->repo || !file)
		return 0;

	section_len = strlen(section);

	/*
	 * We checked that the string starts with `includeIf.` and ends
	 * in `.path` to get here.  Make sure it consists of more.
	 */
	if (section_len < CONST_STRLEN("includeIf.") + CONST_STRLEN(".path"))
		return 0;

	condition = git__substrdup(section + CONST_STRLEN("includeIf."),
		section_len - CONST_STRLEN("includeIf.") - CONST_STRLEN(".path"));

	GIT_ERROR_CHECK_ALLOC(condition);

	for (i = 0; i < ARRAY_SIZE(conditions); i++) {
		if (git__prefixcmp(condition, conditions[i].prefix))
			continue;

		if ((error = conditions[i].matches(&matches,
						   parse_data->repo,
						   parse_data->file->path,
						   condition + strlen(conditions[i].prefix))) < 0)
			break;

		if (matches)
			error = parse_include(parse_data, file);

		break;
	}

	git__free(condition);
	return error;
}

static int read_on_variable(
	git_config_parser *reader,
	const char *current_section,
	const char *var_name,
	const char *var_value,
	const char *line,
	size_t line_len,
	void *data)
{
	config_file_parse_data *parse_data = (config_file_parse_data *)data;
	git_str buf = GIT_STR_INIT;
	git_config_list_entry *entry;
	const char *c;
	int result = 0;

	GIT_UNUSED(reader);
	GIT_UNUSED(line);
	GIT_UNUSED(line_len);

	if (current_section) {
		/* TODO: Once warnings lang, we should likely warn
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
	GIT_ERROR_CHECK_ALLOC(entry->base.entry.name);

	if (var_value) {
		entry->base.entry.value = git__strdup(var_value);
		GIT_ERROR_CHECK_ALLOC(entry->base.entry.value);
	}

	entry->base.entry.backend_type = git_config_list_add_string(parse_data->config_list, CONFIG_FILE_TYPE);
	GIT_ERROR_CHECK_ALLOC(entry->base.entry.backend_type);

	entry->base.entry.origin_path = git_config_list_add_string(parse_data->config_list, parse_data->file->path);
	GIT_ERROR_CHECK_ALLOC(entry->base.entry.origin_path);

	entry->base.entry.level = parse_data->level;
	entry->base.entry.include_depth = parse_data->depth;
	entry->base.free = git_config_list_entry_free;
	entry->config_list = parse_data->config_list;

	if ((result = git_config_list_append(parse_data->config_list, entry)) < 0)
		return result;

	result = 0;

	/* Add or append the new config option */
	if (!git__strcmp(entry->base.entry.name, "include.path"))
		result = parse_include(parse_data, entry->base.entry.value);
	else if (!git__prefixcmp(entry->base.entry.name, "includeif.") &&
	         !git__suffixcmp(entry->base.entry.name, ".path"))
		result = parse_conditional_include(parse_data, entry->base.entry.name, entry->base.entry.value);

	return result;
}

static int config_file_read_buffer(
	git_config_list *config_list,
	const git_repository *repo,
	config_file *file,
	git_config_level_t level,
	int depth,
	const char *buf,
	size_t buflen)
{
	config_file_parse_data parse_data;
	git_config_parser reader;
	int error;

	if (depth >= MAX_INCLUDE_DEPTH) {
		git_error_set(GIT_ERROR_CONFIG, "maximum config include depth reached");
		return -1;
	}

	/* Initialize the reading position */
	reader.path = file->path;
	git_parse_ctx_init(&reader.ctx, buf, buflen);

	/* If the file is empty, there's nothing for us to do */
	if (!reader.ctx.content || *reader.ctx.content == '\0') {
		error = 0;
		goto out;
	}

	parse_data.repo = repo;
	parse_data.file = file;
	parse_data.config_list = config_list;
	parse_data.level = level;
	parse_data.depth = depth;

	error = git_config_parse(&reader, NULL, read_on_variable, NULL, NULL, &parse_data);

out:
	return error;
}

static int config_file_read(
	git_config_list *config_list,
	const git_repository *repo,
	config_file *file,
	git_config_level_t level,
	int depth)
{
	git_str contents = GIT_STR_INIT;
	struct stat st;
	int error;

	if (p_stat(file->path, &st) < 0) {
		error = git_fs_path_set_error(errno, file->path, "stat");
		goto out;
	}

	if ((error = git_futils_readbuffer(&contents, file->path)) < 0)
		goto out;

	git_futils_filestamp_set_from_stat(&file->stamp, &st);
	if ((error = git_hash_buf(file->checksum, contents.ptr, contents.size, GIT_HASH_ALGORITHM_SHA256)) < 0)
		goto out;

	if ((error = config_file_read_buffer(config_list, repo, file, level, depth,
					     contents.ptr, contents.size)) < 0)
		goto out;

out:
	git_str_dispose(&contents);
	return error;
}

static int write_section(git_str *fbuf, const char *key)
{
	int result;
	const char *dot;
	git_str buf = GIT_STR_INIT;

	/* All of this just for [section "subsection"] */
	dot = strchr(key, '.');
	git_str_putc(&buf, '[');
	if (dot == NULL) {
		git_str_puts(&buf, key);
	} else {
		char *escaped;
		git_str_put(&buf, key, dot - key);
		escaped = escape_value(dot + 1);
		GIT_ERROR_CHECK_ALLOC(escaped);
		git_str_printf(&buf, " \"%s\"", escaped);
		git__free(escaped);
	}
	git_str_puts(&buf, "]\n");

	if (git_str_oom(&buf))
		return -1;

	result = git_str_put(fbuf, git_str_cstr(&buf), buf.size);
	git_str_dispose(&buf);

	return result;
}

static const char *quotes_for_value(const char *value)
{
	const char *ptr;

	if (value[0] == ' ' || value[0] == '\0')
		return "\"";

	for (ptr = value; *ptr; ++ptr) {
		if (*ptr == ';' || *ptr == '#')
			return "\"";
	}

	if (ptr[-1] == ' ')
		return "\"";

	return "";
}

struct write_data {
	git_str *buf;
	git_str buffered_comment;
	unsigned int in_section : 1,
		preg_replaced : 1;
	const char *orig_section;
	const char *section;
	const char *orig_name;
	const char *name;
	const git_regexp *preg;
	const char *value;
};

static int write_line_to(git_str *buf, const char *line, size_t line_len)
{
	int result = git_str_put(buf, line, line_len);

	if (!result && line_len && line[line_len-1] != '\n')
		result = git_str_printf(buf, "\n");

	return result;
}

static int write_line(struct write_data *write_data, const char *line, size_t line_len)
{
	return write_line_to(write_data->buf, line, line_len);
}

static int write_value(struct write_data *write_data)
{
	const char *q;
	int result;

	q = quotes_for_value(write_data->value);
	result = git_str_printf(write_data->buf,
		"\t%s = %s%s%s\n", write_data->orig_name, q, write_data->value, q);

	/* If we are updating a single name/value, we're done.  Setting `value`
	 * to `NULL` will prevent us from trying to write it again later (in
	 * `write_on_section`) if we see the same section repeated.
	 */
	if (!write_data->preg)
		write_data->value = NULL;

	return result;
}

static int write_on_section(
	git_config_parser *reader,
	const char *current_section,
	const char *line,
	size_t line_len,
	void *data)
{
	struct write_data *write_data = (struct write_data *)data;
	int result = 0;

	GIT_UNUSED(reader);

	/* If we were previously in the correct section (but aren't anymore)
	 * and haven't written our value (for a simple name/value set, not
	 * a multivar), then append it to the end of the section before writing
	 * the new one.
	 */
	if (write_data->in_section && !write_data->preg && write_data->value)
		result = write_value(write_data);

	write_data->in_section = strcmp(current_section, write_data->section) == 0;

	/*
	 * If there were comments just before this section, dump them as well.
	 */
	if (!result) {
		result = git_str_put(write_data->buf, write_data->buffered_comment.ptr, write_data->buffered_comment.size);
		git_str_clear(&write_data->buffered_comment);
	}

	if (!result)
		result = write_line(write_data, line, line_len);

	return result;
}

static int write_on_variable(
	git_config_parser *reader,
	const char *current_section,
	const char *var_name,
	const char *var_value,
	const char *line,
	size_t line_len,
	void *data)
{
	struct write_data *write_data = (struct write_data *)data;
	bool has_matched = false;
	int error;

	GIT_UNUSED(reader);
	GIT_UNUSED(current_section);

	/*
	 * If there were comments just before this variable, let's dump them as well.
	 */
	if ((error = git_str_put(write_data->buf, write_data->buffered_comment.ptr, write_data->buffered_comment.size)) < 0)
		return error;

	git_str_clear(&write_data->buffered_comment);

	/* See if we are to update this name/value pair; first examine name */
	if (write_data->in_section &&
		strcasecmp(write_data->name, var_name) == 0)
		has_matched = true;

	/* If we have a regex to match the value, see if it matches */
	if (has_matched && write_data->preg != NULL)
		has_matched = (git_regexp_match(write_data->preg, var_value) == 0);

	/* If this isn't the name/value we're looking for, simply dump the
	 * existing data back out and continue on.
	 */
	if (!has_matched)
		return write_line(write_data, line, line_len);

	write_data->preg_replaced = 1;

	/* If value is NULL, we are deleting this value; write nothing. */
	if (!write_data->value)
		return 0;

	return write_value(write_data);
}

static int write_on_comment(git_config_parser *reader, const char *line, size_t line_len, void *data)
{
	struct write_data *write_data;

	GIT_UNUSED(reader);

	write_data = (struct write_data *)data;
	return write_line_to(&write_data->buffered_comment, line, line_len);
}

static int write_on_eof(
	git_config_parser *reader, const char *current_section, void *data)
{
	struct write_data *write_data = (struct write_data *)data;
	int result = 0;

	GIT_UNUSED(reader);

	/*
	 * If we've buffered comments when reaching EOF, make sure to dump them.
	 */
	if ((result = git_str_put(write_data->buf, write_data->buffered_comment.ptr, write_data->buffered_comment.size)) < 0)
		return result;

	/* If we are at the EOF and have not written our value (again, for a
	 * simple name/value set, not a multivar) then we have never seen the
	 * section in question and should create a new section and write the
	 * value.
	 */
	if ((!write_data->preg || !write_data->preg_replaced) && write_data->value) {
		/* write the section header unless we're already in it */
		if (!current_section || strcmp(current_section, write_data->section))
			result = write_section(write_data->buf, write_data->orig_section);

		if (!result)
			result = write_value(write_data);
	}

	return result;
}

/*
 * This is pretty much the parsing, except we write out anything we don't have
 */
static int config_file_write(
	config_file_backend *cfg,
	const char *orig_key,
	const char *key,
	const git_regexp *preg,
	const char *value)

{
	char *orig_section = NULL, *section = NULL, *orig_name, *name, *ldot;
	git_str buf = GIT_STR_INIT, contents = GIT_STR_INIT;
	git_config_parser parser = GIT_CONFIG_PARSER_INIT;
	git_filebuf file = GIT_FILEBUF_INIT;
	struct write_data write_data;
	int error;

	memset(&write_data, 0, sizeof(write_data));

	if (cfg->locked) {
		error = git_str_puts(&contents, git_str_cstr(&cfg->locked_content) == NULL ? "" : git_str_cstr(&cfg->locked_content));
	} else {
		if ((error = git_filebuf_open(&file, cfg->file.path,
				GIT_FILEBUF_HASH_SHA256,
				GIT_CONFIG_FILE_MODE)) < 0)
			goto done;

		/* We need to read in our own config file */
		error = git_futils_readbuffer(&contents, cfg->file.path);
	}
	if (error < 0 && error != GIT_ENOTFOUND)
		goto done;

	if ((git_config_parser_init(&parser, cfg->file.path, contents.ptr, contents.size)) < 0)
		goto done;

	ldot = strrchr(key, '.');
	name = ldot + 1;
	section = git__strndup(key, ldot - key);
	GIT_ERROR_CHECK_ALLOC(section);

	ldot = strrchr(orig_key, '.');
	orig_name = ldot + 1;
	orig_section = git__strndup(orig_key, ldot - orig_key);
	GIT_ERROR_CHECK_ALLOC(orig_section);

	write_data.buf = &buf;
	write_data.orig_section = orig_section;
	write_data.section = section;
	write_data.orig_name = orig_name;
	write_data.name = name;
	write_data.preg = preg;
	write_data.value = value;

	if ((error = git_config_parse(&parser, write_on_section, write_on_variable,
				      write_on_comment, write_on_eof, &write_data)) < 0)
		goto done;

	if (cfg->locked) {
		size_t len = buf.asize;
		/* Update our copy with the modified contents */
		git_str_dispose(&cfg->locked_content);
		git_str_attach(&cfg->locked_content, git_str_detach(&buf), len);
	} else {
		git_filebuf_write(&file, git_str_cstr(&buf), git_str_len(&buf));

		if ((error = git_filebuf_commit(&file)) < 0)
			goto done;

		if ((error = config_file_refresh_from_buffer(&cfg->parent, buf.ptr, buf.size)) < 0)
			goto done;
	}

done:
	git__free(section);
	git__free(orig_section);
	git_str_dispose(&write_data.buffered_comment);
	git_str_dispose(&buf);
	git_str_dispose(&contents);
	git_filebuf_cleanup(&file);
	git_config_parser_dispose(&parser);

	return error;
}
