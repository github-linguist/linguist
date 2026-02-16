/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "attrcache.h"

#include "repository.h"
#include "attr_file.h"
#include "config.h"
#include "sysdir.h"
#include "ignore.h"
#include "path.h"
#include "hashmap_str.h"

GIT_HASHMAP_STR_SETUP(git_attr_cache_filemap, git_attr_file_entry *);
GIT_HASHMAP_STR_SETUP(git_attr_cache_macromap, git_attr_rule *);

struct git_attr_cache {
	char *cfg_attr_file; /* cached value of core.attributesfile */
	char *cfg_excl_file; /* cached value of core.excludesfile */

	/* hash path to git_attr_file_entry records */
	git_attr_cache_filemap files;
	/* hash name to git_attr_rule */
	git_attr_cache_macromap macros;

	git_mutex lock;
	git_pool  pool;
};

const char *git_attr_cache_attributesfile(git_attr_cache *cache)
{
	return cache->cfg_attr_file;
}

const char *git_attr_cache_excludesfile(git_attr_cache *cache)
{
	return cache->cfg_excl_file;
}

git_pool *git_attr_cache_pool(git_attr_cache *cache)
{
	return &cache->pool;
}

GIT_INLINE(int) attr_cache_lock(git_attr_cache *cache)
{
	GIT_UNUSED(cache); /* avoid warning if threading is off */

	if (git_mutex_lock(&cache->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "unable to get attr cache lock");
		return -1;
	}
	return 0;
}

GIT_INLINE(void) attr_cache_unlock(git_attr_cache *cache)
{
	GIT_UNUSED(cache); /* avoid warning if threading is off */
	git_mutex_unlock(&cache->lock);
}

GIT_INLINE(git_attr_file_entry *) attr_cache_lookup_entry(
	git_attr_cache *cache, const char *path)
{
	git_attr_file_entry *result;

	if (git_attr_cache_filemap_get(&result, &cache->files, path) == 0)
		return result;

	return NULL;
}

int git_attr_cache__alloc_file_entry(
	git_attr_file_entry **out,
	git_repository *repo,
	const char *base,
	const char *path,
	git_pool *pool)
{
	git_str fullpath_str = GIT_STR_INIT;
	size_t baselen = 0, pathlen = strlen(path);
	size_t cachesize = sizeof(git_attr_file_entry) + pathlen + 1;
	git_attr_file_entry *ce;

	if (base != NULL && git_fs_path_root(path) < 0) {
		baselen = strlen(base);
		cachesize += baselen;

		if (baselen && base[baselen - 1] != '/')
			cachesize++;
	}

	ce = git_pool_mallocz(pool, cachesize);
	GIT_ERROR_CHECK_ALLOC(ce);

	if (baselen) {
		memcpy(ce->fullpath, base, baselen);

		if (base[baselen - 1] != '/')
			ce->fullpath[baselen++] = '/';
	}
	memcpy(&ce->fullpath[baselen], path, pathlen);

	fullpath_str.ptr = ce->fullpath;
	fullpath_str.size = pathlen + baselen;

	if (git_path_validate_str_length(repo, &fullpath_str) < 0)
		return -1;

	ce->path = &ce->fullpath[baselen];
	*out = ce;

	return 0;
}

/* call with attrcache locked */
static int attr_cache_make_entry(
	git_attr_file_entry **out, git_repository *repo, const char *path)
{
	git_attr_cache *cache = git_repository_attr_cache(repo);
	git_attr_file_entry *entry = NULL;
	int error;

	if ((error = git_attr_cache__alloc_file_entry(&entry, repo,
		git_repository_workdir(repo), path, &cache->pool)) < 0)
		return error;

	if ((error = git_attr_cache_filemap_put(&cache->files, entry->path, entry)) < 0)
		return error;

	*out = entry;
	return error;
}

/* insert entry or replace existing if we raced with another thread */
static int attr_cache_upsert(git_attr_cache *cache, git_attr_file *file)
{
	git_attr_file_entry *entry;
	git_attr_file *old;

	if (attr_cache_lock(cache) < 0)
		return -1;

	entry = attr_cache_lookup_entry(cache, file->entry->path);

	GIT_REFCOUNT_OWN(file, entry);
	GIT_REFCOUNT_INC(file);

	/*
	 * Replace the existing value if another thread has
	 * created it in the meantime.
	 */
	old = git_atomic_swap(entry->file[file->source.type], file);

	if (old) {
		GIT_REFCOUNT_OWN(old, NULL);
		git_attr_file__free(old);
	}

	attr_cache_unlock(cache);
	return 0;
}

static int attr_cache_remove(git_attr_cache *cache, git_attr_file *file)
{
	int error = 0;
	git_attr_file_entry *entry;
	git_attr_file *oldfile = NULL;

	if (!file)
		return 0;

	if ((error = attr_cache_lock(cache)) < 0)
		return error;

	if ((entry = attr_cache_lookup_entry(cache, file->entry->path)) != NULL)
		oldfile = git_atomic_compare_and_swap(&entry->file[file->source.type], file, NULL);

	attr_cache_unlock(cache);

	if (oldfile == file) {
		GIT_REFCOUNT_OWN(file, NULL);
		git_attr_file__free(file);
	}

	return error;
}

/* Look up cache entry and file.
 * - If entry is not present, create it while the cache is locked.
 * - If file is present, increment refcount before returning it, so the
 *   cache can be unlocked and it won't go away.
 */
static int attr_cache_lookup(
	git_attr_file **out_file,
	git_attr_file_entry **out_entry,
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_file_source *source)
{
	int error = 0;
	git_str path = GIT_STR_INIT;
	const char *wd = git_repository_workdir(repo);
	const char *filename;
	git_attr_cache *cache = git_repository_attr_cache(repo);
	git_attr_file_entry *entry = NULL;
	git_attr_file *file = NULL;

	/* join base and path as needed */
	if (source->base != NULL && git_fs_path_root(source->filename) < 0) {
		git_str *p = attr_session ? &attr_session->tmp : &path;

		if (git_str_joinpath(p, source->base, source->filename) < 0 ||
		    git_path_validate_str_length(repo, p) < 0)
			return -1;

		filename = p->ptr;
	} else {
		filename = source->filename;
	}

	if (wd && !git__prefixcmp(filename, wd))
		filename += strlen(wd);

	/* check cache for existing entry */
	if ((error = attr_cache_lock(cache)) < 0)
		goto cleanup;

	entry = attr_cache_lookup_entry(cache, filename);

	if (!entry) {
		error = attr_cache_make_entry(&entry, repo, filename);
	} else if (entry->file[source->type] != NULL) {
		file = entry->file[source->type];
		GIT_REFCOUNT_INC(file);
	}

	attr_cache_unlock(cache);

cleanup:
	*out_file  = file;
	*out_entry = entry;

	git_str_dispose(&path);
	return error;
}

int git_attr_cache__get(
	git_attr_file **out,
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_file_source *source,
	git_attr_file_parser parser,
	bool allow_macros)
{
	int error = 0;
	git_attr_cache *cache = git_repository_attr_cache(repo);
	git_attr_file_entry *entry = NULL;
	git_attr_file *file = NULL, *updated = NULL;

	if ((error = attr_cache_lookup(&file, &entry, repo, attr_session, source)) < 0)
		return error;

	/* load file if we don't have one or if existing one is out of date */
	if (!file ||
	    (error = git_attr_file__out_of_date(repo, attr_session, file, source)) > 0)
		error = git_attr_file__load(&updated, repo, attr_session,
		                            entry, source, parser,
		                            allow_macros);

	/* if we loaded the file, insert into and/or update cache */
	if (updated) {
		if ((error = attr_cache_upsert(cache, updated)) < 0) {
			git_attr_file__free(updated);
		} else {
			git_attr_file__free(file); /* offset incref from lookup */
			file = updated;
		}
	}

	/* if file could not be loaded */
	if (error < 0) {
		/* remove existing entry */
		if (file) {
			attr_cache_remove(cache, file);
			git_attr_file__free(file); /* offset incref from lookup */
			file = NULL;
		}
		/* no error if file simply doesn't exist */
		if (error == GIT_ENOTFOUND) {
			git_error_clear();
			error = 0;
		}
	}

	*out = file;
	return error;
}

bool git_attr_cache__is_cached(
	git_repository *repo,
	git_attr_file_source_t source_type,
	const char *filename)
{
	git_attr_cache *cache = git_repository_attr_cache(repo);
	git_attr_file_entry *entry;

	if (!cache)
		return false;

	if (git_attr_cache_filemap_get(&entry, &cache->files, filename) != 0)
		return false;

	return entry && (entry->file[source_type] != NULL);
}


static int attr_cache__lookup_path(
	char **out, git_config *cfg, const char *key, const char *fallback)
{
	git_str buf = GIT_STR_INIT;
	int error;
	git_config_entry *entry = NULL;

	*out = NULL;

	if ((error = git_config__lookup_entry(&entry, cfg, key, false)) < 0)
		return error;

	if (entry) {
		const char *cfgval = entry->value;

		/* expand leading ~/ as needed */
		if (cfgval && cfgval[0] == '~' && cfgval[1] == '/') {
			if (! (error = git_sysdir_expand_homedir_file(&buf, &cfgval[2])))
				*out = git_str_detach(&buf);
		} else if (cfgval) {
			*out = git__strdup(cfgval);
		}
	}
	else if (!git_sysdir_find_xdg_file(&buf, fallback)) {
		*out = git_str_detach(&buf);
	}

	git_config_entry_free(entry);
	git_str_dispose(&buf);

	return error;
}

static void attr_cache__free(git_attr_cache *cache)
{
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;
	git_attr_rule *rule;
	git_attr_file_entry *entry;
	bool unlock;

	if (!cache)
		return;

	unlock = (attr_cache_lock(cache) == 0);

	while (git_attr_cache_filemap_iterate(&iter, NULL, &entry, &cache->files) == 0) {
		git_attr_file *file;
		size_t i;

		for (i = 0; i < GIT_ATTR_FILE_NUM_SOURCES; i++) {
			if ((file = git_atomic_swap(entry->file[i], NULL)) != NULL) {
				GIT_REFCOUNT_OWN(file, NULL);
				git_attr_file__free(file);
			}
		}
	}

	iter = GIT_HASHMAP_ITER_INIT;
	while (git_attr_cache_macromap_iterate(&iter, NULL, &rule, &cache->macros) == 0)
		git_attr_rule__free(rule);

	git_attr_cache_filemap_dispose(&cache->files);
	git_attr_cache_macromap_dispose(&cache->macros);

	git_pool_clear(&cache->pool);

	git__free(cache->cfg_attr_file);
	cache->cfg_attr_file = NULL;

	git__free(cache->cfg_excl_file);
	cache->cfg_excl_file = NULL;

	if (unlock)
		attr_cache_unlock(cache);
	git_mutex_free(&cache->lock);

	git__free(cache);
}

int git_attr_cache__init(git_repository *repo)
{
	int ret = 0;
	git_attr_cache *cache = git_repository_attr_cache(repo);
	git_config *cfg = NULL;

	if (cache)
		return 0;

	cache = git__calloc(1, sizeof(git_attr_cache));
	GIT_ERROR_CHECK_ALLOC(cache);

	/* set up lock */
	if (git_mutex_init(&cache->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "unable to initialize lock for attr cache");
		git__free(cache);
		return -1;
	}

	if ((ret = git_repository_config_snapshot(&cfg, repo)) < 0)
		goto cancel;

	/* cache config settings for attributes and ignores */
	ret = attr_cache__lookup_path(
		&cache->cfg_attr_file, cfg, GIT_ATTR_CONFIG, GIT_ATTR_FILE_XDG);
	if (ret < 0)
		goto cancel;

	ret = attr_cache__lookup_path(
		&cache->cfg_excl_file, cfg, GIT_IGNORE_CONFIG, GIT_IGNORE_FILE_XDG);
	if (ret < 0)
		goto cancel;

	/* allocate hashtable for attribute and ignore file contents,
	 * hashtable for attribute macros, and string pool
	 */
	if ((ret = git_pool_init(&cache->pool, 1)) < 0)
		goto cancel;

	if (git_atomic_compare_and_swap(&repo->attrcache, NULL, cache) != NULL)
		goto cancel; /* raced with another thread, free this but no error */

	git_config_free(cfg);

	/* insert default macros */
	return git_attr_add_macro(repo, "binary", "-diff -merge -text -crlf");

cancel:
	attr_cache__free(cache);
	git_config_free(cfg);
	return ret;
}

int git_attr_cache_flush(git_repository *repo)
{
	git_attr_cache *cache;

	/* this could be done less expensively, but for now, we'll just free
	 * the entire attrcache and let the next use reinitialize it...
	 */
	if (repo && (cache = git_atomic_swap(repo->attrcache, NULL)) != NULL)
		attr_cache__free(cache);

	return 0;
}

int git_attr_cache__insert_macro(git_repository *repo, git_attr_rule *macro)
{
	git_attr_cache *cache = git_repository_attr_cache(repo);
	git_attr_rule *preexisting;
	bool locked = false;
	int error = 0;

	/*
	 * Callers assume that if we return success, that the
	 * macro will have been adopted by the attributes cache.
	 * Thus, we have to free the macro here if it's not being
	 * added to the cache.
	 *
	 * TODO: generate warning log if (macro->assigns.length == 0)
	 */
	if (macro->assigns.length == 0) {
		git_attr_rule__free(macro);
		goto out;
	}

	if ((error = attr_cache_lock(cache)) < 0)
		goto out;
	locked = true;

	if (git_attr_cache_macromap_get(&preexisting, &cache->macros, macro->match.pattern) == 0)
		git_attr_rule__free(preexisting);

	if ((error = git_attr_cache_macromap_put(&cache->macros, macro->match.pattern, macro)) < 0)
		goto out;

out:
	if (locked)
		attr_cache_unlock(cache);
	return error;
}

git_attr_rule *git_attr_cache__lookup_macro(
	git_repository *repo, const char *name)
{
	git_attr_cache *cache = git_repository_attr_cache(repo);
	git_attr_rule *rule;

	if (!cache ||
	    git_attr_cache_macromap_get(&rule, &cache->macros, name) != 0)
		return NULL;

	return rule;
}
