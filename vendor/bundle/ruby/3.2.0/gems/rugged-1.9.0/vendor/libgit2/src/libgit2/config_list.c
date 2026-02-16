/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config_list.h"
#include "hashmap_str.h"

typedef struct config_entry_list {
	struct config_entry_list *next;
	struct config_entry_list *last;
	git_config_list_entry *entry;
} config_entry_list;

typedef struct {
	git_config_list_entry *entry;
	bool multivar;
} config_entry_map_head;

typedef struct config_list_iterator {
	git_config_iterator parent;
	git_config_list *list;
	config_entry_list *head;
} config_list_iterator;

GIT_HASHMAP_STR_SETUP(git_config_list_pathmap, char *);
GIT_HASHMAP_STR_SETUP(git_config_list_headmap, config_entry_map_head *);

struct git_config_list {
	git_refcount rc;

	/* Interned strings - paths to config files or backend types */
	git_config_list_pathmap strings;

	/* Config entries */
	git_config_list_headmap map;
	config_entry_list *entries;
};

int git_config_list_new(git_config_list **out)
{
	git_config_list *config_list;

	config_list = git__calloc(1, sizeof(git_config_list));
	GIT_ERROR_CHECK_ALLOC(config_list);
	GIT_REFCOUNT_INC(config_list);

	*out = config_list;
	return 0;
}

int git_config_list_dup_entry(git_config_list *config_list, const git_config_entry *entry)
{
	git_config_list_entry *duplicated;
	int error;

	duplicated = git__calloc(1, sizeof(git_config_list_entry));
	GIT_ERROR_CHECK_ALLOC(duplicated);

	duplicated->base.entry.name = git__strdup(entry->name);
	GIT_ERROR_CHECK_ALLOC(duplicated->base.entry.name);

	if (entry->value) {
		duplicated->base.entry.value = git__strdup(entry->value);
		GIT_ERROR_CHECK_ALLOC(duplicated->base.entry.value);
	}

	duplicated->base.entry.backend_type = git_config_list_add_string(config_list, entry->backend_type);
	GIT_ERROR_CHECK_ALLOC(duplicated->base.entry.backend_type);

	if (entry->origin_path) {
		duplicated->base.entry.origin_path = git_config_list_add_string(config_list, entry->origin_path);
		GIT_ERROR_CHECK_ALLOC(duplicated->base.entry.origin_path);
	}

	duplicated->base.entry.level = entry->level;
	duplicated->base.entry.include_depth = entry->include_depth;
	duplicated->base.free = git_config_list_entry_free;
	duplicated->config_list = config_list;

	if ((error = git_config_list_append(config_list, duplicated)) < 0)
		goto out;

out:
	if (error && duplicated) {
		git__free((char *) duplicated->base.entry.name);
		git__free((char *) duplicated->base.entry.value);
		git__free(duplicated);
	}
	return error;
}

int git_config_list_dup(git_config_list **out, git_config_list *config_list)
{
	git_config_list *result = NULL;
	config_entry_list *head;
	int error;

	if ((error = git_config_list_new(&result)) < 0)
		goto out;

	for (head = config_list->entries; head; head = head->next)
		if ((git_config_list_dup_entry(result, &head->entry->base.entry)) < 0)
			goto out;

	*out = result;
	result = NULL;

out:
	git_config_list_free(result);
	return error;
}

void git_config_list_incref(git_config_list *config_list)
{
	GIT_REFCOUNT_INC(config_list);
}

static void config_list_free(git_config_list *config_list)
{
	config_entry_list *entry_list = NULL, *next;
	config_entry_map_head *head;
	char *str;
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;

	while (git_config_list_pathmap_iterate(&iter, NULL, &str, &config_list->strings) == 0)
		git__free(str);

	git_config_list_pathmap_dispose(&config_list->strings);

	iter = GIT_HASHMAP_ITER_INIT;
	while (git_config_list_headmap_iterate(&iter, NULL, &head, &config_list->map) == 0) {
		git__free((char *) head->entry->base.entry.name);
		git__free(head);
	}
	git_config_list_headmap_dispose(&config_list->map);

	entry_list = config_list->entries;
	while (entry_list != NULL) {
		next = entry_list->next;
		git__free((char *) entry_list->entry->base.entry.value);
		git__free(entry_list->entry);
		git__free(entry_list);
		entry_list = next;
	}

	git__free(config_list);
}

void git_config_list_free(git_config_list *config_list)
{
	if (config_list)
		GIT_REFCOUNT_DEC(config_list, config_list_free);
}

int git_config_list_append(git_config_list *config_list, git_config_list_entry *entry)
{
	config_entry_list *list_head;
	config_entry_map_head *map_head;

	if (git_config_list_headmap_get(&map_head, &config_list->map, entry->base.entry.name) == 0) {
		map_head->multivar = true;
		/*
		 * This is a micro-optimization for configuration files
		 * with a lot of same keys. As for multivars the entry's
		 * key will be the same for all list, we can just free
		 * all except the first entry's name and just re-use it.
		 */
		git__free((char *) entry->base.entry.name);
		entry->base.entry.name = map_head->entry->base.entry.name;
	} else {
		map_head = git__calloc(1, sizeof(*map_head));
		if ((git_config_list_headmap_put(&config_list->map, entry->base.entry.name, map_head)) < 0)
			return -1;
	}
	map_head->entry = entry;

	list_head = git__calloc(1, sizeof(config_entry_list));
	GIT_ERROR_CHECK_ALLOC(list_head);
	list_head->entry = entry;

	if (config_list->entries)
		config_list->entries->last->next = list_head;
	else
		config_list->entries = list_head;
	config_list->entries->last = list_head;

	return 0;
}

int git_config_list_get(git_config_list_entry **out, git_config_list *config_list, const char *key)
{
	config_entry_map_head *entry;

	if (git_config_list_headmap_get(&entry, &config_list->map, key) != 0)
		return GIT_ENOTFOUND;

	*out = entry->entry;
	return 0;
}

int git_config_list_get_unique(git_config_list_entry **out, git_config_list *config_list, const char *key)
{
	config_entry_map_head *entry;

	if (git_config_list_headmap_get(&entry, &config_list->map, key) != 0)
		return GIT_ENOTFOUND;

	if (entry->multivar) {
		git_error_set(GIT_ERROR_CONFIG, "entry is not unique due to being a multivar");
		return -1;
	}

	if (entry->entry->base.entry.include_depth) {
		git_error_set(GIT_ERROR_CONFIG, "entry is not unique due to being included");
		return -1;
	}

	*out = entry->entry;
	return 0;
}

static void config_iterator_free(git_config_iterator *iter)
{
	config_list_iterator *it = (config_list_iterator *) iter;
	git_config_list_free(it->list);
	git__free(it);
}

static int config_iterator_next(
	git_config_backend_entry **entry,
	git_config_iterator *iter)
{
	config_list_iterator *it = (config_list_iterator *) iter;

	if (!it->head)
		return GIT_ITEROVER;

	*entry = &it->head->entry->base;
	it->head = it->head->next;

	return 0;
}

int git_config_list_iterator_new(git_config_iterator **out, git_config_list *config_list)
{
	config_list_iterator *it;

	it = git__calloc(1, sizeof(config_list_iterator));
	GIT_ERROR_CHECK_ALLOC(it);
	it->parent.next = config_iterator_next;
	it->parent.free = config_iterator_free;
	it->head = config_list->entries;
	it->list = config_list;

	git_config_list_incref(config_list);
	*out = &it->parent;

	return 0;
}

/* release the map containing the entry as an equivalent to freeing it */
void git_config_list_entry_free(git_config_backend_entry *e)
{
	git_config_list_entry *entry = (git_config_list_entry *)e;
	git_config_list_free(entry->config_list);
}

const char *git_config_list_add_string(
	git_config_list *config_list,
	const char *str)
{
	char *s;

	if (git_config_list_pathmap_get(&s, &config_list->strings, str) == 0)
		return s;

	if ((s = git__strdup(str)) == NULL ||
	     git_config_list_pathmap_put(&config_list->strings, s, s) < 0)
		return NULL;

	return s;
}
