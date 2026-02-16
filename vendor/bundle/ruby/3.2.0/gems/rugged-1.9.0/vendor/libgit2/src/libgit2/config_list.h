/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/sys/config.h"
#include "config.h"

typedef struct git_config_list git_config_list;

typedef struct {
	git_config_backend_entry base;
	git_config_list *config_list;
} git_config_list_entry;

int git_config_list_new(git_config_list **out);
int git_config_list_dup(git_config_list **out, git_config_list *list);
int git_config_list_dup_entry(git_config_list *list, const git_config_entry *entry);
void git_config_list_incref(git_config_list *list);
void git_config_list_free(git_config_list *list);
/* Add or append the new config option */
int git_config_list_append(git_config_list *list, git_config_list_entry *entry);
int git_config_list_get(git_config_list_entry **out, git_config_list *list, const char *key);
int git_config_list_get_unique(git_config_list_entry **out, git_config_list *list, const char *key);
int git_config_list_iterator_new(git_config_iterator **out, git_config_list *list);
const char *git_config_list_add_string(git_config_list *list, const char *str);

void git_config_list_entry_free(git_config_backend_entry *entry);
