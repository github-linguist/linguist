/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_diff_driver_h__
#define INCLUDE_diff_driver_h__

#include "common.h"

#include "attr_file.h"
#include "str.h"
#include "hashmap.h"

typedef struct git_diff_driver git_diff_driver;
typedef struct git_diff_driver_registry git_diff_driver_registry;

git_diff_driver_registry *git_diff_driver_registry_new(void);
void git_diff_driver_registry_free(git_diff_driver_registry *);

int git_diff_driver_lookup(git_diff_driver **, git_repository *,
	git_attr_session *attrsession, const char *);
void git_diff_driver_free(git_diff_driver *);

/* diff option flags to force off and on for this driver */
void git_diff_driver_update_options(uint32_t *option_flags, git_diff_driver *);

/* returns -1 meaning "unknown", 0 meaning not binary, 1 meaning binary */
int git_diff_driver_content_is_binary(
	git_diff_driver *, const char *content, size_t content_len);

typedef long (*git_diff_find_context_fn)(
	const char *, long, char *, long, void *);

typedef int (*git_diff_find_context_line)(
	git_diff_driver *, git_str *);

typedef struct {
	git_diff_driver *driver;
	git_diff_find_context_line match_line;
	git_str line;
} git_diff_find_context_payload;

void git_diff_find_context_init(
	git_diff_find_context_fn *findfn_out,
	git_diff_find_context_payload *payload_out,
	git_diff_driver *driver);

void git_diff_find_context_clear(git_diff_find_context_payload *);

#endif
