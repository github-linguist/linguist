/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_commit_h__
#define INCLUDE_commit_h__

#include "common.h"

#include "git2/commit.h"
#include "tree.h"
#include "repository.h"
#include "array.h"

#include <time.h>

struct git_commit {
	git_object object;

	git_array_t(git_oid) parent_ids;
	git_oid tree_id;

	git_signature *author;
	git_signature *committer;

	char *message_encoding;
	char *raw_message;
	char *raw_header;

	char *summary;
	char *body;
};

typedef struct {
	git_oid_t oid_type;
	unsigned int flags;
} git_commit__parse_options;

typedef enum {
	/** Only parse parents and committer info */
	GIT_COMMIT_PARSE_QUICK = (1 << 0)
} git_commit__parse_flags;

int git_commit__header_field(
	git_str *out,
	const git_commit *commit,
	const char *field);

int git_commit__extract_signature(
	git_str *signature,
	git_str *signed_data,
	git_repository *repo,
	git_oid *commit_id,
	const char *field);

int git_commit__create_buffer(
	git_str *out,
	git_repository *repo,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree,
	size_t parent_count,
	const git_commit *parents[]);

int git_commit__parse(
	void *commit,
	git_odb_object *obj,
	git_oid_t oid_type);

int git_commit__parse_raw(
	void *commit,
	const char *data,
	size_t size,
	git_oid_t oid_type);

int git_commit__parse_ext(
	git_commit *commit,
	git_odb_object *odb_obj,
	git_commit__parse_options *parse_opts);

void git_commit__free(void *commit);

#endif
