/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_annotated_commit_h__
#define INCLUDE_annotated_commit_h__

#include "common.h"

#include "oidarray.h"

#include "git2/oid.h"

typedef enum {
	GIT_ANNOTATED_COMMIT_REAL = 1,
	GIT_ANNOTATED_COMMIT_VIRTUAL = 2
} git_annotated_commit_t;

/**
 * Internal structure for merge inputs.  An annotated commit is generally
 * "real" and backed by an actual commit in the repository, but merge will
 * internally create "virtual" commits that are in-memory intermediate
 * commits backed by an index.
 */
struct git_annotated_commit {
	git_annotated_commit_t type;

	/* real commit */
	git_commit *commit;
	git_tree *tree;

	/* virtual commit structure */
	git_index *index;
	git_array_oid_t parents;

	/* how this commit was looked up */
	const char *description;

	const char *ref_name;
	const char *remote_url;

	char id_str[GIT_OID_MAX_HEXSIZE + 1];
};

extern int git_annotated_commit_from_head(git_annotated_commit **out,
	git_repository *repo);
extern int git_annotated_commit_from_commit(git_annotated_commit **out,
	git_commit *commit);

#endif
