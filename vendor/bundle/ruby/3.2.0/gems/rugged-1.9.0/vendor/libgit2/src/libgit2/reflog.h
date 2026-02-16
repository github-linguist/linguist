/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_reflog_h__
#define INCLUDE_reflog_h__

#include "common.h"

#include "git2/reflog.h"
#include "vector.h"

#define GIT_REFLOG_DIR "logs/"
#define GIT_REFLOG_DIR_MODE 0777
#define GIT_REFLOG_FILE_MODE 0666

struct git_reflog_entry {
	git_oid oid_old;
	git_oid oid_cur;

	git_signature *committer;

	char *msg;
};

struct git_reflog {
	git_refdb *db;
	char *ref_name;
	git_oid_t oid_type;
	git_vector entries;
};

GIT_INLINE(size_t) reflog_inverse_index(size_t idx, size_t total)
{
	return (total - 1) - idx;
}

void git_reflog_entry__free(git_reflog_entry *entry);

#endif
