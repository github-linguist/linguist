/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_merge_h__
#define INCLUDE_merge_h__

#include "common.h"

#include "vector.h"
#include "commit_list.h"
#include "pool.h"
#include "iterator.h"

#include "git2/types.h"
#include "git2/merge.h"
#include "git2/sys/merge.h"

#define GIT_MERGE_MSG_FILE		"MERGE_MSG"
#define GIT_MERGE_MODE_FILE		"MERGE_MODE"
#define GIT_MERGE_FILE_MODE		0666

#define GIT_MERGE_DEFAULT_RENAME_THRESHOLD	50
#define GIT_MERGE_DEFAULT_TARGET_LIMIT		1000

/** Types of changes when files are merged from branch to branch. */
typedef enum {
	/* No conflict - a change only occurs in one branch. */
	GIT_MERGE_DIFF_NONE = 0,

	/* Occurs when a file is modified in both branches. */
	GIT_MERGE_DIFF_BOTH_MODIFIED = (1 << 0),

	/* Occurs when a file is added in both branches. */
	GIT_MERGE_DIFF_BOTH_ADDED = (1 << 1),

	/* Occurs when a file is deleted in both branches. */
	GIT_MERGE_DIFF_BOTH_DELETED = (1 << 2),

	/* Occurs when a file is modified in one branch and deleted in the other. */
	GIT_MERGE_DIFF_MODIFIED_DELETED = (1 << 3),

	/* Occurs when a file is renamed in one branch and modified in the other. */
	GIT_MERGE_DIFF_RENAMED_MODIFIED = (1 << 4),

	/* Occurs when a file is renamed in one branch and deleted in the other. */
	GIT_MERGE_DIFF_RENAMED_DELETED = (1 << 5),

	/* Occurs when a file is renamed in one branch and a file with the same
	 * name is added in the other.  Eg, A->B and new file B.  Core git calls
	 * this a "rename/delete". */
	GIT_MERGE_DIFF_RENAMED_ADDED = (1 << 6),

	/* Occurs when both a file is renamed to the same name in the ours and
	 * theirs branches.  Eg, A->B and A->B in both.  Automergeable. */
	GIT_MERGE_DIFF_BOTH_RENAMED = (1 << 7),

	/* Occurs when a file is renamed to different names in the ours and theirs
	 * branches.  Eg, A->B and A->C. */
	GIT_MERGE_DIFF_BOTH_RENAMED_1_TO_2 = (1 << 8),

	/* Occurs when two files are renamed to the same name in the ours and
	 * theirs branches.  Eg, A->C and B->C. */
	GIT_MERGE_DIFF_BOTH_RENAMED_2_TO_1 = (1 << 9),

	/* Occurs when an item at a path in one branch is a directory, and an
	 * item at the same path in a different branch is a file. */
	GIT_MERGE_DIFF_DIRECTORY_FILE = (1 << 10),

	/* The child of a folder that is in a directory/file conflict. */
	GIT_MERGE_DIFF_DF_CHILD = (1 << 11)
} git_merge_diff_t;

typedef struct {
	git_repository *repo;
	git_pool pool;

	/* Vector of git_index_entry that represent the merged items that
	 * have been staged, either because only one side changed, or because
	 * the two changes were non-conflicting and mergeable.  These items
	 * will be written as staged entries in the main index.
	 */
	git_vector staged;

	/* Vector of git_merge_diff entries that represent the conflicts that
	 * have not been automerged.  These items will be written to high-stage
	 * entries in the main index.
	 */
	git_vector conflicts;

	/* Vector of git_merge_diff that have been automerged.  These items
	 * will be written to the REUC when the index is produced.
	 */
	git_vector resolved;
} git_merge_diff_list;

/**
 * Description of changes to one file across three trees.
 */
typedef struct {
	git_merge_diff_t type;

	git_index_entry ancestor_entry;

	git_index_entry our_entry;
	git_delta_t our_status;

	git_index_entry their_entry;
	git_delta_t their_status;

} git_merge_diff;

int git_merge__bases_many(
	git_commit_list **out,
	git_revwalk *walk,
	git_commit_list_node *one,
	git_vector *twos,
	uint32_t minimum_generation);

/*
 * Three-way tree differencing
 */

git_merge_diff_list *git_merge_diff_list__alloc(git_repository *repo);

int git_merge_diff_list__find_differences(
	git_merge_diff_list *merge_diff_list,
	git_iterator *ancestor_iterator,
	git_iterator *ours_iter,
	git_iterator *theirs_iter);

int git_merge_diff_list__find_renames(git_repository *repo, git_merge_diff_list *merge_diff_list, const git_merge_options *opts);

void git_merge_diff_list__free(git_merge_diff_list *diff_list);

/* Merge metadata setup */

int git_merge__setup(
	git_repository *repo,
	const git_annotated_commit *our_head,
	const git_annotated_commit *heads[],
	size_t heads_len);

int git_merge__iterators(
	git_index **out,
	git_repository *repo,
	git_iterator *ancestor_iter,
	git_iterator *our_iter,
	git_iterator *their_iter,
	const git_merge_options *given_opts);

int git_merge__check_result(git_repository *repo, git_index *index_new);

int git_merge__append_conflicts_to_merge_msg(git_repository *repo, git_index *index);

/* Merge files */

GIT_INLINE(const char *) git_merge_file__best_path(
	const char *ancestor,
	const char *ours,
	const char *theirs)
{
	if (!ancestor) {
		if (ours && theirs && strcmp(ours, theirs) == 0)
			return ours;

		return NULL;
	}

	if (ours && strcmp(ancestor, ours) == 0)
		return theirs;
	else if(theirs && strcmp(ancestor, theirs) == 0)
		return ours;

	return NULL;
}

GIT_INLINE(uint32_t) git_merge_file__best_mode(
	uint32_t ancestor, uint32_t ours, uint32_t theirs)
{
	/*
	 * If ancestor didn't exist and either ours or theirs is executable,
	 * assume executable.  Otherwise, if any mode changed from the ancestor,
	 * use that one.
	 */
	if (!ancestor) {
		if (ours == GIT_FILEMODE_BLOB_EXECUTABLE ||
			theirs == GIT_FILEMODE_BLOB_EXECUTABLE)
			return GIT_FILEMODE_BLOB_EXECUTABLE;

		return GIT_FILEMODE_BLOB;
	} else if (ours && theirs) {
		if (ancestor == ours)
			return theirs;

		return ours;
	}

	return 0;
}

#endif
