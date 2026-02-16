/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_iterator_h__
#define INCLUDE_iterator_h__

#include "common.h"

#include "git2/index.h"
#include "vector.h"
#include "str.h"
#include "ignore.h"

typedef struct git_iterator git_iterator;

typedef enum {
	GIT_ITERATOR_EMPTY = 0,
	GIT_ITERATOR_TREE = 1,
	GIT_ITERATOR_INDEX = 2,
	GIT_ITERATOR_WORKDIR = 3,
	GIT_ITERATOR_FS = 4
} git_iterator_t;

typedef enum {
	/** ignore case for entry sort order */
	GIT_ITERATOR_IGNORE_CASE = (1u << 0),
	/** force case sensitivity for entry sort order */
	GIT_ITERATOR_DONT_IGNORE_CASE = (1u << 1),
	/** return tree items in addition to blob items */
	GIT_ITERATOR_INCLUDE_TREES    = (1u << 2),
	/** don't flatten trees, requiring advance_into (implies INCLUDE_TREES) */
	GIT_ITERATOR_DONT_AUTOEXPAND  = (1u << 3),
	/** convert precomposed unicode to decomposed unicode */
	GIT_ITERATOR_PRECOMPOSE_UNICODE = (1u << 4),
	/** never convert precomposed unicode to decomposed unicode */
	GIT_ITERATOR_DONT_PRECOMPOSE_UNICODE = (1u << 5),
	/** include conflicts */
	GIT_ITERATOR_INCLUDE_CONFLICTS = (1u << 6),
	/** descend into symlinked directories */
	GIT_ITERATOR_DESCEND_SYMLINKS = (1u << 7),
	/** hash files in workdir or filesystem iterators */
	GIT_ITERATOR_INCLUDE_HASH = (1u << 8)
} git_iterator_flag_t;

typedef enum {
	GIT_ITERATOR_STATUS_NORMAL = 0,
	GIT_ITERATOR_STATUS_IGNORED = 1,
	GIT_ITERATOR_STATUS_EMPTY = 2,
	GIT_ITERATOR_STATUS_FILTERED = 3
} git_iterator_status_t;

typedef struct {
	const char *start;
	const char *end;

	/* paths to include in the iterator (literal).  if set, any paths not
	 * listed here will be excluded from iteration.
	 */
	git_strarray pathlist;

	/* flags, from above */
	unsigned int flags;

	/* oid type - necessary for non-workdir filesystem iterators */
	git_oid_t oid_type;
} git_iterator_options;

#define GIT_ITERATOR_OPTIONS_INIT {0}

typedef struct {
	int (*current)(const git_index_entry **, git_iterator *);
	int (*advance)(const git_index_entry **, git_iterator *);
	int (*advance_into)(const git_index_entry **, git_iterator *);
	int (*advance_over)(
		const git_index_entry **, git_iterator_status_t *, git_iterator *);
	int (*reset)(git_iterator *);
	void (*free)(git_iterator *);
} git_iterator_callbacks;

struct git_iterator {
	git_iterator_t type;
	git_iterator_callbacks *cb;

	git_repository *repo;
	git_index *index;

	char *start;
	size_t start_len;

	char *end;
	size_t end_len;

	bool started;
	bool ended;
	git_vector pathlist;
	size_t pathlist_walk_idx;
	int (*strcomp)(const char *a, const char *b);
	int (*strncomp)(const char *a, const char *b, size_t n);
	int (*prefixcomp)(const char *str, const char *prefix);
	int (*entry_srch)(const void *key, const void *array_member);
	size_t stat_calls;
	unsigned int flags;
};

extern int git_iterator_for_nothing(
	git_iterator **out,
	git_iterator_options *options);

/* tree iterators will match the ignore_case value from the index of the
 * repository, unless you override with a non-zero flag value
 */
extern int git_iterator_for_tree(
	git_iterator **out,
	git_tree *tree,
	git_iterator_options *options);

/* index iterators will take the ignore_case value from the index; the
 * ignore_case flags are not used
 */
extern int git_iterator_for_index(
	git_iterator **out,
	git_repository *repo,
	git_index *index,
	git_iterator_options *options);

extern int git_iterator_for_workdir_ext(
	git_iterator **out,
	git_repository *repo,
	const char *repo_workdir,
	git_index *index,
	git_tree *tree,
	git_iterator_options *options);

/* workdir iterators will match the ignore_case value from the index of the
 * repository, unless you override with a non-zero flag value
 */
GIT_INLINE(int) git_iterator_for_workdir(
	git_iterator **out,
	git_repository *repo,
	git_index *index,
	git_tree *tree,
	git_iterator_options *options)
{
	return git_iterator_for_workdir_ext(out, repo, NULL, index, tree, options);
}

/* for filesystem iterators, you have to explicitly pass in the ignore_case
 * behavior that you desire
 */
extern int git_iterator_for_filesystem(
	git_iterator **out,
	const char *root,
	git_iterator_options *options);

extern void git_iterator_free(git_iterator *iter);

/* Return a git_index_entry structure for the current value the iterator
 * is looking at or NULL if the iterator is at the end.
 *
 * The entry may noy be fully populated.  Tree iterators will only have a
 * value mode, OID, and path.  Workdir iterators will not have an OID (but
 * you can use `git_iterator_current_oid()` to calculate it on demand).
 *
 * You do not need to free the entry.  It is still "owned" by the iterator.
 * Once you call `git_iterator_advance()` then the old entry is no longer
 * guaranteed to be valid - it may be freed or just overwritten in place.
 */
GIT_INLINE(int) git_iterator_current(
	const git_index_entry **entry, git_iterator *iter)
{
	return iter->cb->current(entry, iter);
}

/**
 * Advance to the next item for the iterator.
 *
 * If GIT_ITERATOR_INCLUDE_TREES is set, this may be a tree item.  If
 * GIT_ITERATOR_DONT_AUTOEXPAND is set, calling this again when on a tree
 * item will skip over all the items under that tree.
 */
GIT_INLINE(int) git_iterator_advance(
	const git_index_entry **entry, git_iterator *iter)
{
	return iter->cb->advance(entry, iter);
}

/**
 * Iterate into a tree item (when GIT_ITERATOR_DONT_AUTOEXPAND is set).
 *
 * git_iterator_advance() steps through all items being iterated over
 * (either with or without trees, depending on GIT_ITERATOR_INCLUDE_TREES),
 * but if GIT_ITERATOR_DONT_AUTOEXPAND is set, it will skip to the next
 * sibling of a tree instead of going to the first child of the tree.  In
 * that case, use this function to advance to the first child of the tree.
 *
 * If the current item is not a tree, this is a no-op.
 *
 * For filesystem and working directory iterators, a tree (i.e. directory)
 * can be empty.  In that case, this function returns GIT_ENOTFOUND and
 * does not advance.  That can't happen for tree and index iterators.
 */
GIT_INLINE(int) git_iterator_advance_into(
	const git_index_entry **entry, git_iterator *iter)
{
	return iter->cb->advance_into(entry, iter);
}

/* Advance over a directory and check if it contains no files or just
 * ignored files.
 *
 * In a tree or the index, all directories will contain files, but in the
 * working directory it is possible to have an empty directory tree or a
 * tree that only contains ignored files.  Many Git operations treat these
 * cases specially.  This advances over a directory (presumably an
 * untracked directory) but checks during the scan if there are any files
 * and any non-ignored files.
 */
GIT_INLINE(int) git_iterator_advance_over(
	const git_index_entry **entry,
	git_iterator_status_t *status,
	git_iterator *iter)
{
	return iter->cb->advance_over(entry, status, iter);
}

/**
 * Go back to the start of the iteration.
 */
GIT_INLINE(int) git_iterator_reset(git_iterator *iter)
{
	return iter->cb->reset(iter);
}

/**
 * Go back to the start of the iteration after updating the `start` and
 * `end` pathname boundaries of the iteration.
 */
extern int git_iterator_reset_range(
	git_iterator *iter, const char *start, const char *end);

GIT_INLINE(git_iterator_t) git_iterator_type(git_iterator *iter)
{
	return iter->type;
}

GIT_INLINE(git_repository *) git_iterator_owner(git_iterator *iter)
{
	return iter->repo;
}

GIT_INLINE(git_index *) git_iterator_index(git_iterator *iter)
{
	return iter->index;
}

GIT_INLINE(git_iterator_flag_t) git_iterator_flags(git_iterator *iter)
{
	return iter->flags;
}

GIT_INLINE(bool) git_iterator_ignore_case(git_iterator *iter)
{
	return ((iter->flags & GIT_ITERATOR_IGNORE_CASE) != 0);
}

extern int git_iterator_set_ignore_case(
	git_iterator *iter, bool ignore_case);

extern int git_iterator_current_tree_entry(
	const git_tree_entry **entry_out, git_iterator *iter);

extern int git_iterator_current_parent_tree(
	const git_tree **tree_out, git_iterator *iter, size_t depth);

extern bool git_iterator_current_is_ignored(git_iterator *iter);

extern bool git_iterator_current_tree_is_ignored(git_iterator *iter);

/**
 * Get full path of the current item from a workdir iterator.  This will
 * return NULL for a non-workdir iterator.  The git_str is still owned by
 * the iterator; this is exposed just for efficiency.
 */
extern int git_iterator_current_workdir_path(
	git_str **path, git_iterator *iter);

/**
 * Retrieve the index stored in the iterator.
 *
 * Only implemented for the workdir and index iterators.
 */
extern git_index *git_iterator_index(git_iterator *iter);

typedef int (*git_iterator_foreach_cb)(
	const git_index_entry *entry,
	void *data);

/**
 * Walk the given iterator and invoke the callback for each path
 * contained in the iterator.
 */
extern int git_iterator_foreach(
	git_iterator *iterator,
	git_iterator_foreach_cb cb,
	void *data);

typedef int (*git_iterator_walk_cb)(
	const git_index_entry **entries,
	void *data);

/**
 * Walk the given iterators in lock-step.  The given callback will be
 * called for each unique path, with the index entry in each iterator
 * (or NULL if the given iterator does not contain that path).
 */
extern int git_iterator_walk(
	git_iterator **iterators,
	size_t cnt,
	git_iterator_walk_cb cb,
	void *data);

#endif
