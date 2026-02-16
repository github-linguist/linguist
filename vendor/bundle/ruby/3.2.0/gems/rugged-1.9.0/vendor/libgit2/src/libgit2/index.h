/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_index_h__
#define INCLUDE_index_h__

#include "common.h"

#include "futils.h"
#include "filebuf.h"
#include "vector.h"
#include "tree-cache.h"
#include "index_map.h"
#include "git2/odb.h"
#include "git2/index.h"

#define GIT_INDEX_FILE "index"
#define GIT_INDEX_FILE_MODE 0666

extern bool git_index__enforce_unsaved_safety;

struct git_index {
	git_refcount rc;

	char *index_file_path;
	git_futils_filestamp stamp;
	unsigned char checksum[GIT_HASH_MAX_SIZE];

	git_vector entries;
	git_index_entrymap entries_map;

	git_vector deleted; /* deleted entries if readers > 0 */
	git_atomic32 readers; /* number of active iterators */

	git_oid_t oid_type;

	unsigned int on_disk:1;
	unsigned int ignore_case:1;
	unsigned int distrust_filemode:1;
	unsigned int no_symlinks:1;
	unsigned int dirty:1;	/* whether we have unsaved changes */

	git_tree_cache *tree;
	git_pool tree_pool;

	git_vector names;
	git_vector reuc;

	git_vector_cmp entries_cmp_path;
	git_vector_cmp entries_search;
	git_vector_cmp entries_search_path;
	git_vector_cmp reuc_search;

	unsigned int version;
};

struct git_index_iterator {
	git_index *index;
	git_vector snap;
	size_t cur;
};

struct git_index_conflict_iterator {
	git_index *index;
	size_t cur;
};

extern void git_index_entry__init_from_stat(
	git_index_entry *entry, struct stat *st, bool trust_mode);

/* Index entry comparison functions for array sorting */
extern int git_index_entry_cmp(const void *a, const void *b);
extern int git_index_entry_icmp(const void *a, const void *b);

/* Index entry search functions for search using a search spec */
extern int git_index_entry_srch(const void *a, const void *b);
extern int git_index_entry_isrch(const void *a, const void *b);

/* Index time handling functions */
GIT_INLINE(bool) git_index_time_eq(const git_index_time *one, const git_index_time *two)
{
	if (one->seconds != two->seconds)
		return false;

#ifdef GIT_USE_NSEC
	if (one->nanoseconds != two->nanoseconds)
		return false;
#endif

	return true;
}

/*
 * Test if the given index time is newer than the given existing index entry.
 * If the timestamps are exactly equivalent, then the given index time is
 * considered "racily newer" than the existing index entry.
 */
GIT_INLINE(bool) git_index_entry_newer_than_index(
	const git_index_entry *entry, git_index *index)
{
	/* If we never read the index, we can't have this race either */
	if (!index || index->stamp.mtime.tv_sec == 0)
		return false;

	/* If the timestamp is the same or newer than the index, it's racy */
#if defined(GIT_USE_NSEC)
	if ((int32_t)index->stamp.mtime.tv_sec < entry->mtime.seconds)
		return true;
	else if ((int32_t)index->stamp.mtime.tv_sec > entry->mtime.seconds)
		return false;
	else
		return (uint32_t)index->stamp.mtime.tv_nsec <= entry->mtime.nanoseconds;
#else
	return ((int32_t)index->stamp.mtime.tv_sec) <= entry->mtime.seconds;
#endif
}

/* Search index for `path`, returning GIT_ENOTFOUND if it does not exist
 * (but not setting an error message).
 *
 * `at_pos` is set to the position where it is or would be inserted.
 * Pass `path_len` as strlen of path or 0 to call strlen internally.
 */
extern int git_index__find_pos(
	size_t *at_pos, git_index *index, const char *path, size_t path_len, int stage);

extern int git_index__fill(git_index *index, const git_vector *source_entries);

extern void git_index__set_ignore_case(git_index *index, bool ignore_case);

extern unsigned int git_index__create_mode(unsigned int mode);

GIT_INLINE(const git_futils_filestamp *) git_index__filestamp(git_index *index)
{
	return &index->stamp;
}

GIT_INLINE(unsigned char *) git_index__checksum(git_index *index)
{
	return index->checksum;
}

/* SHA256-aware internal functions */

extern int git_index__new(
	git_index **index_out,
	git_oid_t oid_type);

extern int git_index__open(
	git_index **index_out,
	const char *index_path,
	git_oid_t oid_type);

/* Copy the current entries vector *and* increment the index refcount.
 * Call `git_index__release_snapshot` when done.
 */
extern int git_index_snapshot_new(git_vector *snap, git_index *index);
extern void git_index_snapshot_release(git_vector *snap, git_index *index);

/* Allow searching in a snapshot; entries must already be sorted! */
extern int git_index_snapshot_find(
	size_t *at_pos, git_vector *snap, git_vector_cmp entry_srch,
	const char *path, size_t path_len, int stage);

/* Replace an index with a new index */
int git_index_read_index(git_index *index, const git_index *new_index);

GIT_INLINE(int) git_index_is_dirty(git_index *index)
{
	return index->dirty;
}

extern int git_index_read_safely(git_index *index);

typedef struct {
	git_index *index;
	git_filebuf file;
	unsigned int should_write:1;
} git_indexwriter;

#define GIT_INDEXWRITER_INIT { NULL, GIT_FILEBUF_INIT }

/* Lock the index for eventual writing. */
extern int git_indexwriter_init(git_indexwriter *writer, git_index *index);

/* Lock the index for eventual writing by a repository operation: a merge,
 * revert, cherry-pick or a rebase.  Note that the given checkout strategy
 * will be updated for the operation's use so that checkout will not write
 * the index.
 */
extern int git_indexwriter_init_for_operation(
	git_indexwriter *writer,
	git_repository *repo,
	unsigned int *checkout_strategy);

/* Write the index and unlock it. */
extern int git_indexwriter_commit(git_indexwriter *writer);

/* Cleanup an index writing session, unlocking the file (if it is still
 * locked and freeing any data structures.
 */
extern void git_indexwriter_cleanup(git_indexwriter *writer);

#endif
