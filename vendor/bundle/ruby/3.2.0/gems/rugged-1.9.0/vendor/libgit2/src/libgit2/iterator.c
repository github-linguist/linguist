/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "iterator.h"

#include "tree.h"
#include "index.h"
#include "path.h"

#define GIT_ITERATOR_FIRST_ACCESS   (1 << 15)
#define GIT_ITERATOR_HONOR_IGNORES  (1 << 16)
#define GIT_ITERATOR_IGNORE_DOT_GIT (1 << 17)

#define iterator__flag(I,F) ((((git_iterator *)(I))->flags & GIT_ITERATOR_ ## F) != 0)
#define iterator__ignore_case(I)       iterator__flag(I,IGNORE_CASE)
#define iterator__include_trees(I)     iterator__flag(I,INCLUDE_TREES)
#define iterator__dont_autoexpand(I)   iterator__flag(I,DONT_AUTOEXPAND)
#define iterator__do_autoexpand(I)    !iterator__flag(I,DONT_AUTOEXPAND)
#define iterator__include_conflicts(I) iterator__flag(I,INCLUDE_CONFLICTS)
#define iterator__has_been_accessed(I) iterator__flag(I,FIRST_ACCESS)
#define iterator__honor_ignores(I)     iterator__flag(I,HONOR_IGNORES)
#define iterator__ignore_dot_git(I)    iterator__flag(I,IGNORE_DOT_GIT)
#define iterator__descend_symlinks(I)  iterator__flag(I,DESCEND_SYMLINKS)

static void iterator_set_ignore_case(git_iterator *iter, bool ignore_case)
{
	int (*vector_cmp)(const void *a, const void *b);

	if (ignore_case)
		iter->flags |= GIT_ITERATOR_IGNORE_CASE;
	else
		iter->flags &= ~GIT_ITERATOR_IGNORE_CASE;

	iter->strcomp = ignore_case ? git__strcasecmp : git__strcmp;
	iter->strncomp = ignore_case ? git__strncasecmp : git__strncmp;
	iter->prefixcomp = ignore_case ? git__prefixcmp_icase : git__prefixcmp;
	iter->entry_srch = ignore_case ? git_index_entry_isrch : git_index_entry_srch;

	vector_cmp = ignore_case ? git__strcasecmp_cb : git__strcmp_cb;

	git_vector_set_cmp(&iter->pathlist, vector_cmp);
}

static int iterator_range_init(
	git_iterator *iter, const char *start, const char *end)
{
	if (start && *start) {
		iter->start = git__strdup(start);
		GIT_ERROR_CHECK_ALLOC(iter->start);

		iter->start_len = strlen(iter->start);
	}

	if (end && *end) {
		iter->end = git__strdup(end);
		GIT_ERROR_CHECK_ALLOC(iter->end);

		iter->end_len = strlen(iter->end);
	}

	iter->started = (iter->start == NULL);
	iter->ended = false;

	return 0;
}

static void iterator_range_free(git_iterator *iter)
{
	if (iter->start) {
		git__free(iter->start);
		iter->start = NULL;
		iter->start_len = 0;
	}

	if (iter->end) {
		git__free(iter->end);
		iter->end = NULL;
		iter->end_len = 0;
	}
}

static int iterator_reset_range(
	git_iterator *iter, const char *start, const char *end)
{
	iterator_range_free(iter);
	return iterator_range_init(iter, start, end);
}

static int iterator_pathlist_init(git_iterator *iter, git_strarray *pathlist)
{
	size_t i;

	if (git_vector_init(&iter->pathlist, pathlist->count, NULL) < 0)
		return -1;

	for (i = 0; i < pathlist->count; i++) {
		if (!pathlist->strings[i])
			continue;

		if (git_vector_insert(&iter->pathlist, pathlist->strings[i]) < 0)
			return -1;
	}

	return 0;
}

static int iterator_init_common(
	git_iterator *iter,
	git_repository *repo,
	git_index *index,
	git_iterator_options *given_opts)
{
	static git_iterator_options default_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_iterator_options *options = given_opts ? given_opts : &default_opts;
	bool ignore_case;
	int precompose;
	int error;

	iter->repo = repo;
	iter->index = index;
	iter->flags = options->flags;

	if ((iter->flags & GIT_ITERATOR_IGNORE_CASE) != 0) {
		ignore_case = true;
	} else if ((iter->flags & GIT_ITERATOR_DONT_IGNORE_CASE) != 0) {
		ignore_case = false;
	} else if (repo) {
		git_index *index;

		if ((error = git_repository_index__weakptr(&index, iter->repo)) < 0)
			return error;

		ignore_case = !!index->ignore_case;

		if (ignore_case == 1)
			iter->flags |= GIT_ITERATOR_IGNORE_CASE;
		else
			iter->flags |= GIT_ITERATOR_DONT_IGNORE_CASE;
	} else {
		ignore_case = false;
	}

	/* try to look up precompose and set flag if appropriate */
	if (repo &&
		(iter->flags & GIT_ITERATOR_PRECOMPOSE_UNICODE) == 0 &&
		(iter->flags & GIT_ITERATOR_DONT_PRECOMPOSE_UNICODE) == 0) {

		if (git_repository__configmap_lookup(&precompose, repo, GIT_CONFIGMAP_PRECOMPOSE) < 0)
			git_error_clear();
		else if (precompose)
			iter->flags |= GIT_ITERATOR_PRECOMPOSE_UNICODE;
	}

	if ((iter->flags & GIT_ITERATOR_DONT_AUTOEXPAND))
		iter->flags |= GIT_ITERATOR_INCLUDE_TREES;

	if ((error = iterator_range_init(iter, options->start, options->end)) < 0 ||
		(error = iterator_pathlist_init(iter, &options->pathlist)) < 0)
		return error;

	iterator_set_ignore_case(iter, ignore_case);
	return 0;
}

static void iterator_clear(git_iterator *iter)
{
	iter->started = false;
	iter->ended = false;
	iter->stat_calls = 0;
	iter->pathlist_walk_idx = 0;
	iter->flags &= ~GIT_ITERATOR_FIRST_ACCESS;
}

GIT_INLINE(bool) iterator_has_started(
	git_iterator *iter, const char *path, bool is_submodule)
{
	size_t path_len;

	if (iter->start == NULL || iter->started == true)
		return true;

	/* the starting path is generally a prefix - we have started once we
	 * are prefixed by this path
	 */
	iter->started = (iter->prefixcomp(path, iter->start) >= 0);

	if (iter->started)
		return true;

	path_len = strlen(path);

	/* if, however, we are a submodule, then we support `start` being
	 * suffixed with a `/` for crazy legacy reasons.  match `submod`
	 * with a start path of `submod/`.
	 */
	if (is_submodule && iter->start_len && path_len == iter->start_len - 1 &&
		iter->start[iter->start_len-1] == '/')
		return true;

	/* if, however, our current path is a directory, and our starting path
	 * is _beneath_ that directory, then recurse into the directory (even
	 * though we have not yet "started")
	 */
	if (path_len > 0 && path[path_len-1] == '/' &&
		iter->strncomp(path, iter->start, path_len) == 0)
		return true;

	return false;
}

GIT_INLINE(bool) iterator_has_ended(git_iterator *iter, const char *path)
{
	if (iter->end == NULL)
		return false;
	else if (iter->ended)
		return true;

	iter->ended = (iter->prefixcomp(path, iter->end) > 0);
	return iter->ended;
}

/* walker for the index and tree iterator that allows it to walk the sorted
 * pathlist entries alongside sorted iterator entries.
 */
static bool iterator_pathlist_next_is(git_iterator *iter, const char *path)
{
	char *p;
	size_t path_len, p_len, cmp_len, i;
	int cmp;

	if (iter->pathlist.length == 0)
		return true;

	git_vector_sort(&iter->pathlist);

	path_len = strlen(path);

	/* for comparison, drop the trailing slash on the current '/' */
	if (path_len && path[path_len-1] == '/')
		path_len--;

	for (i = iter->pathlist_walk_idx; i < iter->pathlist.length; i++) {
		p = iter->pathlist.contents[i];
		p_len = strlen(p);

		if (p_len && p[p_len-1] == '/')
			p_len--;

		cmp_len = min(path_len, p_len);

		/* see if the pathlist entry is a prefix of this path */
		cmp = iter->strncomp(p, path, cmp_len);

		/* prefix match - see if there's an exact match, or if we were
		 * given a path that matches the directory
		 */
		if (cmp == 0) {
			/* if this pathlist entry is not suffixed with a '/' then
			 * it matches a path that is a file or a directory.
			 * (eg, pathlist = "foo" and path is "foo" or "foo/" or
			 * "foo/something")
			 */
			if (p[cmp_len] == '\0' &&
				(path[cmp_len] == '\0' || path[cmp_len] == '/'))
				return true;

			/* if this pathlist entry _is_ suffixed with a '/' then
			 * it matches only paths that are directories.
			 * (eg, pathlist = "foo/" and path is "foo/" or "foo/something")
			 */
			if (p[cmp_len] == '/' && path[cmp_len] == '/')
				return true;
		}

		/* this pathlist entry sorts before the given path, try the next */
		else if (cmp < 0) {
			iter->pathlist_walk_idx++;
			continue;
		}

		/* this pathlist sorts after the given path, no match. */
		else if (cmp > 0) {
			break;
		}
	}

	return false;
}

typedef enum {
	ITERATOR_PATHLIST_NONE = 0,
	ITERATOR_PATHLIST_IS_FILE = 1,
	ITERATOR_PATHLIST_IS_DIR = 2,
	ITERATOR_PATHLIST_IS_PARENT = 3,
	ITERATOR_PATHLIST_FULL = 4
} iterator_pathlist_search_t;

static iterator_pathlist_search_t iterator_pathlist_search(
	git_iterator *iter, const char *path, size_t path_len)
{
	int (*vector_cmp)(const void *a, const void *b);
	const char *p;
	size_t idx;
	int error;

	if (iter->pathlist.length == 0)
		return ITERATOR_PATHLIST_FULL;

	git_vector_sort(&iter->pathlist);

	vector_cmp = (iter->flags & GIT_ITERATOR_IGNORE_CASE) != 0 ?
		git__strcasecmp_cb : git__strcmp_cb;

	error = git_vector_bsearch2(&idx, &iter->pathlist, vector_cmp, path);

	/* the given path was found in the pathlist.  since the pathlist only
	 * matches directories when they're suffixed with a '/', analyze the
	 * path string to determine whether it's a directory or not.
	 */
	if (error == 0) {
		if (path_len && path[path_len-1] == '/')
			return ITERATOR_PATHLIST_IS_DIR;

		return ITERATOR_PATHLIST_IS_FILE;
	}

	/* at this point, the path we're examining may be a directory (though we
	 * don't know that yet, since we're avoiding a stat unless it's necessary)
	 * so walk the pathlist looking for the given path with a '/' after it,
	 */
	while ((p = git_vector_get(&iter->pathlist, idx)) != NULL) {
		if (iter->prefixcomp(p, path) != 0)
			break;

		/* an exact match would have been matched by the bsearch above */
		GIT_ASSERT_WITH_RETVAL(p[path_len], ITERATOR_PATHLIST_NONE);

		/* is this a literal directory entry (eg `foo/`) or a file beneath */
		if (p[path_len] == '/') {
			return (p[path_len+1] == '\0') ?
				ITERATOR_PATHLIST_IS_DIR :
				ITERATOR_PATHLIST_IS_PARENT;
		}

		if (p[path_len] > '/')
			break;

		idx++;
	}

	return ITERATOR_PATHLIST_NONE;
}

/* Empty iterator */

static int empty_iterator_noop(const git_index_entry **e, git_iterator *i)
{
	GIT_UNUSED(i);

	if (e)
		*e = NULL;

	return GIT_ITEROVER;
}

static int empty_iterator_advance_over(
	const git_index_entry **e,
	git_iterator_status_t *s,
	git_iterator *i)
{
	*s = GIT_ITERATOR_STATUS_EMPTY;
	return empty_iterator_noop(e, i);
}

static int empty_iterator_reset(git_iterator *i)
{
	GIT_UNUSED(i);
	return 0;
}

static void empty_iterator_free(git_iterator *i)
{
	GIT_UNUSED(i);
}

typedef struct {
	git_iterator base;
	git_iterator_callbacks cb;
} empty_iterator;

int git_iterator_for_nothing(
	git_iterator **out,
	git_iterator_options *options)
{
	empty_iterator *iter;

	static git_iterator_callbacks callbacks = {
		empty_iterator_noop,
		empty_iterator_noop,
		empty_iterator_noop,
		empty_iterator_advance_over,
		empty_iterator_reset,
		empty_iterator_free
	};

	*out = NULL;

	iter = git__calloc(1, sizeof(empty_iterator));
	GIT_ERROR_CHECK_ALLOC(iter);

	iter->base.type = GIT_ITERATOR_EMPTY;
	iter->base.cb = &callbacks;
	iter->base.flags = options->flags;

	*out = &iter->base;
	return 0;
}

/* Tree iterator */

typedef struct {
	git_tree_entry *tree_entry;
	const char *parent_path;
} tree_iterator_entry;

typedef struct {
	git_tree *tree;

	/* path to this particular frame (folder) */
	git_str path;

	/* a sorted list of the entries for this frame (folder), these are
	 * actually pointers to the iterator's entry pool.
	 */
	git_vector entries;
	tree_iterator_entry *current;

	size_t next_idx;

	/* on case insensitive iterations, we also have an array of other
	 * paths that were case insensitively equal to this one, and their
	 * tree objects.  we have coalesced the tree entries into this frame.
	 * a child `tree_iterator_entry` will contain a pointer to its actual
	 * parent path.
	 */
	git_vector similar_trees;
	git_array_t(git_str) similar_paths;
} tree_iterator_frame;

typedef struct {
	git_iterator base;
	git_tree *root;
	git_array_t(tree_iterator_frame) frames;

	git_index_entry entry;
	git_str entry_path;

	/* a pool of entries to reduce the number of allocations */
	git_pool entry_pool;
} tree_iterator;

GIT_INLINE(tree_iterator_frame *) tree_iterator_parent_frame(
	tree_iterator *iter)
{
	return iter->frames.size > 1 ?
		&iter->frames.ptr[iter->frames.size-2] : NULL;
}

GIT_INLINE(tree_iterator_frame *) tree_iterator_current_frame(
	tree_iterator *iter)
{
	return iter->frames.size ? &iter->frames.ptr[iter->frames.size-1] : NULL;
}

GIT_INLINE(int) tree_entry_cmp(
	const git_tree_entry *a, const git_tree_entry *b, bool icase)
{
	return git_fs_path_cmp(
		a->filename, a->filename_len, a->attr == GIT_FILEMODE_TREE,
		b->filename, b->filename_len, b->attr == GIT_FILEMODE_TREE,
		icase ? git__strncasecmp : git__strncmp);
}

GIT_INLINE(int) tree_iterator_entry_cmp_icase(
	const void *ptr_a, const void *ptr_b)
{
	const tree_iterator_entry *a = (const tree_iterator_entry *)ptr_a;
	const tree_iterator_entry *b = (const tree_iterator_entry *)ptr_b;

	return tree_entry_cmp(a->tree_entry, b->tree_entry, true);
}

static int tree_iterator_entry_sort_icase(const void *ptr_a, const void *ptr_b)
{
	const tree_iterator_entry *a = (const tree_iterator_entry *)ptr_a;
	const tree_iterator_entry *b = (const tree_iterator_entry *)ptr_b;

	int c = tree_entry_cmp(a->tree_entry, b->tree_entry, true);

	/* stabilize the sort order for filenames that are (case insensitively)
	 * the same by examining the parent path (case sensitively) before
	 * falling back to a case sensitive sort of the filename.
	 */
	if (!c && a->parent_path != b->parent_path)
		c = git__strcmp(a->parent_path, b->parent_path);

	if (!c)
		c = tree_entry_cmp(a->tree_entry, b->tree_entry, false);

	return c;
}

static int tree_iterator_compute_path(
	git_str *out,
	tree_iterator_entry *entry)
{
	git_str_clear(out);

	if (entry->parent_path)
		git_str_joinpath(out, entry->parent_path, entry->tree_entry->filename);
	else
		git_str_puts(out, entry->tree_entry->filename);

	if (git_tree_entry__is_tree(entry->tree_entry))
		git_str_putc(out, '/');

	if (git_str_oom(out))
		return -1;

	return 0;
}

static int tree_iterator_frame_init(
	tree_iterator *iter,
	git_tree *tree,
	tree_iterator_entry *frame_entry)
{
	tree_iterator_frame *new_frame = NULL;
	tree_iterator_entry *new_entry;
	git_tree *dup = NULL;
	git_tree_entry *tree_entry;
	git_vector_cmp cmp;
	size_t i;
	int error = 0;

	new_frame = git_array_alloc(iter->frames);
	GIT_ERROR_CHECK_ALLOC(new_frame);

	if ((error = git_tree_dup(&dup, tree)) < 0)
		goto done;

	memset(new_frame, 0x0, sizeof(tree_iterator_frame));
	new_frame->tree = dup;

	if (frame_entry &&
	    (error = tree_iterator_compute_path(&new_frame->path, frame_entry)) < 0)
		goto done;

	cmp = iterator__ignore_case(&iter->base) ?
		tree_iterator_entry_sort_icase : NULL;

	if ((error = git_vector_init(&new_frame->entries,
				     dup->entries.size, cmp)) < 0)
		goto done;

	git_array_foreach(dup->entries, i, tree_entry) {
		if ((new_entry = git_pool_malloc(&iter->entry_pool, 1)) == NULL) {
			git_error_set_oom();
			error = -1;
			goto done;
		}

		new_entry->tree_entry = tree_entry;
		new_entry->parent_path = new_frame->path.ptr;

		if ((error = git_vector_insert(&new_frame->entries, new_entry)) < 0)
			goto done;
	}

	git_vector_set_sorted(&new_frame->entries,
		!iterator__ignore_case(&iter->base));

done:
	if (error < 0) {
		git_tree_free(dup);
		git_array_pop(iter->frames);
	}

	return error;
}

GIT_INLINE(tree_iterator_entry *) tree_iterator_current_entry(
	tree_iterator_frame *frame)
{
	return frame->current;
}

GIT_INLINE(int) tree_iterator_frame_push_neighbors(
	tree_iterator *iter,
	tree_iterator_frame *parent_frame,
	tree_iterator_frame *frame,
	const char *filename)
{
	tree_iterator_entry *entry, *new_entry;
	git_tree *tree = NULL;
	git_tree_entry *tree_entry;
	git_str *path;
	size_t new_size, i;
	int error = 0;

	while (parent_frame->next_idx < parent_frame->entries.length) {
		entry = parent_frame->entries.contents[parent_frame->next_idx];

		if (strcasecmp(filename, entry->tree_entry->filename) != 0)
			break;

		if ((error = git_tree_lookup(&tree,
			iter->base.repo, &entry->tree_entry->oid)) < 0)
			break;

		if (git_vector_insert(&parent_frame->similar_trees, tree) < 0)
			break;

		path = git_array_alloc(parent_frame->similar_paths);
		GIT_ERROR_CHECK_ALLOC(path);

		memset(path, 0, sizeof(git_str));

		if ((error = tree_iterator_compute_path(path, entry)) < 0)
			break;

		GIT_ERROR_CHECK_ALLOC_ADD(&new_size,
			frame->entries.length, tree->entries.size);
		git_vector_size_hint(&frame->entries, new_size);

		git_array_foreach(tree->entries, i, tree_entry) {
			new_entry = git_pool_malloc(&iter->entry_pool, 1);
			GIT_ERROR_CHECK_ALLOC(new_entry);

			new_entry->tree_entry = tree_entry;
			new_entry->parent_path = path->ptr;

			if ((error = git_vector_insert(&frame->entries, new_entry)) < 0)
				break;
		}

		if (error)
			break;

		parent_frame->next_idx++;
	}

	return error;
}

GIT_INLINE(int) tree_iterator_frame_push(
	tree_iterator *iter, tree_iterator_entry *entry)
{
	tree_iterator_frame *parent_frame, *frame;
	git_tree *tree = NULL;
	int error;

	if ((error = git_tree_lookup(&tree,
			iter->base.repo, &entry->tree_entry->oid)) < 0 ||
		(error = tree_iterator_frame_init(iter, tree, entry)) < 0)
		goto done;

	parent_frame = tree_iterator_parent_frame(iter);
	frame = tree_iterator_current_frame(iter);

	/* if we're case insensitive, then we may have another directory that
	 * is (case insensitively) equal to this one.  coalesce those children
	 * into this tree.
	 */
	if (iterator__ignore_case(&iter->base))
		error = tree_iterator_frame_push_neighbors(iter,
			parent_frame, frame, entry->tree_entry->filename);

done:
	git_tree_free(tree);
	return error;
}

static int tree_iterator_frame_pop(tree_iterator *iter)
{
	tree_iterator_frame *frame;
	git_str *buf = NULL;
	git_tree *tree;
	size_t i;

	GIT_ASSERT(iter->frames.size);

	frame = git_array_pop(iter->frames);

	git_vector_dispose(&frame->entries);
	git_tree_free(frame->tree);

	do {
		buf = git_array_pop(frame->similar_paths);
		git_str_dispose(buf);
	} while (buf != NULL);

	git_array_clear(frame->similar_paths);

	git_vector_foreach(&frame->similar_trees, i, tree)
		git_tree_free(tree);

	git_vector_dispose(&frame->similar_trees);

	git_str_dispose(&frame->path);

	return 0;
}

static int tree_iterator_current(
	const git_index_entry **out, git_iterator *i)
{
	tree_iterator *iter = (tree_iterator *)i;

	if (!iterator__has_been_accessed(i))
		return iter->base.cb->advance(out, i);

	if (!iter->frames.size) {
		*out = NULL;
		return GIT_ITEROVER;
	}

	*out = &iter->entry;
	return 0;
}

static void tree_iterator_set_current(
	tree_iterator *iter,
	tree_iterator_frame *frame,
	tree_iterator_entry *entry)
{
	git_tree_entry *tree_entry = entry->tree_entry;

	frame->current = entry;

	memset(&iter->entry, 0x0, sizeof(git_index_entry));

	iter->entry.mode = tree_entry->attr;
	iter->entry.path = iter->entry_path.ptr;
	git_oid_cpy(&iter->entry.id, &tree_entry->oid);
}

static int tree_iterator_advance(const git_index_entry **out, git_iterator *i)
{
	tree_iterator *iter = (tree_iterator *)i;
	int error = 0;

	iter->base.flags |= GIT_ITERATOR_FIRST_ACCESS;

	/* examine tree entries until we find the next one to return */
	while (true) {
		tree_iterator_entry *prev_entry, *entry;
		tree_iterator_frame *frame;
		bool is_tree;

		if ((frame = tree_iterator_current_frame(iter)) == NULL) {
			error = GIT_ITEROVER;
			break;
		}

		/* no more entries in this frame.  pop the frame out */
		if (frame->next_idx == frame->entries.length) {
			if ((error = tree_iterator_frame_pop(iter)) < 0)
				break;

			continue;
		}

		/* we may have coalesced the contents of case-insensitively same-named
		 * directories, so do the sort now.
		 */
		if (frame->next_idx == 0 && !git_vector_is_sorted(&frame->entries))
			git_vector_sort(&frame->entries);

		/* we have more entries in the current frame, that's our next entry */
		prev_entry = tree_iterator_current_entry(frame);
		entry = frame->entries.contents[frame->next_idx];
		frame->next_idx++;

		/* we can have collisions when iterating case insensitively.  (eg,
		 * 'A/a' and 'a/A').  squash this one if it's already been seen.
		 */
		if (iterator__ignore_case(&iter->base) &&
			prev_entry &&
			tree_iterator_entry_cmp_icase(prev_entry, entry) == 0)
			continue;

		if ((error = tree_iterator_compute_path(&iter->entry_path, entry)) < 0)
			break;

		/* if this path is before our start, advance over this entry */
		if (!iterator_has_started(&iter->base, iter->entry_path.ptr, false))
			continue;

		/* if this path is after our end, stop */
		if (iterator_has_ended(&iter->base, iter->entry_path.ptr)) {
			error = GIT_ITEROVER;
			break;
		}

		/* if we have a list of paths we're interested in, examine it */
		if (!iterator_pathlist_next_is(&iter->base, iter->entry_path.ptr))
			continue;

		is_tree = git_tree_entry__is_tree(entry->tree_entry);

		/* if we are *not* including trees then advance over this entry */
		if (is_tree && !iterator__include_trees(iter)) {

			/* if we've found a tree (and are not returning it to the caller)
			 * and we are autoexpanding, then we want to return the first
			 * child.  push the new directory and advance.
			 */
			if (iterator__do_autoexpand(iter)) {
				if ((error = tree_iterator_frame_push(iter, entry)) < 0)
					break;
			}

			continue;
		}

		tree_iterator_set_current(iter, frame, entry);

		/* if we are autoexpanding, then push this as a new frame, so that
		 * the next call to `advance` will dive into this directory.
		 */
		if (is_tree && iterator__do_autoexpand(iter))
			error = tree_iterator_frame_push(iter, entry);

		break;
	}

	if (out)
		*out = (error == 0) ? &iter->entry : NULL;

	return error;
}

static int tree_iterator_advance_into(
	const git_index_entry **out, git_iterator *i)
{
	tree_iterator *iter = (tree_iterator *)i;
	tree_iterator_frame *frame;
	tree_iterator_entry *prev_entry;
	int error;

	if (out)
		*out = NULL;

	if ((frame = tree_iterator_current_frame(iter)) == NULL)
		return GIT_ITEROVER;

	/* get the last seen entry */
	prev_entry = tree_iterator_current_entry(frame);

	/* it's legal to call advance_into when auto-expand is on.  in this case,
	 * we will have pushed a new (empty) frame on to the stack for this
	 * new directory.  since it's empty, its current_entry should be null.
	 */
	GIT_ASSERT(iterator__do_autoexpand(i) ^ (prev_entry != NULL));

	if (prev_entry) {
		if (!git_tree_entry__is_tree(prev_entry->tree_entry))
			return 0;

		if ((error = tree_iterator_frame_push(iter, prev_entry)) < 0)
			return error;
	}

	/* we've advanced into the directory in question, let advance
	 * find the first entry
	 */
	return tree_iterator_advance(out, i);
}

static int tree_iterator_advance_over(
	const git_index_entry **out,
	git_iterator_status_t *status,
	git_iterator *i)
{
	*status = GIT_ITERATOR_STATUS_NORMAL;
	return git_iterator_advance(out, i);
}

static void tree_iterator_clear(tree_iterator *iter)
{
	while (iter->frames.size)
		tree_iterator_frame_pop(iter);

	git_array_clear(iter->frames);

	git_pool_clear(&iter->entry_pool);
	git_str_clear(&iter->entry_path);

	iterator_clear(&iter->base);
}

static int tree_iterator_init(tree_iterator *iter)
{
	int error;

	if ((error = git_pool_init(&iter->entry_pool, sizeof(tree_iterator_entry))) < 0 ||
	    (error = tree_iterator_frame_init(iter, iter->root, NULL)) < 0)
		return error;

	iter->base.flags &= ~GIT_ITERATOR_FIRST_ACCESS;

	return 0;
}

static int tree_iterator_reset(git_iterator *i)
{
	tree_iterator *iter = (tree_iterator *)i;

	tree_iterator_clear(iter);
	return tree_iterator_init(iter);
}

static void tree_iterator_free(git_iterator *i)
{
	tree_iterator *iter = (tree_iterator *)i;

	tree_iterator_clear(iter);

	git_tree_free(iter->root);
	git_str_dispose(&iter->entry_path);
}

int git_iterator_for_tree(
	git_iterator **out,
	git_tree *tree,
	git_iterator_options *options)
{
	tree_iterator *iter;
	int error;

	static git_iterator_callbacks callbacks = {
		tree_iterator_current,
		tree_iterator_advance,
		tree_iterator_advance_into,
		tree_iterator_advance_over,
		tree_iterator_reset,
		tree_iterator_free
	};

	*out = NULL;

	if (tree == NULL)
		return git_iterator_for_nothing(out, options);

	iter = git__calloc(1, sizeof(tree_iterator));
	GIT_ERROR_CHECK_ALLOC(iter);

	iter->base.type = GIT_ITERATOR_TREE;
	iter->base.cb = &callbacks;

	if ((error = iterator_init_common(&iter->base,
			git_tree_owner(tree), NULL, options)) < 0 ||
		(error = git_tree_dup(&iter->root, tree)) < 0 ||
		(error = tree_iterator_init(iter)) < 0)
		goto on_error;

	*out = &iter->base;
	return 0;

on_error:
	git_iterator_free(&iter->base);
	return error;
}

int git_iterator_current_tree_entry(
	const git_tree_entry **tree_entry, git_iterator *i)
{
	tree_iterator *iter;
	tree_iterator_frame *frame;
	tree_iterator_entry *entry;

	GIT_ASSERT(i->type == GIT_ITERATOR_TREE);

	iter = (tree_iterator *)i;

	frame = tree_iterator_current_frame(iter);
	entry = tree_iterator_current_entry(frame);

	*tree_entry = entry->tree_entry;
	return 0;
}

int git_iterator_current_parent_tree(
	const git_tree **parent_tree, git_iterator *i, size_t depth)
{
	tree_iterator *iter;
	tree_iterator_frame *frame;

	GIT_ASSERT(i->type == GIT_ITERATOR_TREE);

	iter = (tree_iterator *)i;

	GIT_ASSERT(depth < iter->frames.size);
	frame = &iter->frames.ptr[iter->frames.size-depth-1];

	*parent_tree = frame->tree;
	return 0;
}

/* Filesystem iterator */

typedef struct {
	struct stat st;
	size_t path_len;
	iterator_pathlist_search_t match;
	git_oid id;
	char path[GIT_FLEX_ARRAY];
} filesystem_iterator_entry;

typedef struct {
	git_vector entries;
	git_pool entry_pool;
	size_t next_idx;

	size_t path_len;
	int is_ignored;
} filesystem_iterator_frame;

typedef struct {
	git_iterator base;
	char *root;
	size_t root_len;

	unsigned int dirload_flags;

	git_tree *tree;
	git_index *index;
	git_vector index_snapshot;

	git_oid_t oid_type;

	git_array_t(filesystem_iterator_frame) frames;
	git_ignores ignores;

	/* info about the current entry */
	git_index_entry entry;
	git_str current_path;
	int current_is_ignored;

	/* temporary buffer for advance_over */
	git_str tmp_buf;
} filesystem_iterator;


GIT_INLINE(filesystem_iterator_frame *) filesystem_iterator_parent_frame(
	filesystem_iterator *iter)
{
	return iter->frames.size > 1 ?
		&iter->frames.ptr[iter->frames.size-2] : NULL;
}

GIT_INLINE(filesystem_iterator_frame *) filesystem_iterator_current_frame(
	filesystem_iterator *iter)
{
	return iter->frames.size ? &iter->frames.ptr[iter->frames.size-1] : NULL;
}

GIT_INLINE(filesystem_iterator_entry *) filesystem_iterator_current_entry(
	filesystem_iterator_frame *frame)
{
	return frame->next_idx == 0 ?
		NULL : frame->entries.contents[frame->next_idx-1];
}

static int filesystem_iterator_entry_cmp(const void *_a, const void *_b)
{
	const filesystem_iterator_entry *a = (const filesystem_iterator_entry *)_a;
	const filesystem_iterator_entry *b = (const filesystem_iterator_entry *)_b;

	return git__strcmp(a->path, b->path);
}

static int filesystem_iterator_entry_cmp_icase(const void *_a, const void *_b)
{
	const filesystem_iterator_entry *a = (const filesystem_iterator_entry *)_a;
	const filesystem_iterator_entry *b = (const filesystem_iterator_entry *)_b;

	return git__strcasecmp(a->path, b->path);
}

#define FILESYSTEM_MAX_DEPTH 100

/**
 * Figure out if an entry is a submodule.
 *
 * We consider it a submodule if the path is listed as a submodule in
 * either the tree or the index.
 */
static int filesystem_iterator_is_submodule(
	bool *out, filesystem_iterator *iter, const char *path, size_t path_len)
{
	bool is_submodule = false;
	int error;

	*out = false;

	/* first see if this path is a submodule in HEAD */
	if (iter->tree) {
		git_tree_entry *entry;

		error = git_tree_entry_bypath(&entry, iter->tree, path);

		if (error < 0 && error != GIT_ENOTFOUND)
			return error;

		if (!error) {
			is_submodule = (entry->attr == GIT_FILEMODE_COMMIT);
			git_tree_entry_free(entry);
		}
	}

	if (!is_submodule && iter->base.index) {
		size_t pos;

		error = git_index_snapshot_find(&pos,
			&iter->index_snapshot, iter->base.entry_srch, path, path_len, 0);

		if (error < 0 && error != GIT_ENOTFOUND)
			return error;

		if (!error) {
			git_index_entry *e = git_vector_get(&iter->index_snapshot, pos);
			is_submodule = (e->mode == GIT_FILEMODE_COMMIT);
		}
	}

	*out = is_submodule;
	return 0;
}

static void filesystem_iterator_frame_push_ignores(
	filesystem_iterator *iter,
	filesystem_iterator_entry *frame_entry,
	filesystem_iterator_frame *new_frame)
{
	filesystem_iterator_frame *previous_frame;
	const char *path = frame_entry ? frame_entry->path : "";

	if (!iterator__honor_ignores(&iter->base))
		return;

	if (git_ignore__lookup(&new_frame->is_ignored,
			&iter->ignores, path, GIT_DIR_FLAG_TRUE) < 0) {
		git_error_clear();
		new_frame->is_ignored = GIT_IGNORE_NOTFOUND;
	}

	/* if this is not the top level directory... */
	if (frame_entry) {
		const char *relative_path;

		previous_frame = filesystem_iterator_parent_frame(iter);

		/* push new ignores for files in this directory */
		relative_path = frame_entry->path + previous_frame->path_len;

		/* inherit ignored from parent if no rule specified */
		if (new_frame->is_ignored <= GIT_IGNORE_NOTFOUND)
			new_frame->is_ignored = previous_frame->is_ignored;

		git_ignore__push_dir(&iter->ignores, relative_path);
	}
}

static void filesystem_iterator_frame_pop_ignores(
	filesystem_iterator *iter)
{
	if (iterator__honor_ignores(&iter->base))
		git_ignore__pop_dir(&iter->ignores);
}

GIT_INLINE(bool) filesystem_iterator_examine_path(
	bool *is_dir_out,
	iterator_pathlist_search_t *match_out,
	filesystem_iterator *iter,
	filesystem_iterator_entry *frame_entry,
	const char *path,
	size_t path_len)
{
	bool is_dir = 0;
	iterator_pathlist_search_t match = ITERATOR_PATHLIST_FULL;

	*is_dir_out = false;
	*match_out = ITERATOR_PATHLIST_NONE;

	if (iter->base.start_len) {
		int cmp = iter->base.strncomp(path, iter->base.start, path_len);

		/* we haven't stat'ed `path` yet, so we don't yet know if it's a
		 * directory or not.  special case if the current path may be a
		 * directory that matches the start prefix.
		 */
		if (cmp == 0) {
			if (iter->base.start[path_len] == '/')
				is_dir = true;

			else if (iter->base.start[path_len] != '\0')
				cmp = -1;
		}

		if (cmp < 0)
			return false;
	}

	if (iter->base.end_len) {
		int cmp = iter->base.strncomp(path, iter->base.end, iter->base.end_len);

		if (cmp > 0)
			return false;
	}

	/* if we have a pathlist that we're limiting to, examine this path now
	 * to avoid a `stat` if we're not interested in the path.
	 */
	if (iter->base.pathlist.length) {
		/* if our parent was explicitly included, so too are we */
		if (frame_entry && frame_entry->match != ITERATOR_PATHLIST_IS_PARENT)
			match = ITERATOR_PATHLIST_FULL;
		else
			match = iterator_pathlist_search(&iter->base, path, path_len);

		if (match == ITERATOR_PATHLIST_NONE)
			return false;

		/* Ensure that the pathlist entry lines up with what we expected */
		if (match == ITERATOR_PATHLIST_IS_DIR ||
			match == ITERATOR_PATHLIST_IS_PARENT)
			is_dir = true;
	}

	*is_dir_out = is_dir;
	*match_out = match;
	return true;
}

GIT_INLINE(bool) filesystem_iterator_is_dot_git(
	filesystem_iterator *iter, const char *path, size_t path_len)
{
	size_t len;

	if (!iterator__ignore_dot_git(&iter->base))
		return false;

	if ((len = path_len) < 4)
		return false;

	if (path[len - 1] == '/')
		len--;

	if (git__tolower(path[len - 1]) != 't' ||
		git__tolower(path[len - 2]) != 'i' ||
		git__tolower(path[len - 3]) != 'g' ||
		git__tolower(path[len - 4]) != '.')
		return false;

	return (len == 4 || path[len - 5] == '/');
}

static int filesystem_iterator_entry_hash(
	filesystem_iterator *iter,
	filesystem_iterator_entry *entry)
{
	git_str fullpath = GIT_STR_INIT;
	int error;

	if (S_ISDIR(entry->st.st_mode)) {
		memset(&entry->id, 0, git_oid_size(iter->oid_type));
		return 0;
	}

	if (iter->base.type == GIT_ITERATOR_WORKDIR)
		return git_repository_hashfile(&entry->id,
			iter->base.repo, entry->path, GIT_OBJECT_BLOB, NULL);

	if (!(error = git_str_joinpath(&fullpath, iter->root, entry->path)) &&
	    !(error = git_path_validate_str_length(iter->base.repo, &fullpath)))
		error = git_odb__hashfile(&entry->id, fullpath.ptr, GIT_OBJECT_BLOB, iter->oid_type);

	git_str_dispose(&fullpath);
	return error;
}

static int filesystem_iterator_entry_init(
	filesystem_iterator_entry **out,
	filesystem_iterator *iter,
	filesystem_iterator_frame *frame,
	const char *path,
	size_t path_len,
	struct stat *statbuf,
	iterator_pathlist_search_t pathlist_match)
{
	filesystem_iterator_entry *entry;
	size_t entry_size;
	int error = 0;

	*out = NULL;

	/* Make sure to append two bytes, one for the path's null
	 * termination, one for a possible trailing '/' for folders.
	 */
	GIT_ERROR_CHECK_ALLOC_ADD(&entry_size,
		sizeof(filesystem_iterator_entry), path_len);
	GIT_ERROR_CHECK_ALLOC_ADD(&entry_size, entry_size, 2);

	entry = git_pool_malloc(&frame->entry_pool, entry_size);
	GIT_ERROR_CHECK_ALLOC(entry);

	entry->path_len = path_len;
	entry->match = pathlist_match;
	memcpy(entry->path, path, path_len);
	memcpy(&entry->st, statbuf, sizeof(struct stat));

	/* Suffix directory paths with a '/' */
	if (S_ISDIR(entry->st.st_mode))
		entry->path[entry->path_len++] = '/';

	entry->path[entry->path_len] = '\0';

	if (iter->base.flags & GIT_ITERATOR_INCLUDE_HASH)
		error = filesystem_iterator_entry_hash(iter, entry);

	if (!error)
		*out = entry;

	return error;
}

static int filesystem_iterator_frame_push(
	filesystem_iterator *iter,
	filesystem_iterator_entry *frame_entry)
{
	filesystem_iterator_frame *new_frame = NULL;
	git_fs_path_diriter diriter = GIT_FS_PATH_DIRITER_INIT;
	git_str root = GIT_STR_INIT;
	const char *path;
	filesystem_iterator_entry *entry;
	struct stat statbuf;
	size_t path_len;
	int error;

	if (iter->frames.size == FILESYSTEM_MAX_DEPTH) {
		git_error_set(GIT_ERROR_REPOSITORY,
			"directory nesting too deep (%"PRIuZ")", iter->frames.size);
		return -1;
	}

	new_frame = git_array_alloc(iter->frames);
	GIT_ERROR_CHECK_ALLOC(new_frame);

	memset(new_frame, 0, sizeof(filesystem_iterator_frame));

	if (frame_entry)
		git_str_joinpath(&root, iter->root, frame_entry->path);
	else
		git_str_puts(&root, iter->root);

	if (git_str_oom(&root) ||
	    git_path_validate_str_length(iter->base.repo, &root) < 0) {
		error = -1;
		goto done;
	}

	new_frame->path_len = frame_entry ? frame_entry->path_len : 0;

	/* Any error here is equivalent to the dir not existing, skip over it */
	if ((error = git_fs_path_diriter_init(
			&diriter, root.ptr, iter->dirload_flags)) < 0) {
		error = GIT_ENOTFOUND;
		goto done;
	}

	if ((error = git_vector_init(&new_frame->entries, 64,
			iterator__ignore_case(&iter->base) ?
			filesystem_iterator_entry_cmp_icase :
			filesystem_iterator_entry_cmp)) < 0)
		goto done;

	if ((error = git_pool_init(&new_frame->entry_pool, 1)) < 0)
		goto done;

	/* check if this directory is ignored */
	filesystem_iterator_frame_push_ignores(iter, frame_entry, new_frame);

	while ((error = git_fs_path_diriter_next(&diriter)) == 0) {
		iterator_pathlist_search_t pathlist_match = ITERATOR_PATHLIST_FULL;
		git_str path_str = GIT_STR_INIT;
		bool dir_expected = false;

		if ((error = git_fs_path_diriter_fullpath(&path, &path_len, &diriter)) < 0)
			goto done;

		path_str.ptr = (char *)path;
		path_str.size = path_len;

		if ((error = git_path_validate_str_length(iter->base.repo, &path_str)) < 0)
			goto done;

		GIT_ASSERT(path_len > iter->root_len);

		/* remove the prefix if requested */
		path += iter->root_len;
		path_len -= iter->root_len;

		/* examine start / end and the pathlist to see if this path is in it.
		 * note that since we haven't yet stat'ed the path, we cannot know
		 * whether it's a directory yet or not, so this can give us an
		 * expected type (S_IFDIR or S_IFREG) that we should examine)
		 */
		if (!filesystem_iterator_examine_path(&dir_expected, &pathlist_match,
			iter, frame_entry, path, path_len))
			continue;

		/* TODO: don't need to stat if assume unchanged for this path and
		 * we have an index, we can just copy the data out of it.
		 */

		if ((error = git_fs_path_diriter_stat(&statbuf, &diriter)) < 0) {
			/* file was removed between readdir and lstat */
			if (error == GIT_ENOTFOUND)
				continue;

			/* treat the file as unreadable */
			memset(&statbuf, 0, sizeof(statbuf));
			statbuf.st_mode = GIT_FILEMODE_UNREADABLE;

			error = 0;
		}

		iter->base.stat_calls++;

		/* Ignore wacky things in the filesystem */
		if (!S_ISDIR(statbuf.st_mode) &&
			!S_ISREG(statbuf.st_mode) &&
			!S_ISLNK(statbuf.st_mode) &&
			statbuf.st_mode != GIT_FILEMODE_UNREADABLE)
			continue;

		if (filesystem_iterator_is_dot_git(iter, path, path_len))
			continue;

		/* convert submodules to GITLINK and remove trailing slashes */
		if (S_ISDIR(statbuf.st_mode)) {
			bool submodule = false;

			if ((error = filesystem_iterator_is_submodule(&submodule,
					iter, path, path_len)) < 0)
				goto done;

			if (submodule)
				statbuf.st_mode = GIT_FILEMODE_COMMIT;
		}

		/* Ensure that the pathlist entry lines up with what we expected */
		else if (dir_expected)
			continue;

		if ((error = filesystem_iterator_entry_init(&entry,
			iter, new_frame, path, path_len, &statbuf, pathlist_match)) < 0)
			goto done;

		git_vector_insert(&new_frame->entries, entry);
	}

	if (error == GIT_ITEROVER)
		error = 0;

	/* sort now that directory suffix is added */
	git_vector_sort(&new_frame->entries);

done:
	if (error < 0)
		git_array_pop(iter->frames);

	git_str_dispose(&root);
	git_fs_path_diriter_free(&diriter);
	return error;
}

GIT_INLINE(int) filesystem_iterator_frame_pop(filesystem_iterator *iter)
{
	filesystem_iterator_frame *frame;

	GIT_ASSERT(iter->frames.size);

	frame = git_array_pop(iter->frames);
	filesystem_iterator_frame_pop_ignores(iter);

	git_pool_clear(&frame->entry_pool);
	git_vector_dispose(&frame->entries);

	return 0;
}

static void filesystem_iterator_set_current(
	filesystem_iterator *iter,
	filesystem_iterator_entry *entry)
{
	/*
	 * Index entries are limited to 32 bit timestamps.  We can safely
	 * cast this since workdir times are only used in the cache; any
	 * mismatch will cause a hash recomputation which is unfortunate
	 * but affects only people who set their filetimes to 2038.
	 * (Same with the file size.)
	 */
	iter->entry.ctime.seconds = (int32_t)entry->st.st_ctime;
	iter->entry.mtime.seconds = (int32_t)entry->st.st_mtime;

#if defined(GIT_USE_NSEC)
	iter->entry.ctime.nanoseconds = entry->st.st_ctime_nsec;
	iter->entry.mtime.nanoseconds = entry->st.st_mtime_nsec;
#else
	iter->entry.ctime.nanoseconds = 0;
	iter->entry.mtime.nanoseconds = 0;
#endif

	iter->entry.dev = entry->st.st_dev;
	iter->entry.ino = entry->st.st_ino;
	iter->entry.mode = git_futils_canonical_mode(entry->st.st_mode);
	iter->entry.uid = entry->st.st_uid;
	iter->entry.gid = entry->st.st_gid;
	iter->entry.file_size = (uint32_t)entry->st.st_size;

	if (iter->base.flags & GIT_ITERATOR_INCLUDE_HASH)
		git_oid_cpy(&iter->entry.id, &entry->id);
	else
		git_oid_clear(&iter->entry.id, iter->oid_type);

	iter->entry.path = entry->path;

	iter->current_is_ignored = GIT_IGNORE_UNCHECKED;
}

static int filesystem_iterator_current(
	const git_index_entry **out, git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);

	if (!iterator__has_been_accessed(i))
		return iter->base.cb->advance(out, i);

	if (!iter->frames.size) {
		*out = NULL;
		return GIT_ITEROVER;
	}

	*out = &iter->entry;
	return 0;
}

static int filesystem_iterator_is_dir(
	bool *is_dir,
	const filesystem_iterator *iter,
	const filesystem_iterator_entry *entry)
{
	struct stat st;
	git_str fullpath = GIT_STR_INIT;
	int error = 0;

	if (S_ISDIR(entry->st.st_mode)) {
		*is_dir = 1;
		goto done;
	}

	if (!iterator__descend_symlinks(iter) || !S_ISLNK(entry->st.st_mode)) {
		*is_dir = 0;
		goto done;
	}

	if ((error = git_str_joinpath(&fullpath, iter->root, entry->path)) < 0 ||
	    (error = git_path_validate_str_length(iter->base.repo, &fullpath)) < 0 ||
	    (error = p_stat(fullpath.ptr, &st)) < 0)
		goto done;

	*is_dir = S_ISDIR(st.st_mode);

done:
	git_str_dispose(&fullpath);
	return error;
}

static int filesystem_iterator_advance(
	const git_index_entry **out, git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);
	bool is_dir;
	int error = 0;

	iter->base.flags |= GIT_ITERATOR_FIRST_ACCESS;

	/* examine filesystem entries until we find the next one to return */
	while (true) {
		filesystem_iterator_frame *frame;
		filesystem_iterator_entry *entry;

		if ((frame = filesystem_iterator_current_frame(iter)) == NULL) {
			error = GIT_ITEROVER;
			break;
		}

		/* no more entries in this frame.  pop the frame out */
		if (frame->next_idx == frame->entries.length) {
			filesystem_iterator_frame_pop(iter);
			continue;
		}

		/* we have more entries in the current frame, that's our next entry */
		entry = frame->entries.contents[frame->next_idx];
		frame->next_idx++;

		if ((error = filesystem_iterator_is_dir(&is_dir, iter, entry)) < 0)
			break;

		if (is_dir) {
			if (iterator__do_autoexpand(iter)) {
				error = filesystem_iterator_frame_push(iter, entry);

				/* may get GIT_ENOTFOUND due to races or permission problems
				 * that we want to quietly swallow
				 */
				if (error == GIT_ENOTFOUND)
					continue;
				else if (error < 0)
					break;
			}

			if (!iterator__include_trees(iter))
				continue;
		}

		filesystem_iterator_set_current(iter, entry);
		break;
	}

	if (out)
		*out = (error == 0) ? &iter->entry : NULL;

	return error;
}

static int filesystem_iterator_advance_into(
	const git_index_entry **out, git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);
	filesystem_iterator_frame *frame;
	filesystem_iterator_entry *prev_entry;
	int error;

	if (out)
		*out = NULL;

	if ((frame = filesystem_iterator_current_frame(iter)) == NULL)
		return GIT_ITEROVER;

	/* get the last seen entry */
	prev_entry = filesystem_iterator_current_entry(frame);

	/* it's legal to call advance_into when auto-expand is on.  in this case,
	 * we will have pushed a new (empty) frame on to the stack for this
	 * new directory.  since it's empty, its current_entry should be null.
	 */
	GIT_ASSERT(iterator__do_autoexpand(i) ^ (prev_entry != NULL));

	if (prev_entry) {
		if (prev_entry->st.st_mode != GIT_FILEMODE_COMMIT &&
			!S_ISDIR(prev_entry->st.st_mode))
			return 0;

		if ((error = filesystem_iterator_frame_push(iter, prev_entry)) < 0)
			return error;
	}

	/* we've advanced into the directory in question, let advance
	 * find the first entry
	 */
	return filesystem_iterator_advance(out, i);
}

int git_iterator_current_workdir_path(git_str **out, git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);
	const git_index_entry *entry;

	if (i->type != GIT_ITERATOR_FS &&
		i->type != GIT_ITERATOR_WORKDIR) {
		*out = NULL;
		return 0;
	}

	git_str_truncate(&iter->current_path, iter->root_len);

	if (git_iterator_current(&entry, i) < 0 ||
		git_str_puts(&iter->current_path, entry->path) < 0)
		return -1;

	*out = &iter->current_path;
	return 0;
}

GIT_INLINE(git_dir_flag) entry_dir_flag(git_index_entry *entry)
{
#if defined(GIT_WIN32) && !defined(__MINGW32__)
	return (entry && entry->mode) ?
		(S_ISDIR(entry->mode) ? GIT_DIR_FLAG_TRUE : GIT_DIR_FLAG_FALSE) :
		GIT_DIR_FLAG_UNKNOWN;
#else
	GIT_UNUSED(entry);
	return GIT_DIR_FLAG_UNKNOWN;
#endif
}

static void filesystem_iterator_update_ignored(filesystem_iterator *iter)
{
	filesystem_iterator_frame *frame;
	git_dir_flag dir_flag = entry_dir_flag(&iter->entry);

	if (git_ignore__lookup(&iter->current_is_ignored,
			&iter->ignores, iter->entry.path, dir_flag) < 0) {
		git_error_clear();
		iter->current_is_ignored = GIT_IGNORE_NOTFOUND;
	}

	/* use ignore from containing frame stack */
	if (iter->current_is_ignored <= GIT_IGNORE_NOTFOUND) {
		frame = filesystem_iterator_current_frame(iter);
		iter->current_is_ignored = frame->is_ignored;
	}
}

GIT_INLINE(bool) filesystem_iterator_current_is_ignored(
	filesystem_iterator *iter)
{
	if (iter->current_is_ignored == GIT_IGNORE_UNCHECKED)
		filesystem_iterator_update_ignored(iter);

	return (iter->current_is_ignored == GIT_IGNORE_TRUE);
}

bool git_iterator_current_is_ignored(git_iterator *i)
{
	filesystem_iterator *iter = NULL;

	if (i->type != GIT_ITERATOR_WORKDIR)
		return false;

	iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);

	return filesystem_iterator_current_is_ignored(iter);
}

bool git_iterator_current_tree_is_ignored(git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);
	filesystem_iterator_frame *frame;

	if (i->type != GIT_ITERATOR_WORKDIR)
		return false;

	frame = filesystem_iterator_current_frame(iter);
	return (frame->is_ignored == GIT_IGNORE_TRUE);
}

static int filesystem_iterator_advance_over(
	const git_index_entry **out,
	git_iterator_status_t *status,
	git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);
	filesystem_iterator_frame *current_frame;
	filesystem_iterator_entry *current_entry;
	const git_index_entry *entry = NULL;
	const char *base;
	int error = 0;

	*out = NULL;
	*status = GIT_ITERATOR_STATUS_NORMAL;

	GIT_ASSERT(iterator__has_been_accessed(i));

	current_frame = filesystem_iterator_current_frame(iter);
	GIT_ASSERT(current_frame);

	current_entry = filesystem_iterator_current_entry(current_frame);
	GIT_ASSERT(current_entry);

	if ((error = git_iterator_current(&entry, i)) < 0)
		return error;

	if (!S_ISDIR(entry->mode)) {
		if (filesystem_iterator_current_is_ignored(iter))
			*status = GIT_ITERATOR_STATUS_IGNORED;

		return filesystem_iterator_advance(out, i);
	}

	git_str_clear(&iter->tmp_buf);
	if ((error = git_str_puts(&iter->tmp_buf, entry->path)) < 0)
		return error;

	base = iter->tmp_buf.ptr;

	/* scan inside the directory looking for files.  if we find nothing,
	 * we will remain EMPTY.  if we find any ignored item, upgrade EMPTY to
	 * IGNORED.  if we find a real actual item, upgrade all the way to NORMAL
	 * and then stop.
	 *
	 * however, if we're here looking for a pathlist item (but are not
	 * actually in the pathlist ourselves) then start at FILTERED instead of
	 * EMPTY.  callers then know that this path was not something they asked
	 * about.
	 */
	*status = current_entry->match == ITERATOR_PATHLIST_IS_PARENT ?
		GIT_ITERATOR_STATUS_FILTERED : GIT_ITERATOR_STATUS_EMPTY;

	while (entry && !iter->base.prefixcomp(entry->path, base)) {
		if (filesystem_iterator_current_is_ignored(iter)) {
			/* if we found an explicitly ignored item, then update from
			 * EMPTY to IGNORED
			 */
			*status = GIT_ITERATOR_STATUS_IGNORED;
		} else if (S_ISDIR(entry->mode)) {
			error = filesystem_iterator_advance_into(&entry, i);

			if (!error)
				continue;

			/* this directory disappeared, ignore it */
			else if (error == GIT_ENOTFOUND)
				error = 0;

			/* a real error occurred */
			else
				break;
		} else {
			/* we found a non-ignored item, treat parent as untracked */
			*status = GIT_ITERATOR_STATUS_NORMAL;
			break;
		}

		if ((error = git_iterator_advance(&entry, i)) < 0)
			break;
	}

	/* wrap up scan back to base directory */
	while (entry && !iter->base.prefixcomp(entry->path, base)) {
		if ((error = git_iterator_advance(&entry, i)) < 0)
			break;
	}

	if (!error)
		*out = entry;

	return error;
}

static void filesystem_iterator_clear(filesystem_iterator *iter)
{
	while (iter->frames.size)
		filesystem_iterator_frame_pop(iter);

	git_array_clear(iter->frames);
	git_ignore__free(&iter->ignores);

	git_str_dispose(&iter->tmp_buf);

	iterator_clear(&iter->base);
}

static int filesystem_iterator_init(filesystem_iterator *iter)
{
	int error;

	if (iterator__honor_ignores(&iter->base) &&
		(error = git_ignore__for_path(iter->base.repo,
			".gitignore", &iter->ignores)) < 0)
		return error;

	if ((error = filesystem_iterator_frame_push(iter, NULL)) < 0)
		return error;

	iter->base.flags &= ~GIT_ITERATOR_FIRST_ACCESS;

	return 0;
}

static int filesystem_iterator_reset(git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);

	filesystem_iterator_clear(iter);
	return filesystem_iterator_init(iter);
}

static void filesystem_iterator_free(git_iterator *i)
{
	filesystem_iterator *iter = GIT_CONTAINER_OF(i, filesystem_iterator, base);
	git__free(iter->root);
	git_str_dispose(&iter->current_path);
	git_tree_free(iter->tree);
	if (iter->index)
		git_index_snapshot_release(&iter->index_snapshot, iter->index);
	filesystem_iterator_clear(iter);
}

static int iterator_for_filesystem(
	git_iterator **out,
	git_repository *repo,
	const char *root,
	git_index *index,
	git_tree *tree,
	git_iterator_t type,
	git_iterator_options *options)
{
	filesystem_iterator *iter;
	size_t root_len;
	int error;

	static git_iterator_callbacks callbacks = {
		filesystem_iterator_current,
		filesystem_iterator_advance,
		filesystem_iterator_advance_into,
		filesystem_iterator_advance_over,
		filesystem_iterator_reset,
		filesystem_iterator_free
	};

	*out = NULL;

	if (root == NULL)
		return git_iterator_for_nothing(out, options);

	iter = git__calloc(1, sizeof(filesystem_iterator));
	GIT_ERROR_CHECK_ALLOC(iter);

	iter->base.type = type;
	iter->base.cb = &callbacks;

	root_len = strlen(root);

	iter->root = git__malloc(root_len+2);
	GIT_ERROR_CHECK_ALLOC(iter->root);

	memcpy(iter->root, root, root_len);

	if (root_len == 0 || root[root_len-1] != '/') {
		iter->root[root_len] = '/';
		root_len++;
	}
	iter->root[root_len] = '\0';
	iter->root_len = root_len;

	if ((error = git_str_puts(&iter->current_path, iter->root)) < 0)
		goto on_error;

	if ((error = iterator_init_common(&iter->base, repo, index, options)) < 0)
		goto on_error;

	if (tree && (error = git_tree_dup(&iter->tree, tree)) < 0)
		goto on_error;

	if (index &&
		(error = git_index_snapshot_new(&iter->index_snapshot, index)) < 0)
		goto on_error;

	iter->index = index;
	iter->dirload_flags =
		(iterator__ignore_case(&iter->base) ?
			GIT_FS_PATH_DIR_IGNORE_CASE : 0) |
		(iterator__flag(&iter->base, PRECOMPOSE_UNICODE) ?
			GIT_FS_PATH_DIR_PRECOMPOSE_UNICODE : 0);

	iter->oid_type = options->oid_type;

	if ((error = filesystem_iterator_init(iter)) < 0)
		goto on_error;

	*out = &iter->base;
	return 0;

on_error:
	git_iterator_free(&iter->base);
	return error;
}

int git_iterator_for_filesystem(
	git_iterator **out,
	const char *root,
	git_iterator_options *given_opts)
{
	git_iterator_options options = GIT_ITERATOR_OPTIONS_INIT;

	if (given_opts)
		memcpy(&options, given_opts, sizeof(git_iterator_options));

	return iterator_for_filesystem(out,
		NULL, root, NULL, NULL, GIT_ITERATOR_FS, &options);
}

int git_iterator_for_workdir_ext(
	git_iterator **out,
	git_repository *repo,
	const char *repo_workdir,
	git_index *index,
	git_tree *tree,
	git_iterator_options *given_opts)
{
	git_iterator_options options = GIT_ITERATOR_OPTIONS_INIT;

	if (!repo_workdir) {
		if (git_repository__ensure_not_bare(repo, "scan working directory") < 0)
			return GIT_EBAREREPO;

		repo_workdir = git_repository_workdir(repo);
	}

	/* upgrade to a workdir iterator, adding necessary internal flags */
	if (given_opts)
		memcpy(&options, given_opts, sizeof(git_iterator_options));

	options.flags |= GIT_ITERATOR_HONOR_IGNORES |
		GIT_ITERATOR_IGNORE_DOT_GIT;

	if (!options.oid_type)
		options.oid_type = repo->oid_type;
	else if (options.oid_type != repo->oid_type)
		git_error_set(GIT_ERROR_INVALID,
			"specified object ID type does not match repository object ID type");

	return iterator_for_filesystem(out,
		repo, repo_workdir, index, tree, GIT_ITERATOR_WORKDIR, &options);
}


/* Index iterator */


typedef struct {
	git_iterator base;
	git_vector entries;
	size_t next_idx;

	/* the pseudotree entry */
	git_index_entry tree_entry;
	git_str tree_buf;
	bool skip_tree;

	const git_index_entry *entry;
} index_iterator;

static int index_iterator_current(
	const git_index_entry **out, git_iterator *i)
{
	index_iterator *iter = (index_iterator *)i;

	if (!iterator__has_been_accessed(i))
		return iter->base.cb->advance(out, i);

	if (iter->entry == NULL) {
		*out = NULL;
		return GIT_ITEROVER;
	}

	*out = iter->entry;
	return 0;
}

static bool index_iterator_create_pseudotree(
	const git_index_entry **out,
	index_iterator *iter,
	const char *path)
{
	const char *prev_path, *relative_path, *dirsep;
	size_t common_len;

	prev_path = iter->entry ? iter->entry->path : "";

	/* determine if the new path is in a different directory from the old */
	common_len = git_fs_path_common_dirlen(prev_path, path);
	relative_path = path + common_len;

	if ((dirsep = strchr(relative_path, '/')) == NULL)
		return false;

	git_str_clear(&iter->tree_buf);
	git_str_put(&iter->tree_buf, path, (dirsep - path) + 1);

	iter->tree_entry.mode = GIT_FILEMODE_TREE;
	iter->tree_entry.path = iter->tree_buf.ptr;

	*out = &iter->tree_entry;
	return true;
}

static int index_iterator_skip_pseudotree(index_iterator *iter)
{
	GIT_ASSERT(iterator__has_been_accessed(&iter->base));
	GIT_ASSERT(S_ISDIR(iter->entry->mode));

	while (true) {
		const git_index_entry *next_entry = NULL;

		if (++iter->next_idx >= iter->entries.length)
			return GIT_ITEROVER;

		next_entry = iter->entries.contents[iter->next_idx];

		if (iter->base.strncomp(iter->tree_buf.ptr, next_entry->path,
			iter->tree_buf.size) != 0)
			break;
	}

	iter->skip_tree = false;
	return 0;
}

static int index_iterator_advance(
	const git_index_entry **out, git_iterator *i)
{
	index_iterator *iter = GIT_CONTAINER_OF(i, index_iterator, base);
	const git_index_entry *entry = NULL;
	bool is_submodule;
	int error = 0;

	iter->base.flags |= GIT_ITERATOR_FIRST_ACCESS;

	while (true) {
		if (iter->next_idx >= iter->entries.length) {
			error = GIT_ITEROVER;
			break;
		}

		/* we were not asked to expand this pseudotree.  advance over it. */
		if (iter->skip_tree) {
			index_iterator_skip_pseudotree(iter);
			continue;
		}

		entry = iter->entries.contents[iter->next_idx];
		is_submodule = S_ISGITLINK(entry->mode);

		if (!iterator_has_started(&iter->base, entry->path, is_submodule)) {
			iter->next_idx++;
			continue;
		}

		if (iterator_has_ended(&iter->base, entry->path)) {
			error = GIT_ITEROVER;
			break;
		}

		/* if we have a list of paths we're interested in, examine it */
		if (!iterator_pathlist_next_is(&iter->base, entry->path)) {
			iter->next_idx++;
			continue;
		}

		/* if this is a conflict, skip it unless we're including conflicts */
		if (git_index_entry_is_conflict(entry) &&
			!iterator__include_conflicts(&iter->base)) {
			iter->next_idx++;
			continue;
		}

		/* we've found what will be our next _file_ entry.  but if we are
		 * returning trees entries, we may need to return a pseudotree
		 * entry that will contain this.  don't advance over this entry,
		 * though, we still need to return it on the next `advance`.
		 */
		if (iterator__include_trees(&iter->base) &&
			index_iterator_create_pseudotree(&entry, iter, entry->path)) {

			/* Note whether this pseudo tree should be expanded or not */
			iter->skip_tree = iterator__dont_autoexpand(&iter->base);
			break;
		}

		iter->next_idx++;
		break;
	}

	iter->entry = (error == 0) ? entry : NULL;

	if (out)
		*out = iter->entry;

	return error;
}

static int index_iterator_advance_into(
	const git_index_entry **out, git_iterator *i)
{
	index_iterator *iter = GIT_CONTAINER_OF(i, index_iterator, base);

	if (! S_ISDIR(iter->tree_entry.mode)) {
		if (out)
			*out = NULL;

		return 0;
	}

	iter->skip_tree = false;
	return index_iterator_advance(out, i);
}

static int index_iterator_advance_over(
	const git_index_entry **out,
	git_iterator_status_t *status,
	git_iterator *i)
{
	index_iterator *iter = GIT_CONTAINER_OF(i, index_iterator, base);
	const git_index_entry *entry;
	int error;

	if ((error = index_iterator_current(&entry, i)) < 0)
		return error;

	if (S_ISDIR(entry->mode))
		index_iterator_skip_pseudotree(iter);

	*status = GIT_ITERATOR_STATUS_NORMAL;
	return index_iterator_advance(out, i);
}

static void index_iterator_clear(index_iterator *iter)
{
	iterator_clear(&iter->base);
}

static int index_iterator_init(index_iterator *iter)
{
	iter->base.flags &= ~GIT_ITERATOR_FIRST_ACCESS;
	iter->next_idx = 0;
	iter->skip_tree = false;
	return 0;
}

static int index_iterator_reset(git_iterator *i)
{
	index_iterator *iter = GIT_CONTAINER_OF(i, index_iterator, base);

	index_iterator_clear(iter);
	return index_iterator_init(iter);
}

static void index_iterator_free(git_iterator *i)
{
	index_iterator *iter = GIT_CONTAINER_OF(i, index_iterator, base);

	git_index_snapshot_release(&iter->entries, iter->base.index);
	git_str_dispose(&iter->tree_buf);
}

int git_iterator_for_index(
	git_iterator **out,
	git_repository *repo,
	git_index  *index,
	git_iterator_options *options)
{
	index_iterator *iter;
	int error;

	static git_iterator_callbacks callbacks = {
		index_iterator_current,
		index_iterator_advance,
		index_iterator_advance_into,
		index_iterator_advance_over,
		index_iterator_reset,
		index_iterator_free
	};

	*out = NULL;

	if (index == NULL)
		return git_iterator_for_nothing(out, options);

	iter = git__calloc(1, sizeof(index_iterator));
	GIT_ERROR_CHECK_ALLOC(iter);

	iter->base.type = GIT_ITERATOR_INDEX;
	iter->base.cb = &callbacks;

	if ((error = iterator_init_common(&iter->base, repo, index, options)) < 0 ||
		(error = git_index_snapshot_new(&iter->entries, index)) < 0 ||
		(error = index_iterator_init(iter)) < 0)
		goto on_error;

	git_vector_set_cmp(&iter->entries, iterator__ignore_case(&iter->base) ?
		git_index_entry_icmp : git_index_entry_cmp);
	git_vector_sort(&iter->entries);

	*out = &iter->base;
	return 0;

on_error:
	git_iterator_free(&iter->base);
	return error;
}


/* Iterator API */

int git_iterator_reset_range(
	git_iterator *i, const char *start, const char *end)
{
	if (iterator_reset_range(i, start, end) < 0)
		return -1;

	return i->cb->reset(i);
}

int git_iterator_set_ignore_case(git_iterator *i, bool ignore_case)
{
	GIT_ASSERT(!iterator__has_been_accessed(i));
	iterator_set_ignore_case(i, ignore_case);
	return 0;
}

void git_iterator_free(git_iterator *iter)
{
	if (iter == NULL)
		return;

	iter->cb->free(iter);

	git_vector_dispose(&iter->pathlist);
	git__free(iter->start);
	git__free(iter->end);

	memset(iter, 0, sizeof(*iter));

	git__free(iter);
}

int git_iterator_foreach(
	git_iterator *iterator,
	git_iterator_foreach_cb cb,
	void *data)
{
	const git_index_entry *iterator_item;
	int error = 0;

	if ((error = git_iterator_current(&iterator_item, iterator)) < 0)
		goto done;

	if ((error = cb(iterator_item, data)) != 0)
		goto done;

	while (true) {
		if ((error = git_iterator_advance(&iterator_item, iterator)) < 0)
			goto done;

		if ((error = cb(iterator_item, data)) != 0)
			goto done;
	}

done:
	if (error == GIT_ITEROVER)
		error = 0;

	return error;
}

int git_iterator_walk(
	git_iterator **iterators,
	size_t cnt,
	git_iterator_walk_cb cb,
	void *data)
{
	const git_index_entry **iterator_item;	/* next in each iterator */
	const git_index_entry **cur_items;		/* current path in each iter */
	const git_index_entry *first_match;
	size_t i, j;
	int error = 0;

	iterator_item = git__calloc(cnt, sizeof(git_index_entry *));
	cur_items = git__calloc(cnt, sizeof(git_index_entry *));

	GIT_ERROR_CHECK_ALLOC(iterator_item);
	GIT_ERROR_CHECK_ALLOC(cur_items);

	/* Set up the iterators */
	for (i = 0; i < cnt; i++) {
		error = git_iterator_current(&iterator_item[i], iterators[i]);

		if (error < 0 && error != GIT_ITEROVER)
			goto done;
	}

	while (true) {
		for (i = 0; i < cnt; i++)
			cur_items[i] = NULL;

		first_match = NULL;

		/* Find the next path(s) to consume from each iterator */
		for (i = 0; i < cnt; i++) {
			if (iterator_item[i] == NULL)
				continue;

			if (first_match == NULL) {
				first_match = iterator_item[i];
				cur_items[i] = iterator_item[i];
			} else {
				int path_diff = git_index_entry_cmp(iterator_item[i], first_match);

				if (path_diff < 0) {
					/* Found an index entry that sorts before the one we're
					 * looking at.  Forget that we've seen the other and
					 * look at the other iterators for this path.
					 */
					for (j = 0; j < i; j++)
						cur_items[j] = NULL;

					first_match = iterator_item[i];
					cur_items[i] = iterator_item[i];
				} else if (path_diff == 0) {
					cur_items[i] = iterator_item[i];
				}
			}
		}

		if (first_match == NULL)
			break;

		if ((error = cb(cur_items, data)) != 0)
			goto done;

		/* Advance each iterator that participated */
		for (i = 0; i < cnt; i++) {
			if (cur_items[i] == NULL)
				continue;

			error = git_iterator_advance(&iterator_item[i], iterators[i]);

			if (error < 0 && error != GIT_ITEROVER)
				goto done;
		}
	}

done:
	git__free((git_index_entry **)iterator_item);
	git__free((git_index_entry **)cur_items);

	if (error == GIT_ITEROVER)
		error = 0;

	return error;
}
