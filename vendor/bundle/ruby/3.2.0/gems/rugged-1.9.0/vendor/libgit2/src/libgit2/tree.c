/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "tree.h"

#include "commit.h"
#include "git2/repository.h"
#include "git2/object.h"
#include "futils.h"
#include "tree-cache.h"
#include "index.h"
#include "path.h"

#define DEFAULT_TREE_SIZE 16
#define MAX_FILEMODE_BYTES 6

#define TREE_ENTRY_CHECK_NAMELEN(n) \
	if (n > UINT16_MAX) { git_error_set(GIT_ERROR_INVALID, "tree entry path too long"); }

GIT_HASHMAP_STR_FUNCTIONS(git_treebuilder_entrymap, GIT_HASHMAP_INLINE, git_tree_entry *);

static bool valid_filemode(const int filemode)
{
	return (filemode == GIT_FILEMODE_TREE
		|| filemode == GIT_FILEMODE_BLOB
		|| filemode == GIT_FILEMODE_BLOB_EXECUTABLE
		|| filemode == GIT_FILEMODE_LINK
		|| filemode == GIT_FILEMODE_COMMIT);
}

GIT_INLINE(git_filemode_t) normalize_filemode(git_filemode_t filemode)
{
	/* Tree bits set, but it's not a commit */
	if (GIT_MODE_TYPE(filemode) == GIT_FILEMODE_TREE)
		return GIT_FILEMODE_TREE;

	/* If any of the x bits are set */
	if (GIT_PERMS_IS_EXEC(filemode))
		return GIT_FILEMODE_BLOB_EXECUTABLE;

	/* 16XXXX means commit */
	if (GIT_MODE_TYPE(filemode) == GIT_FILEMODE_COMMIT)
		return GIT_FILEMODE_COMMIT;

	/* 12XXXX means symlink */
	if (GIT_MODE_TYPE(filemode) == GIT_FILEMODE_LINK)
		return GIT_FILEMODE_LINK;

	/* Otherwise, return a blob */
	return GIT_FILEMODE_BLOB;
}

static int valid_entry_name(git_repository *repo, const char *filename)
{
	return *filename != '\0' &&
		git_path_is_valid(repo, filename, 0,
		GIT_FS_PATH_REJECT_TRAVERSAL | GIT_PATH_REJECT_DOT_GIT | GIT_FS_PATH_REJECT_SLASH);
}

static int entry_sort_cmp(const void *a, const void *b)
{
	const git_tree_entry *e1 = (const git_tree_entry *)a;
	const git_tree_entry *e2 = (const git_tree_entry *)b;

	return git_fs_path_cmp(
		e1->filename, e1->filename_len, git_tree_entry__is_tree(e1),
		e2->filename, e2->filename_len, git_tree_entry__is_tree(e2),
		git__strncmp);
}

int git_tree_entry_cmp(const git_tree_entry *e1, const git_tree_entry *e2)
{
	return entry_sort_cmp(e1, e2);
}

/**
 * Allocate a new self-contained entry, with enough space after it to
 * store the filename and the id.
 */
static git_tree_entry *alloc_entry(const char *filename, size_t filename_len, const git_oid *id)
{
	git_tree_entry *entry = NULL;
	char *filename_ptr;
	size_t tree_len;

#ifdef GIT_EXPERIMENTAL_SHA256
	size_t oid_size = git_oid_size(id->type);
#else
	size_t oid_size = GIT_OID_SHA1_SIZE;
#endif

	TREE_ENTRY_CHECK_NAMELEN(filename_len);

	if (GIT_ADD_SIZET_OVERFLOW(&tree_len, sizeof(git_tree_entry), filename_len) ||
	    GIT_ADD_SIZET_OVERFLOW(&tree_len, tree_len, 1) ||
	    GIT_ADD_SIZET_OVERFLOW(&tree_len, tree_len, oid_size))
		return NULL;

	entry = git__calloc(1, tree_len);
	if (!entry)
		return NULL;

	filename_ptr = ((char *) entry) + sizeof(git_tree_entry);
	memcpy(filename_ptr, filename, filename_len);
	entry->filename = filename_ptr;
	entry->filename_len = (uint16_t)filename_len;

	git_oid_cpy(&entry->oid, id);

	return entry;
}

struct tree_key_search {
	const char *filename;
	uint16_t filename_len;
};

static int homing_search_cmp(const void *key, const void *array_member)
{
	const struct tree_key_search *ksearch = key;
	const git_tree_entry *entry = array_member;

	const uint16_t len1 = ksearch->filename_len;
	const uint16_t len2 = entry->filename_len;

	return memcmp(
		ksearch->filename,
		entry->filename,
		len1 < len2 ? len1 : len2
	);
}

/*
 * Search for an entry in a given tree.
 *
 * Note that this search is performed in two steps because
 * of the way tree entries are sorted internally in git:
 *
 * Entries in a tree are not sorted alphabetically; two entries
 * with the same root prefix will have different positions
 * depending on whether they are folders (subtrees) or normal files.
 *
 * Consequently, it is not possible to find an entry on the tree
 * with a binary search if you don't know whether the filename
 * you're looking for is a folder or a normal file.
 *
 * To work around this, we first perform a homing binary search
 * on the tree, using the minimal length root prefix of our filename.
 * Once the comparisons for this homing search start becoming
 * ambiguous because of folder vs file sorting, we look linearly
 * around the area for our target file.
 */
static int tree_key_search(
	size_t *at_pos,
	const git_tree *tree,
	const char *filename,
	size_t filename_len)
{
	struct tree_key_search ksearch;
	const git_tree_entry *entry;
	size_t homing, i;

	TREE_ENTRY_CHECK_NAMELEN(filename_len);

	ksearch.filename = filename;
	ksearch.filename_len = (uint16_t)filename_len;

	/* Initial homing search; find an entry on the tree with
	 * the same prefix as the filename we're looking for */

	if (git_array_search(&homing,
		tree->entries, &homing_search_cmp, &ksearch) < 0)
		return GIT_ENOTFOUND; /* just a signal error; not passed back to user */

	/* We found a common prefix. Look forward as long as
	 * there are entries that share the common prefix */
	for (i = homing; i < tree->entries.size; ++i) {
		entry = git_array_get(tree->entries, i);

		if (homing_search_cmp(&ksearch, entry) < 0)
			break;

		if (entry->filename_len == filename_len &&
			memcmp(filename, entry->filename, filename_len) == 0) {
			if (at_pos)
				*at_pos = i;

			return 0;
		}
	}

	/* If we haven't found our filename yet, look backwards
	 * too as long as we have entries with the same prefix */
	if (homing > 0) {
		i = homing - 1;

		do {
			entry = git_array_get(tree->entries, i);

			if (homing_search_cmp(&ksearch, entry) > 0)
				break;

			if (entry->filename_len == filename_len &&
				memcmp(filename, entry->filename, filename_len) == 0) {
				if (at_pos)
					*at_pos = i;

				return 0;
			}
		} while (i-- > 0);
	}

	/* The filename doesn't exist at all */
	return GIT_ENOTFOUND;
}

void git_tree_entry_free(git_tree_entry *entry)
{
	if (entry == NULL)
		return;

	git__free(entry);
}

int git_tree_entry_dup(git_tree_entry **dest, const git_tree_entry *source)
{
	git_tree_entry *cpy;

	GIT_ASSERT_ARG(source);

	cpy = alloc_entry(source->filename, source->filename_len, &source->oid);
	if (cpy == NULL)
		return -1;

	cpy->attr = source->attr;

	*dest = cpy;
	return 0;
}

void git_tree__free(void *_tree)
{
	git_tree *tree = _tree;

	git_odb_object_free(tree->odb_obj);
	git_array_clear(tree->entries);
	git__free(tree);
}

git_filemode_t git_tree_entry_filemode(const git_tree_entry *entry)
{
	return normalize_filemode(entry->attr);
}

git_filemode_t git_tree_entry_filemode_raw(const git_tree_entry *entry)
{
	return entry->attr;
}

const char *git_tree_entry_name(const git_tree_entry *entry)
{
	GIT_ASSERT_ARG_WITH_RETVAL(entry, NULL);
	return entry->filename;
}

const git_oid *git_tree_entry_id(const git_tree_entry *entry)
{
	GIT_ASSERT_ARG_WITH_RETVAL(entry, NULL);
	return &entry->oid;
}

git_object_t git_tree_entry_type(const git_tree_entry *entry)
{
	GIT_ASSERT_ARG_WITH_RETVAL(entry, GIT_OBJECT_INVALID);

	if (S_ISGITLINK(entry->attr))
		return GIT_OBJECT_COMMIT;
	else if (S_ISDIR(entry->attr))
		return GIT_OBJECT_TREE;
	else
		return GIT_OBJECT_BLOB;
}

int git_tree_entry_to_object(
	git_object **object_out,
	git_repository *repo,
	const git_tree_entry *entry)
{
	GIT_ASSERT_ARG(entry);
	GIT_ASSERT_ARG(object_out);

	return git_object_lookup(object_out, repo, &entry->oid, GIT_OBJECT_ANY);
}

static const git_tree_entry *entry_fromname(
	const git_tree *tree, const char *name, size_t name_len)
{
	size_t idx;

	if (tree_key_search(&idx, tree, name, name_len) < 0)
		return NULL;

	return git_array_get(tree->entries, idx);
}

const git_tree_entry *git_tree_entry_byname(
	const git_tree *tree, const char *filename)
{
	GIT_ASSERT_ARG_WITH_RETVAL(tree, NULL);
	GIT_ASSERT_ARG_WITH_RETVAL(filename, NULL);

	return entry_fromname(tree, filename, strlen(filename));
}

const git_tree_entry *git_tree_entry_byindex(
	const git_tree *tree, size_t idx)
{
	GIT_ASSERT_ARG_WITH_RETVAL(tree, NULL);
	return git_array_get(tree->entries, idx);
}

const git_tree_entry *git_tree_entry_byid(
	const git_tree *tree, const git_oid *id)
{
	size_t i;
	const git_tree_entry *e;

	GIT_ASSERT_ARG_WITH_RETVAL(tree, NULL);

	git_array_foreach(tree->entries, i, e) {
		if (git_oid_equal(&e->oid, id))
			return e;
	}

	return NULL;
}

size_t git_tree_entrycount(const git_tree *tree)
{
	GIT_ASSERT_ARG_WITH_RETVAL(tree, 0);
	return tree->entries.size;
}

size_t git_treebuilder_entrycount(git_treebuilder *bld)
{
	GIT_ASSERT_ARG_WITH_RETVAL(bld, 0);

	return git_treebuilder_entrymap_size(&bld->map);
}

GIT_INLINE(void) set_error(const char *str, const char *path)
{
	if (path)
		git_error_set(GIT_ERROR_TREE, "%s - %s", str, path);
	else
		git_error_set(GIT_ERROR_TREE, "%s", str);
}

static int tree_error(const char *str, const char *path)
{
	set_error(str, path);
	return -1;
}

static int tree_parse_error(const char *str, const char *path)
{
	set_error(str, path);
	return GIT_EINVALID;
}

static int parse_mode(uint16_t *mode_out, const char *buffer, size_t buffer_len, const char **buffer_out)
{
	int32_t mode;
	int error;

	if (!buffer_len || git__isspace(*buffer))
		return -1;

	if ((error = git__strntol32(&mode, buffer, buffer_len, buffer_out, 8)) < 0)
		return error;

	if (mode < 0 || (uint32_t)mode > UINT16_MAX)
		return -1;

	*mode_out = mode;

	return 0;
}

int git_tree__parse_raw(void *_tree, const char *data, size_t size, git_oid_t oid_type)
{
	git_tree *tree = _tree;
	const char *buffer;
	const char *buffer_end;
	const long oid_size = (long)git_oid_size(oid_type);

	buffer = data;
	buffer_end = buffer + size;

	tree->odb_obj = NULL;
	git_array_init_to_size(tree->entries, DEFAULT_TREE_SIZE);
	GIT_ERROR_CHECK_ARRAY(tree->entries);

	while (buffer < buffer_end) {
		git_tree_entry *entry;
		size_t filename_len;
		const char *nul;
		uint16_t attr;

		if (parse_mode(&attr, buffer, buffer_end - buffer, &buffer) < 0 || !buffer)
			return tree_parse_error("failed to parse tree: can't parse filemode", NULL);

		if (buffer >= buffer_end || (*buffer++) != ' ')
			return tree_parse_error("failed to parse tree: missing space after filemode", NULL);

		if ((nul = memchr(buffer, 0, buffer_end - buffer)) == NULL)
			return tree_parse_error("failed to parse tree: object is corrupted", NULL);

		if ((filename_len = nul - buffer) == 0 || filename_len > UINT16_MAX)
			return tree_parse_error("failed to parse tree: can't parse filename", NULL);

		if ((buffer_end - (nul + 1)) < (long)oid_size)
			return tree_parse_error("failed to parse tree: can't parse OID", NULL);

		/* Allocate the entry */
		entry = git_array_alloc(tree->entries);
		GIT_ERROR_CHECK_ALLOC(entry);

		entry->attr = attr;
		entry->filename_len = (uint16_t)filename_len;
		entry->filename = buffer;
		buffer += filename_len + 1;

		git_oid__fromraw(&entry->oid, (unsigned char *)buffer, oid_type);
		buffer += oid_size;
	}

	return 0;
}

int git_tree__parse(void *_tree, git_odb_object *odb_obj, git_oid_t oid_type)
{
	git_tree *tree = _tree;
	const char *data = git_odb_object_data(odb_obj);
	size_t size = git_odb_object_size(odb_obj);
	int error;

	if ((error = git_tree__parse_raw(tree, data, size, oid_type)) < 0 ||
	    (error = git_odb_object_dup(&tree->odb_obj, odb_obj)) < 0)
		return error;

	return error;
}

static size_t find_next_dir(const char *dirname, git_index *index, size_t start)
{
	size_t dirlen, i, entries = git_index_entrycount(index);

	dirlen = strlen(dirname);
	for (i = start; i < entries; ++i) {
		const git_index_entry *entry = git_index_get_byindex(index, i);
		if (strlen(entry->path) < dirlen ||
		    memcmp(entry->path, dirname, dirlen) ||
			(dirlen > 0 && entry->path[dirlen] != '/')) {
			break;
		}
	}

	return i;
}

static git_object_t otype_from_mode(git_filemode_t filemode)
{
	switch (filemode) {
	case GIT_FILEMODE_TREE:
		return GIT_OBJECT_TREE;
	case GIT_FILEMODE_COMMIT:
		return GIT_OBJECT_COMMIT;
	default:
		return GIT_OBJECT_BLOB;
	}
}

static int check_entry(git_repository *repo, const char *filename, const git_oid *id, git_filemode_t filemode)
{
	if (!valid_filemode(filemode))
		return tree_error("failed to insert entry: invalid filemode for file", filename);

	if (!valid_entry_name(repo, filename))
		return tree_error("failed to insert entry: invalid name for a tree entry", filename);

	if (git_oid_is_zero(id))
		return tree_error("failed to insert entry: invalid null OID", filename);

	if (filemode != GIT_FILEMODE_COMMIT &&
	    !git_object__is_valid(repo, id, otype_from_mode(filemode)))
		return tree_error("failed to insert entry: invalid object specified", filename);

	return 0;
}

static int git_treebuilder__write_with_buffer(
	git_oid *oid,
	git_treebuilder *bld,
	git_str *buf)
{
	int error = 0;
	size_t i, entrycount;
	git_odb *odb;
	git_tree_entry *entry;
	git_vector entries = GIT_VECTOR_INIT;
	size_t oid_size = git_oid_size(bld->repo->oid_type);
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;

	git_str_clear(buf);

	entrycount = git_treebuilder_entrymap_size(&bld->map);

	if ((error = git_vector_init(&entries, entrycount, entry_sort_cmp)) < 0)
		goto out;

	if (buf->asize == 0 &&
	    (error = git_str_grow(buf, entrycount * 72)) < 0)
		goto out;

	while (git_treebuilder_entrymap_iterate(&iter, NULL, &entry, &bld->map) == 0) {
		if ((error = git_vector_insert(&entries, entry)) < 0)
			goto out;
	}

	git_vector_sort(&entries);

	for (i = 0; i < entries.length && !error; ++i) {
		entry = git_vector_get(&entries, i);

		git_str_printf(buf, "%o ", entry->attr);
		git_str_put(buf, entry->filename, entry->filename_len + 1);
		git_str_put(buf, (char *)entry->oid.id, oid_size);

		if (git_str_oom(buf)) {
			error = -1;
			goto out;
		}
	}

	if ((error = git_repository_odb__weakptr(&odb, bld->repo)) == 0)
		error = git_odb_write(oid, odb, buf->ptr, buf->size, GIT_OBJECT_TREE);

out:
	git_vector_dispose(&entries);

	return error;
}

static int append_entry(
	git_treebuilder *bld,
	const char *filename,
	const git_oid *id,
	git_filemode_t filemode,
	bool validate)
{
	git_tree_entry *entry;
	int error = 0;

	if (validate && ((error = check_entry(bld->repo, filename, id, filemode)) < 0))
		return error;

	entry = alloc_entry(filename, strlen(filename), id);
	GIT_ERROR_CHECK_ALLOC(entry);

	entry->attr = (uint16_t)filemode;

	if ((error = git_treebuilder_entrymap_put(&bld->map, entry->filename, entry)) < 0) {
		git_tree_entry_free(entry);
		git_error_set(GIT_ERROR_TREE, "failed to append entry %s to the tree builder", filename);
		return -1;
	}

	return 0;
}

static int write_tree(
	git_oid *oid,
	git_repository *repo,
	git_index *index,
	const char *dirname,
	size_t start,
	git_str *shared_buf)
{
	git_treebuilder *bld = NULL;
	size_t i, entries = git_index_entrycount(index);
	int error;
	size_t dirname_len = strlen(dirname);
	const git_tree_cache *cache;

	cache = git_tree_cache_get(index->tree, dirname);
	if (cache != NULL && cache->entry_count >= 0){
		git_oid_cpy(oid, &cache->oid);
		return (int)find_next_dir(dirname, index, start);
	}

	if ((error = git_treebuilder_new(&bld, repo, NULL)) < 0 || bld == NULL)
		return -1;

	/*
	 * This loop is unfortunate, but necessary. The index doesn't have
	 * any directories, so we need to handle that manually, and we
	 * need to keep track of the current position.
	 */
	for (i = start; i < entries; ++i) {
		const git_index_entry *entry = git_index_get_byindex(index, i);
		const char *filename, *next_slash;

	/*
	 * If we've left our (sub)tree, exit the loop and return. The
	 * first check is an early out (and security for the
	 * third). The second check is a simple prefix comparison. The
	 * third check catches situations where there is a directory
	 * win32/sys and a file win32mmap.c. Without it, the following
	 * code believes there is a file win32/mmap.c
	 */
		if (strlen(entry->path) < dirname_len ||
		    memcmp(entry->path, dirname, dirname_len) ||
		    (dirname_len > 0 && entry->path[dirname_len] != '/')) {
			break;
		}

		filename = entry->path + dirname_len;
		if (*filename == '/')
			filename++;
		next_slash = strchr(filename, '/');
		if (next_slash) {
			git_oid sub_oid;
			int written;
			char *subdir, *last_comp;

			subdir = git__strndup(entry->path, next_slash - entry->path);
			GIT_ERROR_CHECK_ALLOC(subdir);

			/* Write out the subtree */
			written = write_tree(&sub_oid, repo, index, subdir, i, shared_buf);
			if (written < 0) {
				git__free(subdir);
				goto on_error;
			} else {
				i = written - 1; /* -1 because of the loop increment */
			}

			/*
			 * We need to figure out what we want toinsert
			 * into this tree. If we're traversing
			 * deps/zlib/, then we only want to write
			 * 'zlib' into the tree.
			 */
			last_comp = strrchr(subdir, '/');
			if (last_comp) {
				last_comp++; /* Get rid of the '/' */
			} else {
				last_comp = subdir;
			}

			error = append_entry(bld, last_comp, &sub_oid, S_IFDIR, true);
			git__free(subdir);
			if (error < 0)
				goto on_error;
		} else {
			error = append_entry(bld, filename, &entry->id, entry->mode, true);
			if (error < 0)
				goto on_error;
		}
	}

	if (git_treebuilder__write_with_buffer(oid, bld, shared_buf) < 0)
		goto on_error;

	git_treebuilder_free(bld);
	return (int)i;

on_error:
	git_treebuilder_free(bld);
	return -1;
}

int git_tree__write_index(
	git_oid *oid, git_index *index, git_repository *repo)
{
	int ret;
	git_tree *tree;
	git_str shared_buf = GIT_STR_INIT;
	bool old_ignore_case = false;

	GIT_ASSERT_ARG(oid);
	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(repo);

	if (git_index_has_conflicts(index)) {
		git_error_set(GIT_ERROR_INDEX,
			"cannot create a tree from a not fully merged index.");
		return GIT_EUNMERGED;
	}

	if (index->tree != NULL && index->tree->entry_count >= 0) {
		git_oid_cpy(oid, &index->tree->oid);
		return 0;
	}

	/* The tree cache didn't help us; we'll have to write
	 * out a tree. If the index is ignore_case, we must
	 * make it case-sensitive for the duration of the tree-write
	 * operation. */

	if (index->ignore_case) {
		old_ignore_case = true;
		git_index__set_ignore_case(index, false);
	}

	ret = write_tree(oid, repo, index, "", 0, &shared_buf);
	git_str_dispose(&shared_buf);

	if (old_ignore_case)
		git_index__set_ignore_case(index, true);

	index->tree = NULL;

	if (ret < 0)
		return ret;

	git_pool_clear(&index->tree_pool);

	if ((ret = git_tree_lookup(&tree, repo, oid)) < 0)
		return ret;

	/* Read the tree cache into the index */
	ret = git_tree_cache_read_tree(&index->tree, tree, index->oid_type, &index->tree_pool);
	git_tree_free(tree);

	return ret;
}

int git_treebuilder_new(
	git_treebuilder **builder_p,
	git_repository *repo,
	const git_tree *source)
{
	git_treebuilder *bld;
	size_t i;

	GIT_ASSERT_ARG(builder_p);
	GIT_ASSERT_ARG(repo);

	bld = git__calloc(1, sizeof(git_treebuilder));
	GIT_ERROR_CHECK_ALLOC(bld);

	bld->repo = repo;

	if (source != NULL) {
		git_tree_entry *entry_src;

		git_array_foreach(source->entries, i, entry_src) {
			if (append_entry(
				bld, entry_src->filename,
				&entry_src->oid,
				entry_src->attr,
				false) < 0)
				goto on_error;
		}
	}

	*builder_p = bld;
	return 0;

on_error:
	git_treebuilder_free(bld);
	return -1;
}

int git_treebuilder_insert(
	const git_tree_entry **entry_out,
	git_treebuilder *bld,
	const char *filename,
	const git_oid *id,
	git_filemode_t filemode)
{
	git_tree_entry *entry;
	int error;

	GIT_ASSERT_ARG(bld);
	GIT_ASSERT_ARG(id);
	GIT_ASSERT_ARG(filename);

	if ((error = check_entry(bld->repo, filename, id, filemode)) < 0)
		return error;

	if (git_treebuilder_entrymap_get(&entry, &bld->map, filename) == 0) {
		git_oid_cpy(&entry->oid, id);
	} else {
		entry = alloc_entry(filename, strlen(filename), id);
		GIT_ERROR_CHECK_ALLOC(entry);

		if (git_treebuilder_entrymap_put(&bld->map, entry->filename, entry) < 0) {
			git_tree_entry_free(entry);
			git_error_set(GIT_ERROR_TREE, "failed to insert %s", filename);
			return -1;
		}
	}

	entry->attr = filemode;

	if (entry_out)
		*entry_out = entry;

	return 0;
}

static git_tree_entry *treebuilder_get(git_treebuilder *bld, const char *filename)
{
	git_tree_entry *entry;

	GIT_ASSERT_ARG_WITH_RETVAL(bld, NULL);
	GIT_ASSERT_ARG_WITH_RETVAL(filename, NULL);

	if (git_treebuilder_entrymap_get(&entry, &bld->map, filename) != 0)
		return NULL;

	return entry;
}

const git_tree_entry *git_treebuilder_get(git_treebuilder *bld, const char *filename)
{
	return treebuilder_get(bld, filename);
}

int git_treebuilder_remove(git_treebuilder *bld, const char *filename)
{
	git_tree_entry *entry = treebuilder_get(bld, filename);

	if (entry == NULL)
		return tree_error("failed to remove entry: file isn't in the tree", filename);

	git_treebuilder_entrymap_remove(&bld->map, filename);
	git_tree_entry_free(entry);

	return 0;
}

int git_treebuilder_write(git_oid *oid, git_treebuilder *bld)
{
	GIT_ASSERT_ARG(oid);
	GIT_ASSERT_ARG(bld);

	return git_treebuilder__write_with_buffer(oid, bld, &bld->write_cache);
}

int git_treebuilder_filter(
	git_treebuilder *bld,
	git_treebuilder_filter_cb filter,
	void *payload)
{
	const char *filename;
	git_tree_entry *entry;
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;

	GIT_ASSERT_ARG(bld);
	GIT_ASSERT_ARG(filter);

	while (git_treebuilder_entrymap_iterate(&iter, &filename, &entry, &bld->map) == 0) {
		if (filter(entry, payload)) {
			git_treebuilder_entrymap_remove(&bld->map, filename);
			git_tree_entry_free(entry);
		}
	}

	return 0;
}

int git_treebuilder_clear(git_treebuilder *bld)
{
	git_tree_entry *e;
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;

	GIT_ASSERT_ARG(bld);

	while (git_treebuilder_entrymap_iterate(&iter, NULL, &e, &bld->map) == 0)
		git_tree_entry_free(e);

	git_treebuilder_entrymap_clear(&bld->map);

	return 0;
}

void git_treebuilder_free(git_treebuilder *bld)
{
	if (bld == NULL)
		return;

	git_str_dispose(&bld->write_cache);
	git_treebuilder_clear(bld);
	git_treebuilder_entrymap_dispose(&bld->map);
	git__free(bld);
}

static size_t subpath_len(const char *path)
{
	const char *slash_pos = strchr(path, '/');
	if (slash_pos == NULL)
		return strlen(path);

	return slash_pos - path;
}

int git_tree_entry_bypath(
	git_tree_entry **entry_out,
	const git_tree *root,
	const char *path)
{
	int error = 0;
	git_tree *subtree;
	const git_tree_entry *entry;
	size_t filename_len;

	/* Find how long is the current path component (i.e.
	 * the filename between two slashes */
	filename_len = subpath_len(path);

	if (filename_len == 0) {
		git_error_set(GIT_ERROR_TREE, "invalid tree path given");
		return GIT_ENOTFOUND;
	}

	entry = entry_fromname(root, path, filename_len);

	if (entry == NULL) {
		git_error_set(GIT_ERROR_TREE,
			   "the path '%.*s' does not exist in the given tree", (int) filename_len, path);
		return GIT_ENOTFOUND;
	}

	switch (path[filename_len]) {
	case '/':
		/* If there are more components in the path...
		 * then this entry *must* be a tree */
		if (!git_tree_entry__is_tree(entry)) {
			git_error_set(GIT_ERROR_TREE,
				   "the path '%.*s' exists but is not a tree", (int) filename_len, path);
			return GIT_ENOTFOUND;
		}

		/* If there's only a slash left in the path, we
		 * return the current entry; otherwise, we keep
		 * walking down the path */
		if (path[filename_len + 1] != '\0')
			break;
		/* fall through */
	case '\0':
		/* If there are no more components in the path, return
		 * this entry */
		return git_tree_entry_dup(entry_out, entry);
	}

	if (git_tree_lookup(&subtree, root->object.repo, &entry->oid) < 0)
		return -1;

	error = git_tree_entry_bypath(
		entry_out,
		subtree,
		path + filename_len + 1
	);

	git_tree_free(subtree);
	return error;
}

static int tree_walk(
	const git_tree *tree,
	git_treewalk_cb callback,
	git_str *path,
	void *payload,
	bool preorder)
{
	int error = 0;
	size_t i;
	const git_tree_entry *entry;

	git_array_foreach(tree->entries, i, entry) {
		if (preorder) {
			error = callback(path->ptr, entry, payload);
			if (error < 0) { /* negative value stops iteration */
				git_error_set_after_callback_function(error, "git_tree_walk");
				break;
			}
			if (error > 0) { /* positive value skips this entry */
				error = 0;
				continue;
			}
		}

		if (git_tree_entry__is_tree(entry)) {
			git_tree *subtree;
			size_t path_len = git_str_len(path);

			error = git_tree_lookup(&subtree, tree->object.repo, &entry->oid);
			if (error < 0)
				break;

			/* append the next entry to the path */
			git_str_puts(path, entry->filename);
			git_str_putc(path, '/');

			if (git_str_oom(path))
				error = -1;
			else
				error = tree_walk(subtree, callback, path, payload, preorder);

			git_tree_free(subtree);
			if (error != 0)
				break;

			git_str_truncate(path, path_len);
		}

		if (!preorder) {
			error = callback(path->ptr, entry, payload);
			if (error < 0) { /* negative value stops iteration */
				git_error_set_after_callback_function(error, "git_tree_walk");
				break;
			}
			error = 0;
		}
	}

	return error;
}

int git_tree_walk(
	const git_tree *tree,
	git_treewalk_mode mode,
	git_treewalk_cb callback,
	void *payload)
{
	int error = 0;
	git_str root_path = GIT_STR_INIT;

	if (mode != GIT_TREEWALK_POST && mode != GIT_TREEWALK_PRE) {
		git_error_set(GIT_ERROR_INVALID, "invalid walking mode for tree walk");
		return -1;
	}

	error = tree_walk(
		tree, callback, &root_path, payload, (mode == GIT_TREEWALK_PRE));

	git_str_dispose(&root_path);

	return error;
}

static int compare_entries(const void *_a, const void *_b)
{
	const git_tree_update *a = (git_tree_update *) _a;
	const git_tree_update *b = (git_tree_update *) _b;

	return strcmp(a->path, b->path);
}

static int on_dup_entry(void **old, void *new)
{
	GIT_UNUSED(old); GIT_UNUSED(new);

	git_error_set(GIT_ERROR_TREE, "duplicate entries given for update");
	return -1;
}

/*
 * We keep the previous tree and the new one at each level of the
 * stack. When we leave a level we're done with that tree and we can
 * write it out to the odb.
 */
typedef struct {
	git_treebuilder *bld;
	git_tree *tree;
	char *name;
} tree_stack_entry;

/** Count how many slashes (i.e. path components) there are in this string */
GIT_INLINE(size_t) count_slashes(const char *path)
{
	size_t count = 0;
	const char *slash;

	while ((slash = strchr(path, '/')) != NULL) {
		count++;
		path = slash + 1;
	}

	return count;
}

static bool next_component(git_str *out, const char *in)
{
	const char *slash = strchr(in, '/');

	git_str_clear(out);

	if (slash)
		git_str_put(out, in, slash - in);

	return !!slash;
}

static int create_popped_tree(tree_stack_entry *current, tree_stack_entry *popped, git_str *component)
{
	int error;
	git_oid new_tree;

	git_tree_free(popped->tree);

	/* If the tree would be empty, remove it from the one higher up */
	if (git_treebuilder_entrycount(popped->bld) == 0) {
		git_treebuilder_free(popped->bld);
		error = git_treebuilder_remove(current->bld, popped->name);
		git__free(popped->name);
		return error;
	}

	error = git_treebuilder_write(&new_tree, popped->bld);
	git_treebuilder_free(popped->bld);

	if (error < 0) {
		git__free(popped->name);
		return error;
	}

	/* We've written out the tree, now we have to put the new value into its parent */
	git_str_clear(component);
	git_str_puts(component, popped->name);
	git__free(popped->name);

	GIT_ERROR_CHECK_ALLOC(component->ptr);

	/* Error out if this would create a D/F conflict in this update */
	if (current->tree) {
		const git_tree_entry *to_replace;
		to_replace = git_tree_entry_byname(current->tree, component->ptr);
		if (to_replace && git_tree_entry_type(to_replace) != GIT_OBJECT_TREE) {
			git_error_set(GIT_ERROR_TREE, "D/F conflict when updating tree");
			return -1;
		}
	}

	return git_treebuilder_insert(NULL, current->bld, component->ptr, &new_tree, GIT_FILEMODE_TREE);
}

int git_tree_create_updated(git_oid *out, git_repository *repo, git_tree *baseline, size_t nupdates, const git_tree_update *updates)
{
	git_array_t(tree_stack_entry) stack = GIT_ARRAY_INIT;
	tree_stack_entry *root_elem;
	git_vector entries;
	int error;
	size_t i;
	git_str component = GIT_STR_INIT;

	if ((error = git_vector_init(&entries, nupdates, compare_entries)) < 0)
		return error;

	/* Sort the entries for treversal */
	for (i = 0 ; i < nupdates; i++)	{
		if ((error = git_vector_insert_sorted(&entries, (void *) &updates[i], on_dup_entry)) < 0)
			goto cleanup;
	}

	root_elem = git_array_alloc(stack);
	GIT_ERROR_CHECK_ALLOC(root_elem);
	memset(root_elem, 0, sizeof(*root_elem));

	if (baseline && (error = git_tree_dup(&root_elem->tree, baseline)) < 0)
		goto cleanup;

	if ((error = git_treebuilder_new(&root_elem->bld, repo, root_elem->tree)) < 0)
		goto cleanup;

	for (i = 0; i < nupdates; i++) {
		const git_tree_update *last_update = i == 0 ? NULL : git_vector_get(&entries, i-1);
		const git_tree_update *update = git_vector_get(&entries, i);
		size_t common_prefix = 0, steps_up, j;
		const char *path;

		/* Figure out how much we need to change from the previous tree */
		if (last_update)
			common_prefix = git_fs_path_common_dirlen(last_update->path, update->path);

		/*
		 * The entries are sorted, so when we find we're no
		 * longer in the same directory, we need to abandon
		 * the old tree (steps up) and dive down to the next
		 * one.
		 */
		steps_up = last_update == NULL ? 0 : count_slashes(&last_update->path[common_prefix]);

		for (j = 0; j < steps_up; j++) {
			tree_stack_entry *current, *popped = git_array_pop(stack);
			GIT_ASSERT(popped);

			current = git_array_last(stack);
			GIT_ASSERT(current);

			if ((error = create_popped_tree(current, popped, &component)) < 0)
				goto cleanup;
		}

		/* Now that we've created the trees we popped from the stack, let's go back down */
		path = &update->path[common_prefix];
		while (next_component(&component, path)) {
			tree_stack_entry *last, *new_entry;
			const git_tree_entry *entry;

			last = git_array_last(stack);
			entry = last->tree ? git_tree_entry_byname(last->tree, component.ptr) : NULL;
			if (!entry)
				entry = treebuilder_get(last->bld, component.ptr);

			if (entry && git_tree_entry_type(entry) != GIT_OBJECT_TREE) {
				git_error_set(GIT_ERROR_TREE, "D/F conflict when updating tree");
				error = -1;
				goto cleanup;
			}

			new_entry = git_array_alloc(stack);
			GIT_ERROR_CHECK_ALLOC(new_entry);
			memset(new_entry, 0, sizeof(*new_entry));

			new_entry->tree = NULL;
			if (entry && (error = git_tree_lookup(&new_entry->tree, repo, git_tree_entry_id(entry))) < 0)
				goto cleanup;

			if ((error = git_treebuilder_new(&new_entry->bld, repo, new_entry->tree)) < 0)
				goto cleanup;

			new_entry->name = git__strdup(component.ptr);
			GIT_ERROR_CHECK_ALLOC(new_entry->name);

			/* Get to the start of the next component */
			path += component.size + 1;
		}

		/* After all that, we're finally at the place where we want to perform the update */
		switch (update->action) {
			case GIT_TREE_UPDATE_UPSERT:
			{
				/* Make sure we're replacing something of the same type */
				tree_stack_entry *last = git_array_last(stack);
				char *basename = git_fs_path_basename(update->path);
				const git_tree_entry *e = git_treebuilder_get(last->bld, basename);
				if (e && git_tree_entry_type(e) != git_object__type_from_filemode(update->filemode)) {
					git__free(basename);
					git_error_set(GIT_ERROR_TREE, "cannot replace '%s' with '%s' at '%s'",
						   git_object_type2string(git_tree_entry_type(e)),
						   git_object_type2string(git_object__type_from_filemode(update->filemode)),
						   update->path);
					error = -1;
					goto cleanup;
				}

				error = git_treebuilder_insert(NULL, last->bld, basename, &update->id, update->filemode);
				git__free(basename);
				break;
			}
			case GIT_TREE_UPDATE_REMOVE:
			{
				tree_stack_entry *last = git_array_last(stack);
				char *basename = git_fs_path_basename(update->path);
				error = git_treebuilder_remove(last->bld, basename);
				git__free(basename);
				break;
			}
			default:
				git_error_set(GIT_ERROR_TREE, "unknown action for update");
				error = -1;
				goto cleanup;
		}

		if (error < 0)
			goto cleanup;
	}

	/* We're done, go up the stack again and write out the tree */
	{
		tree_stack_entry *current = NULL, *popped = NULL;
		while ((popped = git_array_pop(stack)) != NULL) {
			current = git_array_last(stack);
			/* We've reached the top, current is the root tree */
			if (!current)
				break;

			if ((error = create_popped_tree(current, popped, &component)) < 0)
				goto cleanup;
		}

		/* Write out the root tree */
		git__free(popped->name);
		git_tree_free(popped->tree);

		error = git_treebuilder_write(out, popped->bld);
		git_treebuilder_free(popped->bld);
		if (error < 0)
			goto cleanup;
	}

cleanup:
	{
		tree_stack_entry *e;
		while ((e = git_array_pop(stack)) != NULL) {
			git_treebuilder_free(e->bld);
			git_tree_free(e->tree);
			git__free(e->name);
		}
	}

	git_str_dispose(&component);
	git_array_clear(stack);
	git_vector_dispose(&entries);
	return error;
}

/* Deprecated Functions */

#ifndef GIT_DEPRECATE_HARD

int git_treebuilder_write_with_buffer(git_oid *oid, git_treebuilder *bld, git_buf *buf)
{
	GIT_UNUSED(buf);

	return git_treebuilder_write(oid, bld);
}

#endif
