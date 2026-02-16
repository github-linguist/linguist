/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "index.h"

#include <stddef.h>

#include "repository.h"
#include "tree.h"
#include "tree-cache.h"
#include "hash.h"
#include "iterator.h"
#include "pathspec.h"
#include "ignore.h"
#include "blob.h"
#include "diff.h"
#include "varint.h"
#include "path.h"
#include "index_map.h"

#include "git2/odb.h"
#include "git2/oid.h"
#include "git2/blob.h"
#include "git2/config.h"
#include "git2/sys/index.h"

static int index_apply_to_wd_diff(git_index *index, int action, const git_strarray *paths,
				  unsigned int flags,
				  git_index_matched_path_cb cb, void *payload);

static const size_t INDEX_HEADER_SIZE = 12;

static const unsigned int INDEX_VERSION_NUMBER_DEFAULT = 2;
static const unsigned int INDEX_VERSION_NUMBER_LB = 2;
static const unsigned int INDEX_VERSION_NUMBER_EXT = 3;
static const unsigned int INDEX_VERSION_NUMBER_COMP = 4;
static const unsigned int INDEX_VERSION_NUMBER_UB = 4;

static const unsigned int INDEX_HEADER_SIG = 0x44495243;
static const char INDEX_EXT_TREECACHE_SIG[] = {'T', 'R', 'E', 'E'};
static const char INDEX_EXT_UNMERGED_SIG[] = {'R', 'E', 'U', 'C'};
static const char INDEX_EXT_CONFLICT_NAME_SIG[] = {'N', 'A', 'M', 'E'};

#define INDEX_OWNER(idx) ((git_repository *)(GIT_REFCOUNT_OWNER(idx)))

struct index_header {
	uint32_t signature;
	uint32_t version;
	uint32_t entry_count;
};

struct index_extension {
	char signature[4];
	uint32_t extension_size;
};

struct entry_time {
	uint32_t seconds;
	uint32_t nanoseconds;
};

struct entry_common {
	struct entry_time ctime;
	struct entry_time mtime;
	uint32_t dev;
	uint32_t ino;
	uint32_t mode;
	uint32_t uid;
	uint32_t gid;
	uint32_t file_size;
};

#define entry_short(oid_size)                        \
	struct {                                     \
		struct entry_common common;          \
		unsigned char oid[oid_size];         \
		uint16_t flags;                      \
		char path[1]; /* arbitrary length */ \
	}

#define entry_long(oid_size)                         \
	struct {                                     \
		struct entry_common common;          \
		unsigned char oid[oid_size];         \
		uint16_t flags;                      \
		uint16_t flags_extended;             \
		char path[1]; /* arbitrary length */ \
	}

typedef entry_short(GIT_OID_SHA1_SIZE) index_entry_short_sha1;
typedef entry_long(GIT_OID_SHA1_SIZE) index_entry_long_sha1;

#ifdef GIT_EXPERIMENTAL_SHA256
typedef entry_short(GIT_OID_SHA256_SIZE) index_entry_short_sha256;
typedef entry_long(GIT_OID_SHA256_SIZE) index_entry_long_sha256;
#endif

#undef entry_short
#undef entry_long

struct entry_srch_key {
	const char *path;
	size_t pathlen;
	int stage;
};

struct entry_internal {
	git_index_entry entry;
	size_t pathlen;
	char path[GIT_FLEX_ARRAY];
};

struct reuc_entry_internal {
	git_index_reuc_entry entry;
	size_t pathlen;
	char path[GIT_FLEX_ARRAY];
};

bool git_index__enforce_unsaved_safety = false;

/* local declarations */
static int read_extension(size_t *read_len, git_index *index, size_t checksum_size, const char *buffer, size_t buffer_size);
static int read_header(struct index_header *dest, const void *buffer);

static int parse_index(git_index *index, const char *buffer, size_t buffer_size);
static bool is_index_extended(git_index *index);
static int write_index(unsigned char checksum[GIT_HASH_MAX_SIZE], size_t *checksum_size, git_index *index, git_filebuf *file);

static void index_entry_free(git_index_entry *entry);
static void index_entry_reuc_free(git_index_reuc_entry *reuc);

int git_index_entry_srch(const void *key, const void *array_member)
{
	const struct entry_srch_key *srch_key = key;
	const struct entry_internal *entry = array_member;
	int cmp;
	size_t len1, len2, len;

	len1 = srch_key->pathlen;
	len2 = entry->pathlen;
	len = len1 < len2 ? len1 : len2;

	cmp = memcmp(srch_key->path, entry->path, len);
	if (cmp)
		return cmp;
	if (len1 < len2)
		return -1;
	if (len1 > len2)
		return 1;

	if (srch_key->stage != GIT_INDEX_STAGE_ANY)
		return srch_key->stage - GIT_INDEX_ENTRY_STAGE(&entry->entry);

	return 0;
}

int git_index_entry_isrch(const void *key, const void *array_member)
{
	const struct entry_srch_key *srch_key = key;
	const struct entry_internal *entry = array_member;
	int cmp;
	size_t len1, len2, len;

	len1 = srch_key->pathlen;
	len2 = entry->pathlen;
	len = len1 < len2 ? len1 : len2;

	cmp = strncasecmp(srch_key->path, entry->path, len);

	if (cmp)
		return cmp;
	if (len1 < len2)
		return -1;
	if (len1 > len2)
		return 1;

	if (srch_key->stage != GIT_INDEX_STAGE_ANY)
		return srch_key->stage - GIT_INDEX_ENTRY_STAGE(&entry->entry);

	return 0;
}

static int index_entry_srch_path(const void *path, const void *array_member)
{
	const git_index_entry *entry = array_member;

	return strcmp((const char *)path, entry->path);
}

static int index_entry_isrch_path(const void *path, const void *array_member)
{
	const git_index_entry *entry = array_member;

	return strcasecmp((const char *)path, entry->path);
}

int git_index_entry_cmp(const void *a, const void *b)
{
	int diff;
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	diff = strcmp(entry_a->path, entry_b->path);

	if (diff == 0)
		diff = (GIT_INDEX_ENTRY_STAGE(entry_a) - GIT_INDEX_ENTRY_STAGE(entry_b));

	return diff;
}

int git_index_entry_icmp(const void *a, const void *b)
{
	int diff;
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	diff = strcasecmp(entry_a->path, entry_b->path);

	if (diff == 0)
		diff = (GIT_INDEX_ENTRY_STAGE(entry_a) - GIT_INDEX_ENTRY_STAGE(entry_b));

	return diff;
}

static int conflict_name_cmp(const void *a, const void *b)
{
	const git_index_name_entry *name_a = a;
	const git_index_name_entry *name_b = b;

	if (name_a->ancestor && !name_b->ancestor)
		return 1;

	if (!name_a->ancestor && name_b->ancestor)
		return -1;

	if (name_a->ancestor)
		return strcmp(name_a->ancestor, name_b->ancestor);

	if (!name_a->ours || !name_b->ours)
		return 0;

	return strcmp(name_a->ours, name_b->ours);
}

/**
 * TODO: enable this when resolving case insensitive conflicts
 */
#if 0
static int conflict_name_icmp(const void *a, const void *b)
{
	const git_index_name_entry *name_a = a;
	const git_index_name_entry *name_b = b;

	if (name_a->ancestor && !name_b->ancestor)
		return 1;

	if (!name_a->ancestor && name_b->ancestor)
		return -1;

	if (name_a->ancestor)
		return strcasecmp(name_a->ancestor, name_b->ancestor);

	if (!name_a->ours || !name_b->ours)
		return 0;

	return strcasecmp(name_a->ours, name_b->ours);
}
#endif

static int reuc_srch(const void *key, const void *array_member)
{
	const git_index_reuc_entry *reuc = array_member;

	return strcmp(key, reuc->path);
}

static int reuc_isrch(const void *key, const void *array_member)
{
	const git_index_reuc_entry *reuc = array_member;

	return strcasecmp(key, reuc->path);
}

static int reuc_cmp(const void *a, const void *b)
{
	const git_index_reuc_entry *info_a = a;
	const git_index_reuc_entry *info_b = b;

	return strcmp(info_a->path, info_b->path);
}

static int reuc_icmp(const void *a, const void *b)
{
	const git_index_reuc_entry *info_a = a;
	const git_index_reuc_entry *info_b = b;

	return strcasecmp(info_a->path, info_b->path);
}

static void index_entry_reuc_free(git_index_reuc_entry *reuc)
{
	git__free(reuc);
}

static void index_entry_free(git_index_entry *entry)
{
	if (!entry)
		return;

	memset(&entry->id, 0, sizeof(entry->id));
	git__free(entry);
}

unsigned int git_index__create_mode(unsigned int mode)
{
	if (S_ISLNK(mode))
		return S_IFLNK;

	if (S_ISDIR(mode) || (mode & S_IFMT) == (S_IFLNK | S_IFDIR))
		return (S_IFLNK | S_IFDIR);

	return S_IFREG | GIT_PERMS_CANONICAL(mode);
}

static unsigned int index_merge_mode(
	git_index *index, git_index_entry *existing, unsigned int mode)
{
	if (index->no_symlinks && S_ISREG(mode) &&
		existing && S_ISLNK(existing->mode))
		return existing->mode;

	if (index->distrust_filemode && S_ISREG(mode))
		return (existing && S_ISREG(existing->mode)) ?
			existing->mode : git_index__create_mode(0666);

	return git_index__create_mode(mode);
}

GIT_INLINE(int) index_find_in_entries(
	size_t *out, git_vector *entries, git_vector_cmp entry_srch,
	const char *path, size_t path_len, int stage)
{
	struct entry_srch_key srch_key;
	srch_key.path = path;
	srch_key.pathlen = !path_len ? strlen(path) : path_len;
	srch_key.stage = stage;
	return git_vector_bsearch2(out, entries, entry_srch, &srch_key);
}

GIT_INLINE(int) index_find(
	size_t *out, git_index *index,
	const char *path, size_t path_len, int stage)
{
	git_vector_sort(&index->entries);

	return index_find_in_entries(
		out, &index->entries, index->entries_search, path, path_len, stage);
}

void git_index__set_ignore_case(git_index *index, bool ignore_case)
{
	index->ignore_case = ignore_case;
	index->entries_map.ignore_case = ignore_case;

	if (ignore_case) {
		index->entries_cmp_path    = git__strcasecmp_cb;
		index->entries_search      = git_index_entry_isrch;
		index->entries_search_path = index_entry_isrch_path;
		index->reuc_search         = reuc_isrch;
	} else {
		index->entries_cmp_path    = git__strcmp_cb;
		index->entries_search      = git_index_entry_srch;
		index->entries_search_path = index_entry_srch_path;
		index->reuc_search         = reuc_srch;
	}

	git_vector_set_cmp(&index->entries,
		ignore_case ? git_index_entry_icmp : git_index_entry_cmp);
	git_vector_sort(&index->entries);

	git_vector_set_cmp(&index->reuc, ignore_case ? reuc_icmp : reuc_cmp);
	git_vector_sort(&index->reuc);
}

int git_index__open(
	git_index **index_out,
	const char *index_path,
	git_oid_t oid_type)
{
	git_index *index;
	int error = -1;

	GIT_ASSERT_ARG(index_out);

	index = git__calloc(1, sizeof(git_index));
	GIT_ERROR_CHECK_ALLOC(index);

	GIT_ASSERT_ARG(git_oid_type_is_valid(oid_type));
	index->oid_type = oid_type;

	if (git_pool_init(&index->tree_pool, 1) < 0)
		goto fail;

	if (index_path != NULL) {
		index->index_file_path = git__strdup(index_path);
		if (!index->index_file_path)
			goto fail;

		/* Check if index file is stored on disk already */
		if (git_fs_path_exists(index->index_file_path) == true)
			index->on_disk = 1;
	}

	if (git_vector_init(&index->entries, 32, git_index_entry_cmp) < 0 ||
	    git_vector_init(&index->names, 8, conflict_name_cmp) < 0 ||
	    git_vector_init(&index->reuc, 8, reuc_cmp) < 0 ||
	    git_vector_init(&index->deleted, 8, git_index_entry_cmp) < 0)
		goto fail;

	index->entries_cmp_path = git__strcmp_cb;
	index->entries_search = git_index_entry_srch;
	index->entries_search_path = index_entry_srch_path;
	index->reuc_search = reuc_srch;
	index->version = INDEX_VERSION_NUMBER_DEFAULT;

	if (index_path != NULL && (error = git_index_read(index, true)) < 0)
		goto fail;

	*index_out = index;
	GIT_REFCOUNT_INC(index);

	return 0;

fail:
	git_pool_clear(&index->tree_pool);
	git_index_free(index);
	return error;
}

#ifdef GIT_EXPERIMENTAL_SHA256
int git_index_open(
	git_index **index_out,
	const char *index_path,
	const git_index_options *opts)
{
	return git_index__open(index_out, index_path,
		opts && opts->oid_type ? opts->oid_type : GIT_OID_DEFAULT);
}
#else
int git_index_open(git_index **index_out, const char *index_path)
{
	return git_index__open(index_out, index_path, GIT_OID_SHA1);
}
#endif

int git_index__new(git_index **out, git_oid_t oid_type)
{
	return git_index__open(out, NULL, oid_type);
}

#ifdef GIT_EXPERIMENTAL_SHA256
int git_index_new(git_index **out, const git_index_options *opts)
{
	return git_index__new(out,
		opts && opts->oid_type ? opts->oid_type : GIT_OID_DEFAULT);
}
#else
int git_index_new(git_index **out)
{
	return git_index__new(out, GIT_OID_SHA1);
}
#endif

static void index_free(git_index *index)
{
	/* index iterators increment the refcount of the index, so if we
	 * get here then there should be no outstanding iterators.
	 */
	if (git_atomic32_get(&index->readers))
		return;

	git_index_clear(index);
	git_index_entrymap_dispose(&index->entries_map);
	git_vector_dispose(&index->entries);
	git_vector_dispose(&index->names);
	git_vector_dispose(&index->reuc);
	git_vector_dispose(&index->deleted);

	git__free(index->index_file_path);

	git__memzero(index, sizeof(*index));
	git__free(index);
}

void git_index_free(git_index *index)
{
	if (index == NULL)
		return;

	GIT_REFCOUNT_DEC(index, index_free);
}

/* call with locked index */
static void index_free_deleted(git_index *index)
{
	int readers = (int)git_atomic32_get(&index->readers);
	size_t i;

	if (readers > 0 || !index->deleted.length)
		return;

	for (i = 0; i < index->deleted.length; ++i) {
		git_index_entry *ie = git_atomic_swap(index->deleted.contents[i], NULL);
		index_entry_free(ie);
	}

	git_vector_clear(&index->deleted);
}

/* call with locked index */
static int index_remove_entry(git_index *index, size_t pos)
{
	int error = 0;
	git_index_entry *entry = git_vector_get(&index->entries, pos);

	if (entry != NULL) {
		git_tree_cache_invalidate_path(index->tree, entry->path);
		git_index_entrymap_remove(&index->entries_map, entry);
	}

	error = git_vector_remove(&index->entries, pos);

	if (!error) {
		if (git_atomic32_get(&index->readers) > 0) {
			error = git_vector_insert(&index->deleted, entry);
		} else {
			index_entry_free(entry);
		}

		index->dirty = 1;
	}

	return error;
}

int git_index_clear(git_index *index)
{
	int error = 0;

	GIT_ASSERT_ARG(index);

	index->dirty = 1;
	index->tree = NULL;
	git_pool_clear(&index->tree_pool);

	git_index_entrymap_clear(&index->entries_map);

	while (!error && index->entries.length > 0)
		error = index_remove_entry(index, index->entries.length - 1);

	if (error)
		goto done;

	index_free_deleted(index);

	if ((error = git_index_name_clear(index)) < 0 ||
		(error = git_index_reuc_clear(index)) < 0)
	    goto done;

	git_futils_filestamp_set(&index->stamp, NULL);

done:
	return error;
}

static int create_index_error(int error, const char *msg)
{
	git_error_set_str(GIT_ERROR_INDEX, msg);
	return error;
}

int git_index_set_caps(git_index *index, int caps)
{
	unsigned int old_ignore_case;

	GIT_ASSERT_ARG(index);

	old_ignore_case = index->ignore_case;

	if (caps == GIT_INDEX_CAPABILITY_FROM_OWNER) {
		git_repository *repo = INDEX_OWNER(index);
		int val;

		if (!repo)
			return create_index_error(
				-1, "cannot access repository to set index caps");

		if (!git_repository__configmap_lookup(&val, repo, GIT_CONFIGMAP_IGNORECASE))
			index->ignore_case = (val != 0);
		if (!git_repository__configmap_lookup(&val, repo, GIT_CONFIGMAP_FILEMODE))
			index->distrust_filemode = (val == 0);
		if (!git_repository__configmap_lookup(&val, repo, GIT_CONFIGMAP_SYMLINKS))
			index->no_symlinks = (val == 0);
	}
	else {
		index->ignore_case = ((caps & GIT_INDEX_CAPABILITY_IGNORE_CASE) != 0);
		index->distrust_filemode = ((caps & GIT_INDEX_CAPABILITY_NO_FILEMODE) != 0);
		index->no_symlinks = ((caps & GIT_INDEX_CAPABILITY_NO_SYMLINKS) != 0);
	}

	if (old_ignore_case != index->ignore_case) {
		git_index__set_ignore_case(index, (bool)index->ignore_case);
	}

	return 0;
}

int git_index_caps(const git_index *index)
{
	return ((index->ignore_case ? GIT_INDEX_CAPABILITY_IGNORE_CASE : 0) |
			(index->distrust_filemode ? GIT_INDEX_CAPABILITY_NO_FILEMODE : 0) |
			(index->no_symlinks ? GIT_INDEX_CAPABILITY_NO_SYMLINKS : 0));
}

#ifndef GIT_DEPRECATE_HARD
const git_oid *git_index_checksum(git_index *index)
{
	return (git_oid *)index->checksum;
}
#endif

/**
 * Returns 1 for changed, 0 for not changed and <0 for errors
 */
static int compare_checksum(git_index *index)
{
	int fd;
	ssize_t bytes_read;
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	size_t checksum_size = git_oid_size(index->oid_type);

	if ((fd = p_open(index->index_file_path, O_RDONLY)) < 0)
		return fd;

	if (p_lseek(fd, (0 - (ssize_t)checksum_size), SEEK_END) < 0) {
		p_close(fd);
		git_error_set(GIT_ERROR_OS, "failed to seek to end of file");
		return -1;
	}

	bytes_read = p_read(fd, checksum, checksum_size);
	p_close(fd);

	if (bytes_read < (ssize_t)checksum_size)
		return -1;

	return !!memcmp(checksum, index->checksum, checksum_size);
}

int git_index_read(git_index *index, int force)
{
	int error = 0, updated;
	git_str buffer = GIT_STR_INIT;
	git_futils_filestamp stamp = index->stamp;

	if (!index->index_file_path)
		return create_index_error(-1,
			"failed to read index: The index is in-memory only");

	index->on_disk = git_fs_path_exists(index->index_file_path);

	if (!index->on_disk) {
		if (force && (error = git_index_clear(index)) < 0)
			return error;

		index->dirty = 0;
		return 0;
	}

	if ((updated = git_futils_filestamp_check(&stamp, index->index_file_path) < 0) ||
	    ((updated = compare_checksum(index)) < 0)) {
		git_error_set(
			GIT_ERROR_INDEX,
			"failed to read index: '%s' no longer exists",
			index->index_file_path);
		return updated;
	}

	if (!updated && !force)
		return 0;

	error = git_futils_readbuffer(&buffer, index->index_file_path);
	if (error < 0)
		return error;

	index->tree = NULL;
	git_pool_clear(&index->tree_pool);

	error = git_index_clear(index);

	if (!error)
		error = parse_index(index, buffer.ptr, buffer.size);

	if (!error) {
		git_futils_filestamp_set(&index->stamp, &stamp);
		index->dirty = 0;
	}

	git_str_dispose(&buffer);
	return error;
}

int git_index_read_safely(git_index *index)
{
	if (git_index__enforce_unsaved_safety && index->dirty) {
		git_error_set(GIT_ERROR_INDEX,
			"the index has unsaved changes that would be overwritten by this operation");
		return GIT_EINDEXDIRTY;
	}

	return git_index_read(index, false);
}

static bool is_racy_entry(git_index *index, const git_index_entry *entry)
{
	/* Git special-cases submodules in the check */
	if (S_ISGITLINK(entry->mode))
		return false;

	return git_index_entry_newer_than_index(entry, index);
}

/*
 * Force the next diff to take a look at those entries which have the
 * same timestamp as the current index.
 */
static int truncate_racily_clean(git_index *index)
{
	size_t i;
	int error;
	git_index_entry *entry;
	git_diff_options diff_opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	git_vector paths = GIT_VECTOR_INIT;
	git_diff_delta *delta;

	/* Nothing to do if there's no repo to talk about */
	if (!INDEX_OWNER(index))
		return 0;

	/* If there's no workdir, we can't know where to even check */
	if (!git_repository_workdir(INDEX_OWNER(index)))
		return 0;

	diff_opts.flags |= GIT_DIFF_INCLUDE_TYPECHANGE | GIT_DIFF_IGNORE_SUBMODULES | GIT_DIFF_DISABLE_PATHSPEC_MATCH;
	git_vector_foreach(&index->entries, i, entry) {
		if ((entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE) == 0 &&
			is_racy_entry(index, entry))
			git_vector_insert(&paths, (char *)entry->path);
	}

	if (paths.length == 0)
		goto done;

	diff_opts.pathspec.count = paths.length;
	diff_opts.pathspec.strings = (char **)paths.contents;

	if ((error = git_diff_index_to_workdir(&diff, INDEX_OWNER(index), index, &diff_opts)) < 0) {
		git_vector_dispose(&paths);
		return error;
	}

	git_vector_foreach(&diff->deltas, i, delta) {
		entry = (git_index_entry *)git_index_get_bypath(index, delta->old_file.path, 0);

		/* Ensure that we have a stage 0 for this file (ie, it's not a
		 * conflict), otherwise smudging it is quite pointless.
		 */
		if (entry) {
			entry->file_size = 0;
			index->dirty = 1;
		}
	}

done:
	git_diff_free(diff);
	git_vector_dispose(&paths);
	return 0;
}

unsigned git_index_version(git_index *index)
{
	GIT_ASSERT_ARG(index);

	return index->version;
}

int git_index_set_version(git_index *index, unsigned int version)
{
	GIT_ASSERT_ARG(index);

	if (version < INDEX_VERSION_NUMBER_LB ||
	    version > INDEX_VERSION_NUMBER_UB) {
		git_error_set(GIT_ERROR_INDEX, "invalid version number");
		return -1;
	}

	index->version = version;

	return 0;
}

int git_index_write(git_index *index)
{
	git_indexwriter writer = GIT_INDEXWRITER_INIT;
	int error;

	truncate_racily_clean(index);

	if ((error = git_indexwriter_init(&writer, index)) == 0 &&
		(error = git_indexwriter_commit(&writer)) == 0)
		index->dirty = 0;

	git_indexwriter_cleanup(&writer);

	return error;
}

const char *git_index_path(const git_index *index)
{
	GIT_ASSERT_ARG_WITH_RETVAL(index, NULL);
	return index->index_file_path;
}

int git_index_write_tree(git_oid *oid, git_index *index)
{
	git_repository *repo;

	GIT_ASSERT_ARG(oid);
	GIT_ASSERT_ARG(index);

	repo = INDEX_OWNER(index);

	if (repo == NULL)
		return create_index_error(-1, "Failed to write tree. "
		  "the index file is not backed up by an existing repository");

	return git_tree__write_index(oid, index, repo);
}

int git_index_write_tree_to(
	git_oid *oid, git_index *index, git_repository *repo)
{
	GIT_ASSERT_ARG(oid);
	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(repo);

	return git_tree__write_index(oid, index, repo);
}

size_t git_index_entrycount(const git_index *index)
{
	GIT_ASSERT_ARG(index);

	return index->entries.length;
}

const git_index_entry *git_index_get_byindex(
	git_index *index, size_t n)
{
	GIT_ASSERT_ARG_WITH_RETVAL(index, NULL);

	git_vector_sort(&index->entries);
	return git_vector_get(&index->entries, n);
}

const git_index_entry *git_index_get_bypath(
	git_index *index, const char *path, int stage)
{
	git_index_entry key = {{ 0 }};
	git_index_entry *value;

	GIT_ASSERT_ARG_WITH_RETVAL(index, NULL);

	key.path = path;
	GIT_INDEX_ENTRY_STAGE_SET(&key, stage);

	if (git_index_entrymap_get(&value, &index->entries_map, &key) != 0) {
		git_error_set(GIT_ERROR_INDEX, "index does not contain '%s'", path);
		return NULL;
	}

	return value;
}

void git_index_entry__init_from_stat(
	git_index_entry *entry, struct stat *st, bool trust_mode)
{
	entry->ctime.seconds = (int32_t)st->st_ctime;
	entry->mtime.seconds = (int32_t)st->st_mtime;
#if defined(GIT_USE_NSEC)
	entry->mtime.nanoseconds = st->st_mtime_nsec;
	entry->ctime.nanoseconds = st->st_ctime_nsec;
#endif
	entry->dev  = st->st_rdev;
	entry->ino  = st->st_ino;
	entry->mode = (!trust_mode && S_ISREG(st->st_mode)) ?
		git_index__create_mode(0666) : git_index__create_mode(st->st_mode);
	entry->uid  = st->st_uid;
	entry->gid  = st->st_gid;
	entry->file_size = (uint32_t)st->st_size;
}

static void index_entry_adjust_namemask(
		git_index_entry *entry,
		size_t path_length)
{
	entry->flags &= ~GIT_INDEX_ENTRY_NAMEMASK;

	if (path_length < GIT_INDEX_ENTRY_NAMEMASK)
		entry->flags |= path_length & GIT_INDEX_ENTRY_NAMEMASK;
	else
		entry->flags |= GIT_INDEX_ENTRY_NAMEMASK;
}

/* When `from_workdir` is true, we will validate the paths to avoid placing
 * paths that are invalid for the working directory on the current filesystem
 * (eg, on Windows, we will disallow `GIT~1`, `AUX`, `COM1`, etc).  This
 * function will *always* prevent `.git` and directory traversal `../` from
 * being added to the index.
 */
static int index_entry_create(
	git_index_entry **out,
	git_repository *repo,
	const char *path,
	struct stat *st,
	bool from_workdir)
{
	size_t pathlen = strlen(path), alloclen;
	struct entry_internal *entry;
	unsigned int path_valid_flags = GIT_PATH_REJECT_INDEX_DEFAULTS;
	uint16_t mode = 0;

	/* always reject placing `.git` in the index and directory traversal.
	 * when requested, disallow platform-specific filenames and upgrade to
	 * the platform-specific `.git` tests (eg, `git~1`, etc).
	 */
	if (from_workdir)
		path_valid_flags |= GIT_PATH_REJECT_WORKDIR_DEFAULTS;
	if (st)
		mode = st->st_mode;

	if (!git_path_is_valid(repo, path, mode, path_valid_flags)) {
		git_error_set(GIT_ERROR_INDEX, "invalid path: '%s'", path);
		return -1;
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(struct entry_internal), pathlen);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	entry = git__calloc(1, alloclen);
	GIT_ERROR_CHECK_ALLOC(entry);

	entry->pathlen = pathlen;
	memcpy(entry->path, path, pathlen);
	entry->entry.path = entry->path;

	*out = (git_index_entry *)entry;
	return 0;
}

static int index_entry_init(
	git_index_entry **entry_out,
	git_index *index,
	const char *rel_path)
{
	int error = 0;
	git_index_entry *entry = NULL;
	git_str path = GIT_STR_INIT;
	struct stat st;
	git_oid oid;
	git_repository *repo;

	if (INDEX_OWNER(index) == NULL)
		return create_index_error(-1,
			"could not initialize index entry. "
			"Index is not backed up by an existing repository.");

	/*
	 * FIXME: this is duplicated with the work in
	 * git_blob__create_from_paths. It should accept an optional stat
	 * structure so we can pass in the one we have to do here.
	 */
	repo = INDEX_OWNER(index);
	if (git_repository__ensure_not_bare(repo, "create blob from file") < 0)
		return GIT_EBAREREPO;

	if (git_repository_workdir_path(&path, repo, rel_path) < 0)
		return -1;

	error = git_fs_path_lstat(path.ptr, &st);
	git_str_dispose(&path);

	if (error < 0)
		return error;

	if (index_entry_create(&entry, INDEX_OWNER(index), rel_path, &st, true) < 0)
		return -1;

	/* write the blob to disk and get the oid and stat info */
	error = git_blob__create_from_paths(
		&oid, &st, INDEX_OWNER(index), NULL, rel_path, 0, true);

	if (error < 0) {
		index_entry_free(entry);
		return error;
	}

	entry->id = oid;
	git_index_entry__init_from_stat(entry, &st, !index->distrust_filemode);

	*entry_out = (git_index_entry *)entry;
	return 0;
}

static git_index_reuc_entry *reuc_entry_alloc(const char *path)
{
	size_t pathlen = strlen(path),
		structlen = sizeof(struct reuc_entry_internal),
		alloclen;
	struct reuc_entry_internal *entry;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, structlen, pathlen) ||
		GIT_ADD_SIZET_OVERFLOW(&alloclen, alloclen, 1))
		return NULL;

	entry = git__calloc(1, alloclen);
	if (!entry)
		return NULL;

	entry->pathlen = pathlen;
	memcpy(entry->path, path, pathlen);
	entry->entry.path = entry->path;

	return (git_index_reuc_entry *)entry;
}

static int index_entry_reuc_init(git_index_reuc_entry **reuc_out,
	const char *path,
	int ancestor_mode, const git_oid *ancestor_oid,
	int our_mode, const git_oid *our_oid,
	int their_mode, const git_oid *their_oid)
{
	git_index_reuc_entry *reuc = NULL;

	GIT_ASSERT_ARG(reuc_out);
	GIT_ASSERT_ARG(path);

	*reuc_out = reuc = reuc_entry_alloc(path);
	GIT_ERROR_CHECK_ALLOC(reuc);

	if ((reuc->mode[0] = ancestor_mode) > 0) {
		GIT_ASSERT(ancestor_oid);
		git_oid_cpy(&reuc->oid[0], ancestor_oid);
	}

	if ((reuc->mode[1] = our_mode) > 0) {
		GIT_ASSERT(our_oid);
		git_oid_cpy(&reuc->oid[1], our_oid);
	}

	if ((reuc->mode[2] = their_mode) > 0) {
		GIT_ASSERT(their_oid);
		git_oid_cpy(&reuc->oid[2], their_oid);
	}

	return 0;
}

static void index_entry_cpy(
	git_index_entry *tgt,
	const git_index_entry *src)
{
	const char *tgt_path = tgt->path;
	memcpy(tgt, src, sizeof(*tgt));
	tgt->path = tgt_path;
}

static int index_entry_dup(
	git_index_entry **out,
	git_index *index,
	const git_index_entry *src)
{
	if (index_entry_create(out, INDEX_OWNER(index), src->path, NULL, false) < 0)
		return -1;

	index_entry_cpy(*out, src);
	return 0;
}

static void index_entry_cpy_nocache(
	git_index_entry *tgt,
	const git_index_entry *src)
{
	git_oid_cpy(&tgt->id, &src->id);
	tgt->mode = src->mode;
	tgt->flags = src->flags;
	tgt->flags_extended = (src->flags_extended & GIT_INDEX_ENTRY_EXTENDED_FLAGS);
}

static int index_entry_dup_nocache(
	git_index_entry **out,
	git_index *index,
	const git_index_entry *src)
{
	if (index_entry_create(out, INDEX_OWNER(index), src->path, NULL, false) < 0)
		return -1;

	index_entry_cpy_nocache(*out, src);
	return 0;
}

static int has_file_name(git_index *index,
	 const git_index_entry *entry, size_t pos, int ok_to_replace)
{
	size_t len = strlen(entry->path);
	int stage = GIT_INDEX_ENTRY_STAGE(entry);
	const char *name = entry->path;

	while (pos < index->entries.length) {
		struct entry_internal *p = index->entries.contents[pos++];

		if (len >= p->pathlen)
			break;
		if (memcmp(name, p->path, len))
			break;
		if (GIT_INDEX_ENTRY_STAGE(&p->entry) != stage)
			continue;
		if (p->path[len] != '/')
			continue;
		if (!ok_to_replace)
			return -1;

		if (index_remove_entry(index, --pos) < 0)
			break;
	}
	return 0;
}

/*
 * Do we have another file with a pathname that is a proper
 * subset of the name we're trying to add?
 */
static int has_dir_name(git_index *index,
		const git_index_entry *entry, int ok_to_replace)
{
	int stage = GIT_INDEX_ENTRY_STAGE(entry);
	const char *name = entry->path;
	const char *slash = name + strlen(name);

	for (;;) {
		size_t len, pos;

		for (;;) {
			slash--;

			if (slash <= entry->path)
				return 0;

			if (*slash == '/')
				break;
		}
		len = slash - name;

		if (!index_find(&pos, index, name, len, stage)) {
			if (!ok_to_replace)
				return -1;

			if (index_remove_entry(index, pos) < 0)
				break;
			continue;
		}

		/*
		 * Trivial optimization: if we find an entry that
		 * already matches the sub-directory, then we know
		 * we're ok, and we can exit.
		 */
		for (; pos < index->entries.length; ++pos) {
			struct entry_internal *p = index->entries.contents[pos];

			if (p->pathlen <= len ||
			    p->path[len] != '/' ||
			    memcmp(p->path, name, len))
				break; /* not our subdirectory */

			if (GIT_INDEX_ENTRY_STAGE(&p->entry) == stage)
				return 0;
		}
	}

	return 0;
}

static int check_file_directory_collision(git_index *index,
		git_index_entry *entry, size_t pos, int ok_to_replace)
{
	if (has_file_name(index, entry, pos, ok_to_replace) < 0 ||
	    has_dir_name(index, entry, ok_to_replace) < 0) {
		git_error_set(GIT_ERROR_INDEX,
			"'%s' appears as both a file and a directory", entry->path);
		return -1;
	}

	return 0;
}

static int canonicalize_directory_path(
	git_index *index,
	git_index_entry *entry,
	git_index_entry *existing)
{
	const git_index_entry *match, *best = NULL;
	char *search, *sep;
	size_t pos, search_len, best_len;

	if (!index->ignore_case)
		return 0;

	/* item already exists in the index, simply re-use the existing case */
	if (existing) {
		memcpy((char *)entry->path, existing->path, strlen(existing->path));
		return 0;
	}

	/* nothing to do */
	if (strchr(entry->path, '/') == NULL)
		return 0;

	if ((search = git__strdup(entry->path)) == NULL)
		return -1;

	/* starting at the parent directory and descending to the root, find the
	 * common parent directory.
	 */
	while (!best && (sep = strrchr(search, '/'))) {
		sep[1] = '\0';

		search_len = strlen(search);

		git_vector_bsearch2(
			&pos, &index->entries, index->entries_search_path, search);

		while ((match = git_vector_get(&index->entries, pos))) {
			if (GIT_INDEX_ENTRY_STAGE(match) != 0) {
				/* conflicts do not contribute to canonical paths */
			} else if (strncmp(search, match->path, search_len) == 0) {
				/* prefer an exact match to the input filename */
				best = match;
				best_len = search_len;
				break;
			} else if (strncasecmp(search, match->path, search_len) == 0) {
				/* continue walking, there may be a path with an exact
				 * (case sensitive) match later in the index, but use this
				 * as the best match until that happens.
				 */
				if (!best) {
					best = match;
					best_len = search_len;
				}
			} else {
				break;
			}

			pos++;
		}

		sep[0] = '\0';
	}

	if (best)
		memcpy((char *)entry->path, best->path, best_len);

	git__free(search);
	return 0;
}

static int index_no_dups(void **old, void *new)
{
	const git_index_entry *entry = new;
	GIT_UNUSED(old);
	git_error_set(GIT_ERROR_INDEX, "'%s' appears multiple times at stage %d",
		entry->path, GIT_INDEX_ENTRY_STAGE(entry));
	return GIT_EEXISTS;
}

static void index_existing_and_best(
	git_index_entry **existing,
	size_t *existing_position,
	git_index_entry **best,
	git_index *index,
	const git_index_entry *entry)
{
	git_index_entry *e;
	size_t pos;
	int error;

	error = index_find(&pos,
		index, entry->path, 0, GIT_INDEX_ENTRY_STAGE(entry));

	if (error == 0) {
		*existing = index->entries.contents[pos];
		*existing_position = pos;
		*best = index->entries.contents[pos];
		return;
	}

	*existing = NULL;
	*existing_position = 0;
	*best = NULL;

	if (GIT_INDEX_ENTRY_STAGE(entry) == 0) {
		for (; pos < index->entries.length; pos++) {
			int (*strcomp)(const char *a, const char *b) =
				index->ignore_case ? git__strcasecmp : git__strcmp;

			e = index->entries.contents[pos];

			if (strcomp(entry->path, e->path) != 0)
				break;

			if (GIT_INDEX_ENTRY_STAGE(e) == GIT_INDEX_STAGE_ANCESTOR) {
				*best = e;
				continue;
			} else {
				*best = e;
				break;
			}
		}
	}
}

/* index_insert takes ownership of the new entry - if it can't insert
 * it, then it will return an error **and also free the entry**.  When
 * it replaces an existing entry, it will update the entry_ptr with the
 * actual entry in the index (and free the passed in one).
 *
 * trust_path is whether we use the given path, or whether (on case
 * insensitive systems only) we try to canonicalize the given path to
 * be within an existing directory.
 *
 * trust_mode is whether we trust the mode in entry_ptr.
 *
 * trust_id is whether we trust the id or it should be validated.
 */
static int index_insert(
	git_index *index,
	git_index_entry **entry_ptr,
	int replace,
	bool trust_path,
	bool trust_mode,
	bool trust_id)
{
	git_index_entry *existing, *best, *entry;
	size_t path_length, position;
	int error;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(entry_ptr);

	entry = *entry_ptr;

	/* Make sure that the path length flag is correct */
	path_length = ((struct entry_internal *)entry)->pathlen;
	index_entry_adjust_namemask(entry, path_length);

	/* This entry is now up-to-date and should not be checked for raciness */
	entry->flags_extended |= GIT_INDEX_ENTRY_UPTODATE;

	git_vector_sort(&index->entries);

	/*
	 * Look if an entry with this path already exists, either staged, or (if
	 * this entry is a regular staged item) as the "ours" side of a conflict.
	 */
	index_existing_and_best(&existing, &position, &best, index, entry);

	/* Update the file mode */
	entry->mode = trust_mode ?
		git_index__create_mode(entry->mode) :
		index_merge_mode(index, best, entry->mode);

	/* Canonicalize the directory name */
	if (!trust_path && (error = canonicalize_directory_path(index, entry, best)) < 0)
		goto out;

	/* Ensure that the given id exists (unless it's a submodule) */
	if (!trust_id && INDEX_OWNER(index) &&
	    (entry->mode & GIT_FILEMODE_COMMIT) != GIT_FILEMODE_COMMIT) {

		if (!git_object__is_valid(INDEX_OWNER(index), &entry->id,
					  git_object__type_from_filemode(entry->mode))) {
			error = -1;
			goto out;
		}
	}

	/* Look for tree / blob name collisions, removing conflicts if requested */
	if ((error = check_file_directory_collision(index, entry, position, replace)) < 0)
		goto out;

	/*
	 * If we are replacing an existing item, overwrite the existing entry
	 * and return it in place of the passed in one.
	 */
	if (existing) {
		if (replace) {
			index_entry_cpy(existing, entry);

			if (trust_path)
				memcpy((char *)existing->path, entry->path, strlen(entry->path));
		}

		index_entry_free(entry);
		*entry_ptr = existing;
	} else {
		/*
		 * If replace is not requested or no existing entry exists, insert
		 * at the sorted position.  (Since we re-sort after each insert to
		 * check for dups, this is actually cheaper in the long run.)
		 */
		if ((error = git_vector_insert_sorted(&index->entries, entry, index_no_dups)) < 0 ||
		    (error = git_index_entrymap_put(&index->entries_map, entry)) < 0)
			goto out;
	}

	index->dirty = 1;

out:
	if (error < 0) {
		index_entry_free(*entry_ptr);
		*entry_ptr = NULL;
	}

	return error;
}

static int index_conflict_to_reuc(git_index *index, const char *path)
{
	const git_index_entry *conflict_entries[3];
	int ancestor_mode, our_mode, their_mode;
	git_oid const *ancestor_oid, *our_oid, *their_oid;
	int ret;

	if ((ret = git_index_conflict_get(&conflict_entries[0],
		&conflict_entries[1], &conflict_entries[2], index, path)) < 0)
		return ret;

	ancestor_mode = conflict_entries[0] == NULL ? 0 : conflict_entries[0]->mode;
	our_mode = conflict_entries[1] == NULL ? 0 : conflict_entries[1]->mode;
	their_mode = conflict_entries[2] == NULL ? 0 : conflict_entries[2]->mode;

	ancestor_oid = conflict_entries[0] == NULL ? NULL : &conflict_entries[0]->id;
	our_oid = conflict_entries[1] == NULL ? NULL : &conflict_entries[1]->id;
	their_oid = conflict_entries[2] == NULL ? NULL : &conflict_entries[2]->id;

	if ((ret = git_index_reuc_add(index, path, ancestor_mode, ancestor_oid,
		our_mode, our_oid, their_mode, their_oid)) >= 0)
		ret = git_index_conflict_remove(index, path);

	return ret;
}

GIT_INLINE(bool) is_file_or_link(const int filemode)
{
	return (filemode == GIT_FILEMODE_BLOB ||
		filemode == GIT_FILEMODE_BLOB_EXECUTABLE ||
		filemode == GIT_FILEMODE_LINK);
}

GIT_INLINE(bool) valid_filemode(const int filemode)
{
	return (is_file_or_link(filemode) || filemode == GIT_FILEMODE_COMMIT);
}

int git_index_add_from_buffer(
    git_index *index, const git_index_entry *source_entry,
    const void *buffer, size_t len)
{
	git_index_entry *entry = NULL;
	int error = 0;
	git_oid id;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(source_entry && source_entry->path);

	if (INDEX_OWNER(index) == NULL)
		return create_index_error(-1,
			"could not initialize index entry. "
			"Index is not backed up by an existing repository.");

	if (!is_file_or_link(source_entry->mode)) {
		git_error_set(GIT_ERROR_INDEX, "invalid filemode");
		return -1;
	}

	if (len > UINT32_MAX) {
		git_error_set(GIT_ERROR_INDEX, "buffer is too large");
		return -1;
	}

	if (index_entry_dup(&entry, index, source_entry) < 0)
		return -1;

	error = git_blob_create_from_buffer(&id, INDEX_OWNER(index), buffer, len);
	if (error < 0) {
		index_entry_free(entry);
		return error;
	}

	git_oid_cpy(&entry->id, &id);
	entry->file_size = (uint32_t)len;

	if ((error = index_insert(index, &entry, 1, true, true, true)) < 0)
		return error;

	/* Adding implies conflict was resolved, move conflict entries to REUC */
	if ((error = index_conflict_to_reuc(index, entry->path)) < 0 && error != GIT_ENOTFOUND)
		return error;

	git_tree_cache_invalidate_path(index->tree, entry->path);
	return 0;
}

static int add_repo_as_submodule(git_index_entry **out, git_index *index, const char *path)
{
	git_repository *sub;
	git_str abspath = GIT_STR_INIT;
	git_repository *repo = INDEX_OWNER(index);
	git_reference *head;
	git_index_entry *entry;
	struct stat st;
	int error;

	if ((error = git_repository_workdir_path(&abspath, repo, path)) < 0)
		return error;

	if ((error = p_stat(abspath.ptr, &st)) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to stat repository dir");
		return -1;
	}

	if (index_entry_create(&entry, INDEX_OWNER(index), path, &st, true) < 0)
		return -1;

	git_index_entry__init_from_stat(entry, &st, !index->distrust_filemode);

	if ((error = git_repository_open(&sub, abspath.ptr)) < 0)
		return error;

	if ((error = git_repository_head(&head, sub)) < 0)
		return error;

	git_oid_cpy(&entry->id, git_reference_target(head));
	entry->mode = GIT_FILEMODE_COMMIT;

	git_reference_free(head);
	git_repository_free(sub);
	git_str_dispose(&abspath);

	*out = entry;
	return 0;
}

int git_index_add_bypath(git_index *index, const char *path)
{
	git_index_entry *entry = NULL;
	int ret;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(path);

	if ((ret = index_entry_init(&entry, index, path)) == 0)
		ret = index_insert(index, &entry, 1, false, false, true);

	/* If we were given a directory, let's see if it's a submodule */
	if (ret < 0 && ret != GIT_EDIRECTORY)
		return ret;

	if (ret == GIT_EDIRECTORY) {
		git_submodule *sm;
		git_error *last_error;

		git_error_save(&last_error);

		ret = git_submodule_lookup(&sm, INDEX_OWNER(index), path);
		if (ret == GIT_ENOTFOUND) {
			git_error_restore(last_error);
			return GIT_EDIRECTORY;
		}

		git_error_free(last_error);

		/*
		 * EEXISTS means that there is a repository at that path, but it's not known
		 * as a submodule. We add its HEAD as an entry and don't register it.
		 */
		if (ret == GIT_EEXISTS) {
			if ((ret = add_repo_as_submodule(&entry, index, path)) < 0)
				return ret;

			if ((ret = index_insert(index, &entry, 1, false, false, true)) < 0)
				return ret;
		} else if (ret < 0) {
			return ret;
		} else {
			ret = git_submodule_add_to_index(sm, false);
			git_submodule_free(sm);
			return ret;
		}
	}

	/* Adding implies conflict was resolved, move conflict entries to REUC */
	if ((ret = index_conflict_to_reuc(index, path)) < 0 && ret != GIT_ENOTFOUND)
		return ret;

	git_tree_cache_invalidate_path(index->tree, entry->path);
	return 0;
}

int git_index_remove_bypath(git_index *index, const char *path)
{
	int ret;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(path);

	if (((ret = git_index_remove(index, path, 0)) < 0 &&
		ret != GIT_ENOTFOUND) ||
		((ret = index_conflict_to_reuc(index, path)) < 0 &&
		ret != GIT_ENOTFOUND))
		return ret;

	if (ret == GIT_ENOTFOUND)
		git_error_clear();

	return 0;
}

int git_index__fill(git_index *index, const git_vector *source_entries)
{
	const git_index_entry *source_entry = NULL;
	int error = 0;
	size_t i;

	GIT_ASSERT_ARG(index);

	if (!source_entries->length)
		return 0;

	if (git_vector_size_hint(&index->entries, source_entries->length) < 0 ||
	    git_index_entrymap_resize(&index->entries_map, (size_t)(source_entries->length * 1.3)) < 0)
		return -1;

	git_vector_foreach(source_entries, i, source_entry) {
		git_index_entry *entry = NULL;

		if ((error = index_entry_dup(&entry, index, source_entry)) < 0)
			break;

		index_entry_adjust_namemask(entry, ((struct entry_internal *)entry)->pathlen);
		entry->flags_extended |= GIT_INDEX_ENTRY_UPTODATE;
		entry->mode = git_index__create_mode(entry->mode);

		if ((error = git_vector_insert(&index->entries, entry)) < 0 ||
		    (error = git_index_entrymap_put(&index->entries_map, entry)) < 0)
			break;

		index->dirty = 1;
	}

	if (!error)
		git_vector_sort(&index->entries);

	return error;
}


int git_index_add(git_index *index, const git_index_entry *source_entry)
{
	git_index_entry *entry = NULL;
	int ret;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(source_entry && source_entry->path);

	if (!valid_filemode(source_entry->mode)) {
		git_error_set(GIT_ERROR_INDEX, "invalid entry mode");
		return -1;
	}

	if ((ret = index_entry_dup(&entry, index, source_entry)) < 0 ||
		(ret = index_insert(index, &entry, 1, true, true, false)) < 0)
		return ret;

	git_tree_cache_invalidate_path(index->tree, entry->path);
	return 0;
}

int git_index_remove(git_index *index, const char *path, int stage)
{
	int error;
	size_t position;
	git_index_entry remove_key = {{ 0 }};

	remove_key.path = path;
	GIT_INDEX_ENTRY_STAGE_SET(&remove_key, stage);

	git_index_entrymap_remove(&index->entries_map, &remove_key);

	if (index_find(&position, index, path, 0, stage) < 0) {
		git_error_set(
			GIT_ERROR_INDEX, "index does not contain %s at stage %d", path, stage);
		error = GIT_ENOTFOUND;
	} else {
		error = index_remove_entry(index, position);
	}

	return error;
}

int git_index_remove_directory(git_index *index, const char *dir, int stage)
{
	git_str pfx = GIT_STR_INIT;
	int error = 0;
	size_t pos;
	git_index_entry *entry;

	if (!(error = git_str_sets(&pfx, dir)) &&
		!(error = git_fs_path_to_dir(&pfx)))
		index_find(&pos, index, pfx.ptr, pfx.size, GIT_INDEX_STAGE_ANY);

	while (!error) {
		entry = git_vector_get(&index->entries, pos);
		if (!entry || git__prefixcmp(entry->path, pfx.ptr) != 0)
			break;

		if (GIT_INDEX_ENTRY_STAGE(entry) != stage) {
			++pos;
			continue;
		}

		error = index_remove_entry(index, pos);

		/* removed entry at 'pos' so we don't need to increment */
	}

	git_str_dispose(&pfx);

	return error;
}

int git_index_find_prefix(size_t *at_pos, git_index *index, const char *prefix)
{
	int error = 0;
	size_t pos;
	const git_index_entry *entry;

	index_find(&pos, index, prefix, strlen(prefix), GIT_INDEX_STAGE_ANY);
	entry = git_vector_get(&index->entries, pos);
	if (!entry || git__prefixcmp(entry->path, prefix) != 0)
		error = GIT_ENOTFOUND;

	if (!error && at_pos)
		*at_pos = pos;

	return error;
}

int git_index__find_pos(
	size_t *out, git_index *index, const char *path, size_t path_len, int stage)
{
	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(path);
	return index_find(out, index, path, path_len, stage);
}

int git_index_find(size_t *at_pos, git_index *index, const char *path)
{
	size_t pos;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(path);

	if (git_vector_bsearch2(
			&pos, &index->entries, index->entries_search_path, path) < 0) {
		git_error_set(GIT_ERROR_INDEX, "index does not contain %s", path);
		return GIT_ENOTFOUND;
	}

	/* Since our binary search only looked at path, we may be in the
	 * middle of a list of stages.
	 */
	for (; pos > 0; --pos) {
		const git_index_entry *prev = git_vector_get(&index->entries, pos - 1);

		if (index->entries_cmp_path(prev->path, path) != 0)
			break;
	}

	if (at_pos)
		*at_pos = pos;

	return 0;
}

int git_index_conflict_add(git_index *index,
	const git_index_entry *ancestor_entry,
	const git_index_entry *our_entry,
	const git_index_entry *their_entry)
{
	git_index_entry *entries[3] = { 0 };
	unsigned short i;
	int ret = 0;

	GIT_ASSERT_ARG(index);

	if ((ancestor_entry &&
			(ret = index_entry_dup(&entries[0], index, ancestor_entry)) < 0) ||
		(our_entry &&
			(ret = index_entry_dup(&entries[1], index, our_entry)) < 0) ||
		(their_entry &&
			(ret = index_entry_dup(&entries[2], index, their_entry)) < 0))
		goto on_error;

	/* Validate entries */
	for (i = 0; i < 3; i++) {
		if (entries[i] && !valid_filemode(entries[i]->mode)) {
			git_error_set(GIT_ERROR_INDEX, "invalid filemode for stage %d entry",
				i + 1);
			ret = -1;
			goto on_error;
		}
	}

	/* Remove existing index entries for each path */
	for (i = 0; i < 3; i++) {
		if (entries[i] == NULL)
			continue;

		if ((ret = git_index_remove(index, entries[i]->path, 0)) != 0) {
			if (ret != GIT_ENOTFOUND)
				goto on_error;

			git_error_clear();
			ret = 0;
		}
	}

	/* Add the conflict entries */
	for (i = 0; i < 3; i++) {
		if (entries[i] == NULL)
			continue;

		/* Make sure stage is correct */
		GIT_INDEX_ENTRY_STAGE_SET(entries[i], i + 1);

		if ((ret = index_insert(index, &entries[i], 1, true, true, false)) < 0)
			goto on_error;

		entries[i] = NULL; /* don't free if later entry fails */
	}

	return 0;

on_error:
	for (i = 0; i < 3; i++) {
		if (entries[i] != NULL)
			index_entry_free(entries[i]);
	}

	return ret;
}

static int index_conflict__get_byindex(
	const git_index_entry **ancestor_out,
	const git_index_entry **our_out,
	const git_index_entry **their_out,
	git_index *index,
	size_t n)
{
	const git_index_entry *conflict_entry;
	const char *path = NULL;
	size_t count;
	int stage, len = 0;

	GIT_ASSERT_ARG(ancestor_out);
	GIT_ASSERT_ARG(our_out);
	GIT_ASSERT_ARG(their_out);
	GIT_ASSERT_ARG(index);

	*ancestor_out = NULL;
	*our_out = NULL;
	*their_out = NULL;

	for (count = git_index_entrycount(index); n < count; ++n) {
		conflict_entry = git_vector_get(&index->entries, n);

		if (path && index->entries_cmp_path(conflict_entry->path, path) != 0)
			break;

		stage = GIT_INDEX_ENTRY_STAGE(conflict_entry);
		path = conflict_entry->path;

		switch (stage) {
		case 3:
			*their_out = conflict_entry;
			len++;
			break;
		case 2:
			*our_out = conflict_entry;
			len++;
			break;
		case 1:
			*ancestor_out = conflict_entry;
			len++;
			break;
		default:
			break;
		};
	}

	return len;
}

int git_index_conflict_get(
	const git_index_entry **ancestor_out,
	const git_index_entry **our_out,
	const git_index_entry **their_out,
	git_index *index,
	const char *path)
{
	size_t pos;
	int len = 0;

	GIT_ASSERT_ARG(ancestor_out);
	GIT_ASSERT_ARG(our_out);
	GIT_ASSERT_ARG(their_out);
	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(path);

	*ancestor_out = NULL;
	*our_out = NULL;
	*their_out = NULL;

	if (git_index_find(&pos, index, path) < 0)
		return GIT_ENOTFOUND;

	if ((len = index_conflict__get_byindex(
		ancestor_out, our_out, their_out, index, pos)) < 0)
		return len;
	else if (len == 0)
		return GIT_ENOTFOUND;

	return 0;
}

static int index_conflict_remove(git_index *index, const char *path)
{
	size_t pos = 0;
	git_index_entry *conflict_entry;
	int error = 0;

	if (path != NULL && git_index_find(&pos, index, path) < 0)
		return GIT_ENOTFOUND;

	while ((conflict_entry = git_vector_get(&index->entries, pos)) != NULL) {

		if (path != NULL &&
			index->entries_cmp_path(conflict_entry->path, path) != 0)
			break;

		if (GIT_INDEX_ENTRY_STAGE(conflict_entry) == 0) {
			pos++;
			continue;
		}

		if ((error = index_remove_entry(index, pos)) < 0)
			break;
	}

	return error;
}

int git_index_conflict_remove(git_index *index, const char *path)
{
	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(path);
	return index_conflict_remove(index, path);
}

int git_index_conflict_cleanup(git_index *index)
{
	GIT_ASSERT_ARG(index);
	return index_conflict_remove(index, NULL);
}

int git_index_has_conflicts(const git_index *index)
{
	size_t i;
	git_index_entry *entry;

	GIT_ASSERT_ARG(index);

	git_vector_foreach(&index->entries, i, entry) {
		if (GIT_INDEX_ENTRY_STAGE(entry) > 0)
			return 1;
	}

	return 0;
}

int git_index_iterator_new(
	git_index_iterator **iterator_out,
	git_index *index)
{
	git_index_iterator *it;
	int error;

	GIT_ASSERT_ARG(iterator_out);
	GIT_ASSERT_ARG(index);

	it = git__calloc(1, sizeof(git_index_iterator));
	GIT_ERROR_CHECK_ALLOC(it);

	if ((error = git_index_snapshot_new(&it->snap, index)) < 0) {
		git__free(it);
		return error;
	}

	it->index = index;

	*iterator_out = it;
	return 0;
}

int git_index_iterator_next(
	const git_index_entry **out,
	git_index_iterator *it)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(it);

	if (it->cur >= git_vector_length(&it->snap))
		return GIT_ITEROVER;

	*out = (git_index_entry *)git_vector_get(&it->snap, it->cur++);
	return 0;
}

void git_index_iterator_free(git_index_iterator *it)
{
	if (it == NULL)
		return;

	git_index_snapshot_release(&it->snap, it->index);
	git__free(it);
}

int git_index_conflict_iterator_new(
	git_index_conflict_iterator **iterator_out,
	git_index *index)
{
	git_index_conflict_iterator *it = NULL;

	GIT_ASSERT_ARG(iterator_out);
	GIT_ASSERT_ARG(index);

	it = git__calloc(1, sizeof(git_index_conflict_iterator));
	GIT_ERROR_CHECK_ALLOC(it);

	it->index = index;

	*iterator_out = it;
	return 0;
}

int git_index_conflict_next(
	const git_index_entry **ancestor_out,
	const git_index_entry **our_out,
	const git_index_entry **their_out,
	git_index_conflict_iterator *iterator)
{
	const git_index_entry *entry;
	int len;

	GIT_ASSERT_ARG(ancestor_out);
	GIT_ASSERT_ARG(our_out);
	GIT_ASSERT_ARG(their_out);
	GIT_ASSERT_ARG(iterator);

	*ancestor_out = NULL;
	*our_out = NULL;
	*their_out = NULL;

	while (iterator->cur < iterator->index->entries.length) {
		entry = git_index_get_byindex(iterator->index, iterator->cur);

		if (git_index_entry_is_conflict(entry)) {
			if ((len = index_conflict__get_byindex(
				ancestor_out,
				our_out,
				their_out,
				iterator->index,
				iterator->cur)) < 0)
				return len;

			iterator->cur += len;
			return 0;
		}

		iterator->cur++;
	}

	return GIT_ITEROVER;
}

void git_index_conflict_iterator_free(git_index_conflict_iterator *iterator)
{
	if (iterator == NULL)
		return;

	git__free(iterator);
}

size_t git_index_name_entrycount(git_index *index)
{
	GIT_ASSERT_ARG(index);
	return index->names.length;
}

const git_index_name_entry *git_index_name_get_byindex(
	git_index *index, size_t n)
{
	GIT_ASSERT_ARG_WITH_RETVAL(index, NULL);

	git_vector_sort(&index->names);
	return git_vector_get(&index->names, n);
}

static void index_name_entry_free(git_index_name_entry *ne)
{
	if (!ne)
		return;
	git__free(ne->ancestor);
	git__free(ne->ours);
	git__free(ne->theirs);
	git__free(ne);
}

int git_index_name_add(git_index *index,
	const char *ancestor, const char *ours, const char *theirs)
{
	git_index_name_entry *conflict_name;

	GIT_ASSERT_ARG((ancestor && ours) || (ancestor && theirs) || (ours && theirs));

	conflict_name = git__calloc(1, sizeof(git_index_name_entry));
	GIT_ERROR_CHECK_ALLOC(conflict_name);

	if ((ancestor && !(conflict_name->ancestor = git__strdup(ancestor))) ||
		(ours     && !(conflict_name->ours     = git__strdup(ours))) ||
		(theirs   && !(conflict_name->theirs   = git__strdup(theirs))) ||
		git_vector_insert(&index->names, conflict_name) < 0)
	{
		index_name_entry_free(conflict_name);
		return -1;
	}

	index->dirty = 1;
	return 0;
}

int git_index_name_clear(git_index *index)
{
	size_t i;
	git_index_name_entry *conflict_name;

	GIT_ASSERT_ARG(index);

	git_vector_foreach(&index->names, i, conflict_name)
		index_name_entry_free(conflict_name);

	git_vector_clear(&index->names);

	index->dirty = 1;

	return 0;
}

size_t git_index_reuc_entrycount(git_index *index)
{
	GIT_ASSERT_ARG(index);
	return index->reuc.length;
}

static int index_reuc_on_dup(void **old, void *new)
{
	index_entry_reuc_free(*old);
	*old = new;
	return GIT_EEXISTS;
}

static int index_reuc_insert(
	git_index *index,
	git_index_reuc_entry *reuc)
{
	int res;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(reuc && reuc->path != NULL);
	GIT_ASSERT(git_vector_is_sorted(&index->reuc));

	res = git_vector_insert_sorted(&index->reuc, reuc, &index_reuc_on_dup);
	index->dirty = 1;

	return res == GIT_EEXISTS ? 0 : res;
}

int git_index_reuc_add(git_index *index, const char *path,
	int ancestor_mode, const git_oid *ancestor_oid,
	int our_mode, const git_oid *our_oid,
	int their_mode, const git_oid *their_oid)
{
	git_index_reuc_entry *reuc = NULL;
	int error = 0;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(path);

	if ((error = index_entry_reuc_init(&reuc, path, ancestor_mode,
			ancestor_oid, our_mode, our_oid, their_mode, their_oid)) < 0 ||
		(error = index_reuc_insert(index, reuc)) < 0)
		index_entry_reuc_free(reuc);

	return error;
}

int git_index_reuc_find(size_t *at_pos, git_index *index, const char *path)
{
	return git_vector_bsearch2(at_pos, &index->reuc, index->reuc_search, path);
}

const git_index_reuc_entry *git_index_reuc_get_bypath(
	git_index *index, const char *path)
{
	size_t pos;

	GIT_ASSERT_ARG_WITH_RETVAL(index, NULL);
	GIT_ASSERT_ARG_WITH_RETVAL(path, NULL);

	if (!index->reuc.length)
		return NULL;

	GIT_ASSERT_WITH_RETVAL(git_vector_is_sorted(&index->reuc), NULL);

	if (git_index_reuc_find(&pos, index, path) < 0)
		return NULL;

	return git_vector_get(&index->reuc, pos);
}

const git_index_reuc_entry *git_index_reuc_get_byindex(
	git_index *index, size_t n)
{
	GIT_ASSERT_ARG_WITH_RETVAL(index, NULL);
	GIT_ASSERT_WITH_RETVAL(git_vector_is_sorted(&index->reuc), NULL);

	return git_vector_get(&index->reuc, n);
}

int git_index_reuc_remove(git_index *index, size_t position)
{
	int error;
	git_index_reuc_entry *reuc;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT(git_vector_is_sorted(&index->reuc));

	reuc = git_vector_get(&index->reuc, position);
	error = git_vector_remove(&index->reuc, position);

	if (!error)
		index_entry_reuc_free(reuc);

	index->dirty = 1;
	return error;
}

int git_index_reuc_clear(git_index *index)
{
	size_t i;

	GIT_ASSERT_ARG(index);

	for (i = 0; i < index->reuc.length; ++i)
		index_entry_reuc_free(git_atomic_swap(index->reuc.contents[i], NULL));

	git_vector_clear(&index->reuc);

	index->dirty = 1;

	return 0;
}

static int index_error_invalid(const char *message)
{
	git_error_set(GIT_ERROR_INDEX, "invalid data in index - %s", message);
	return -1;
}

static int read_reuc(git_index *index, const char *buffer, size_t size)
{
	const char *endptr;
	size_t oid_size = git_oid_size(index->oid_type);
	size_t len;
	int i;

	/* If called multiple times, the vector might already be initialized */
	if (index->reuc._alloc_size == 0 &&
		git_vector_init(&index->reuc, 16, reuc_cmp) < 0)
		return -1;

	while (size) {
		git_index_reuc_entry *lost;

		len = p_strnlen(buffer, size) + 1;
		if (size <= len)
			return index_error_invalid("reading reuc entries");

		lost = reuc_entry_alloc(buffer);
		GIT_ERROR_CHECK_ALLOC(lost);

		size -= len;
		buffer += len;

		/* read 3 ASCII octal numbers for stage entries */
		for (i = 0; i < 3; i++) {
			int64_t tmp;

			if (git__strntol64(&tmp, buffer, size, &endptr, 8) < 0 ||
				!endptr || endptr == buffer || *endptr ||
				tmp < 0 || tmp > UINT32_MAX) {
				index_entry_reuc_free(lost);
				return index_error_invalid("reading reuc entry stage");
			}

			lost->mode[i] = (uint32_t)tmp;

			len = (endptr + 1) - buffer;
			if (size <= len) {
				index_entry_reuc_free(lost);
				return index_error_invalid("reading reuc entry stage");
			}

			size -= len;
			buffer += len;
		}

		/* read up to 3 OIDs for stage entries */
		for (i = 0; i < 3; i++) {
			if (!lost->mode[i])
				continue;
			if (size < oid_size) {
				index_entry_reuc_free(lost);
				return index_error_invalid("reading reuc entry oid");
			}

			if (git_oid__fromraw(&lost->oid[i], (const unsigned char *) buffer, index->oid_type) < 0)
				return -1;

			size -= oid_size;
			buffer += oid_size;
		}

		/* entry was read successfully - insert into reuc vector */
		if (git_vector_insert(&index->reuc, lost) < 0)
			return -1;
	}

	/* entries are guaranteed to be sorted on-disk */
	git_vector_set_sorted(&index->reuc, true);

	return 0;
}


static int read_conflict_names(git_index *index, const char *buffer, size_t size)
{
	size_t len;

	/* This gets called multiple times, the vector might already be initialized */
	if (index->names._alloc_size == 0 &&
		git_vector_init(&index->names, 16, conflict_name_cmp) < 0)
		return -1;

#define read_conflict_name(ptr) \
	len = p_strnlen(buffer, size) + 1; \
	if (size < len) { \
		index_error_invalid("reading conflict name entries"); \
		goto out_err; \
	} \
	if (len == 1) \
		ptr = NULL; \
	else { \
		ptr = git__malloc(len); \
		GIT_ERROR_CHECK_ALLOC(ptr); \
		memcpy(ptr, buffer, len); \
	} \
	\
	buffer += len; \
	size -= len;

	while (size) {
		git_index_name_entry *conflict_name = git__calloc(1, sizeof(git_index_name_entry));
		GIT_ERROR_CHECK_ALLOC(conflict_name);

		read_conflict_name(conflict_name->ancestor);
		read_conflict_name(conflict_name->ours);
		read_conflict_name(conflict_name->theirs);

		if (git_vector_insert(&index->names, conflict_name) < 0)
			goto out_err;

		continue;

out_err:
		git__free(conflict_name->ancestor);
		git__free(conflict_name->ours);
		git__free(conflict_name->theirs);
		git__free(conflict_name);
		return -1;
	}

#undef read_conflict_name

	/* entries are guaranteed to be sorted on-disk */
	git_vector_set_sorted(&index->names, true);

	return 0;
}

GIT_INLINE(size_t) index_entry_path_offset(
	git_oid_t oid_type,
	uint32_t flags)
{
	if (oid_type == GIT_OID_SHA1)
		return (flags & GIT_INDEX_ENTRY_EXTENDED) ?
			offsetof(index_entry_long_sha1, path) :
			offsetof(index_entry_short_sha1, path);

#ifdef GIT_EXPERIMENTAL_SHA256
	else if (oid_type == GIT_OID_SHA256)
		return (flags & GIT_INDEX_ENTRY_EXTENDED) ?
			offsetof(index_entry_long_sha256, path) :
			offsetof(index_entry_short_sha256, path);
#endif

	git_error_set(GIT_ERROR_INTERNAL, "invalid oid type");
	return 0;
}

GIT_INLINE(size_t) index_entry_flags_offset(git_oid_t oid_type)
{
	if (oid_type == GIT_OID_SHA1)
		return offsetof(index_entry_long_sha1, flags_extended);

#ifdef GIT_EXPERIMENTAL_SHA256
	else if (oid_type == GIT_OID_SHA256)
		return offsetof(index_entry_long_sha256, flags_extended);
#endif

	git_error_set(GIT_ERROR_INTERNAL, "invalid oid type");
	return 0;
}

static size_t index_entry_size(
	size_t path_len,
	size_t varint_len,
	git_oid_t oid_type,
	uint32_t flags)
{
	size_t offset, size;

	if (!(offset = index_entry_path_offset(oid_type, flags)))
		return 0;

	if (varint_len) {
		if (GIT_ADD_SIZET_OVERFLOW(&size, offset, path_len) ||
		    GIT_ADD_SIZET_OVERFLOW(&size, size, 1) ||
		    GIT_ADD_SIZET_OVERFLOW(&size, size, varint_len))
			return 0;
	} else {
		if (GIT_ADD_SIZET_OVERFLOW(&size, offset, path_len) ||
		    GIT_ADD_SIZET_OVERFLOW(&size, size, 8))
			return 0;

		size &= ~7;
	}

	return size;
}

static int read_entry(
	git_index_entry **out,
	size_t *out_size,
	git_index *index,
	size_t checksum_size,
	const void *buffer,
	size_t buffer_size,
	const char *last)
{
	size_t path_length, path_offset, entry_size;
	const char *path_ptr;
	struct entry_common *source_common;
	index_entry_short_sha1 source_sha1;
#ifdef GIT_EXPERIMENTAL_SHA256
	index_entry_short_sha256 source_sha256;
#endif
	git_index_entry entry = {{0}};
	bool compressed = index->version >= INDEX_VERSION_NUMBER_COMP;
	char *tmp_path = NULL;

	size_t minimal_entry_size = index_entry_path_offset(index->oid_type, 0);

	if (checksum_size + minimal_entry_size > buffer_size)
		return -1;

	/* buffer is not guaranteed to be aligned */
	switch (index->oid_type) {
	case GIT_OID_SHA1:
		source_common = &source_sha1.common;
		memcpy(&source_sha1, buffer, sizeof(source_sha1));
		break;
#ifdef GIT_EXPERIMENTAL_SHA256
	case GIT_OID_SHA256:
		source_common = &source_sha256.common;
		memcpy(&source_sha256, buffer, sizeof(source_sha256));
		break;
#endif
	default:
		GIT_ASSERT(!"invalid oid type");
	}

	entry.ctime.seconds = (git_time_t)ntohl(source_common->ctime.seconds);
	entry.ctime.nanoseconds = ntohl(source_common->ctime.nanoseconds);
	entry.mtime.seconds = (git_time_t)ntohl(source_common->mtime.seconds);
	entry.mtime.nanoseconds = ntohl(source_common->mtime.nanoseconds);
	entry.dev = ntohl(source_common->dev);
	entry.ino = ntohl(source_common->ino);
	entry.mode = ntohl(source_common->mode);
	entry.uid = ntohl(source_common->uid);
	entry.gid = ntohl(source_common->gid);
	entry.file_size = ntohl(source_common->file_size);

	switch (index->oid_type) {
	case GIT_OID_SHA1:
		if (git_oid__fromraw(&entry.id, source_sha1.oid,
		                     GIT_OID_SHA1) < 0)
			return -1;
		entry.flags = ntohs(source_sha1.flags);
		break;
#ifdef GIT_EXPERIMENTAL_SHA256
	case GIT_OID_SHA256:
		if (git_oid__fromraw(&entry.id, source_sha256.oid,
		                     GIT_OID_SHA256) < 0)
			return -1;
		entry.flags = ntohs(source_sha256.flags);
		break;
#endif
	default:
		GIT_ASSERT(!"invalid oid type");
	}

	if (!(path_offset = index_entry_path_offset(index->oid_type, entry.flags)))
		return -1;


	if (entry.flags & GIT_INDEX_ENTRY_EXTENDED) {
		uint16_t flags_raw;
		size_t flags_offset;

		if (!(flags_offset = index_entry_flags_offset(index->oid_type)))
			return -1;

		memcpy(&flags_raw, (const char *)buffer + flags_offset, sizeof(flags_raw));
		flags_raw = ntohs(flags_raw);

		memcpy(&entry.flags_extended, &flags_raw, sizeof(flags_raw));
		path_ptr = (const char *)buffer + path_offset;
	} else {
		path_ptr = (const char *)buffer + path_offset;
	}

	if (!compressed) {
		path_length = entry.flags & GIT_INDEX_ENTRY_NAMEMASK;

		/* if this is a very long string, we must find its
		 * real length without overflowing */
		if (path_length == 0xFFF) {
			const char *path_end;

			path_end = memchr(path_ptr, '\0', buffer_size);
			if (path_end == NULL)
				return index_error_invalid("invalid path name");

			path_length = path_end - path_ptr;
		}

		entry_size = index_entry_size(path_length, 0, index->oid_type, entry.flags);
		entry.path = (char *)path_ptr;
	} else {
		size_t varint_len, last_len, prefix_len, suffix_len, path_len;
		uintmax_t strip_len;

		strip_len = git_decode_varint((const unsigned char *)path_ptr, &varint_len);
		last_len = strlen(last);

		if (varint_len == 0 || last_len < strip_len)
			return index_error_invalid("incorrect prefix length");

		prefix_len = last_len - (size_t)strip_len;
		suffix_len = strlen(path_ptr + varint_len);

		GIT_ERROR_CHECK_ALLOC_ADD(&path_len, prefix_len, suffix_len);
		GIT_ERROR_CHECK_ALLOC_ADD(&path_len, path_len, 1);

		if (path_len > GIT_PATH_MAX)
			return index_error_invalid("unreasonable path length");

		tmp_path = git__malloc(path_len);
		GIT_ERROR_CHECK_ALLOC(tmp_path);

		memcpy(tmp_path, last, prefix_len);
		memcpy(tmp_path + prefix_len, path_ptr + varint_len, suffix_len + 1);

		entry_size = index_entry_size(suffix_len, varint_len, index->oid_type, entry.flags);
		entry.path = tmp_path;
	}

	if (entry_size == 0)
		return -1;

	if (checksum_size + entry_size > buffer_size) {
		git_error_set(GIT_ERROR_INTERNAL, "invalid index checksum");
		return -1;
	}

	if (index_entry_dup(out, index, &entry) < 0) {
		git__free(tmp_path);
		return -1;
	}

	git__free(tmp_path);
	*out_size = entry_size;
	return 0;
}

static int read_header(struct index_header *dest, const void *buffer)
{
	const struct index_header *source = buffer;

	dest->signature = ntohl(source->signature);
	if (dest->signature != INDEX_HEADER_SIG)
		return index_error_invalid("incorrect header signature");

	dest->version = ntohl(source->version);
	if (dest->version < INDEX_VERSION_NUMBER_LB ||
		dest->version > INDEX_VERSION_NUMBER_UB)
		return index_error_invalid("incorrect header version");

	dest->entry_count = ntohl(source->entry_count);
	return 0;
}

static int read_extension(size_t *read_len, git_index *index, size_t checksum_size, const char *buffer, size_t buffer_size)
{
	struct index_extension dest;
	size_t total_size;

	/* buffer is not guaranteed to be aligned */
	memcpy(&dest, buffer, sizeof(struct index_extension));
	dest.extension_size = ntohl(dest.extension_size);

	total_size = dest.extension_size + sizeof(struct index_extension);

	if (dest.extension_size > total_size ||
		buffer_size < total_size ||
		buffer_size - total_size < checksum_size) {
		index_error_invalid("extension is truncated");
		return -1;
	}

	/* optional extension */
	if (dest.signature[0] >= 'A' && dest.signature[0] <= 'Z') {
		/* tree cache */
		if (memcmp(dest.signature, INDEX_EXT_TREECACHE_SIG, 4) == 0) {
			if (git_tree_cache_read(&index->tree, buffer + 8, dest.extension_size, index->oid_type, &index->tree_pool) < 0)
				return -1;
		} else if (memcmp(dest.signature, INDEX_EXT_UNMERGED_SIG, 4) == 0) {
			if (read_reuc(index, buffer + 8, dest.extension_size) < 0)
				return -1;
		} else if (memcmp(dest.signature, INDEX_EXT_CONFLICT_NAME_SIG, 4) == 0) {
			if (read_conflict_names(index, buffer + 8, dest.extension_size) < 0)
				return -1;
		}
		/* else, unsupported extension. We cannot parse this, but we can skip
		 * it by returning `total_size */
	} else {
		/* we cannot handle non-ignorable extensions;
		 * in fact they aren't even defined in the standard */
		git_error_set(GIT_ERROR_INDEX, "unsupported mandatory extension: '%.4s'", dest.signature);
		return -1;
	}

	*read_len = total_size;

	return 0;
}

static int parse_index(git_index *index, const char *buffer, size_t buffer_size)
{
	int error = 0;
	unsigned int i;
	struct index_header header = { 0 };
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	unsigned char zero_checksum[GIT_HASH_MAX_SIZE] = { 0 };
	size_t checksum_size = git_hash_size(git_oid_algorithm(index->oid_type));
	const char *last = NULL;
	const char *empty = "";

#define seek_forward(_increase) { \
	if (_increase >= buffer_size) { \
		error = index_error_invalid("ran out of data while parsing"); \
		goto done; } \
	buffer += _increase; \
	buffer_size -= _increase;\
}

	if (buffer_size < INDEX_HEADER_SIZE + checksum_size)
		return index_error_invalid("insufficient buffer space");

	/*
	 * Precalculate the hash of the files's contents -- we'll match
	 * it to the provided checksum in the footer.
	 */
	git_hash_buf(checksum, buffer, buffer_size - checksum_size,
		git_oid_algorithm(index->oid_type));

	/* Parse header */
	if ((error = read_header(&header, buffer)) < 0)
		return error;

	index->version = header.version;
	if (index->version >= INDEX_VERSION_NUMBER_COMP)
		last = empty;

	seek_forward(INDEX_HEADER_SIZE);

	GIT_ASSERT(!index->entries.length);

	if ((error = git_index_entrymap_resize(&index->entries_map, header.entry_count)) < 0)
		return error;

	/* Parse all the entries */
	for (i = 0; i < header.entry_count && buffer_size > checksum_size; ++i) {
		git_index_entry *entry = NULL;
		size_t entry_size;

		if ((error = read_entry(&entry, &entry_size, index, checksum_size, buffer, buffer_size, last)) < 0) {
			error = index_error_invalid("invalid entry");
			goto done;
		}

		if ((error = git_vector_insert(&index->entries, entry)) < 0) {
			index_entry_free(entry);
			goto done;
		}

		if ((error = git_index_entrymap_put(&index->entries_map, entry)) < 0) {
			index_entry_free(entry);
			goto done;
		}
		error = 0;

		if (index->version >= INDEX_VERSION_NUMBER_COMP)
			last = entry->path;

		seek_forward(entry_size);
	}

	if (i != header.entry_count) {
		error = index_error_invalid("header entries changed while parsing");
		goto done;
	}

	/* There's still space for some extensions! */
	while (buffer_size > checksum_size) {
		size_t extension_size;

		if ((error = read_extension(&extension_size, index, checksum_size, buffer, buffer_size)) < 0) {
			goto done;
		}

		seek_forward(extension_size);
	}

	if (buffer_size != checksum_size) {
		error = index_error_invalid(
			"buffer size does not match index footer size");
		goto done;
	}

	/*
	 * SHA-1 or SHA-256 (depending on the repository's object format)
	 * over the content of the index file before this checksum.
	 * Note: checksum may be 0 if the index was written by a client
	 * where index.skipHash was set to true.
	 */
	if (memcmp(zero_checksum, buffer, checksum_size) != 0 &&
	    memcmp(checksum, buffer, checksum_size) != 0) {
		error = index_error_invalid(
			"calculated checksum does not match expected");
		goto done;
	}

	memcpy(index->checksum, checksum, checksum_size);

#undef seek_forward

	/* Entries are stored case-sensitively on disk, so re-sort now if
	 * in-memory index is supposed to be case-insensitive
	 */
	git_vector_set_sorted(&index->entries, !index->ignore_case);
	git_vector_sort(&index->entries);

	index->dirty = 0;
done:
	return error;
}

static bool is_index_extended(git_index *index)
{
	size_t i, extended;
	git_index_entry *entry;

	extended = 0;

	git_vector_foreach(&index->entries, i, entry) {
		entry->flags &= ~GIT_INDEX_ENTRY_EXTENDED;
		if (entry->flags_extended & GIT_INDEX_ENTRY_EXTENDED_FLAGS) {
			extended++;
			entry->flags |= GIT_INDEX_ENTRY_EXTENDED;
		}
	}

	return (extended > 0);
}

static int write_disk_entry(
	git_index *index,
	git_filebuf *file,
	git_index_entry *entry,
	const char *last)
{
	void *mem = NULL;
	struct entry_common *ondisk_common;
	size_t path_len, path_offset, disk_size;
	int varint_len = 0;
	char *path;
	const char *path_start = entry->path;
	size_t same_len = 0;

	index_entry_short_sha1 ondisk_sha1;
	index_entry_long_sha1 ondisk_ext_sha1;
#ifdef GIT_EXPERIMENTAL_SHA256
	index_entry_short_sha256 ondisk_sha256;
	index_entry_long_sha256 ondisk_ext_sha256;
#endif

	switch (index->oid_type) {
	case GIT_OID_SHA1:
		ondisk_common = &ondisk_sha1.common;
		break;
#ifdef GIT_EXPERIMENTAL_SHA256
	case GIT_OID_SHA256:
		ondisk_common = &ondisk_sha256.common;
		break;
#endif
	default:
		GIT_ASSERT(!"invalid oid type");
	}

	path_len = ((struct entry_internal *)entry)->pathlen;

	if (last) {
		const char *last_c = last;

		while (*path_start == *last_c) {
			if (!*path_start || !*last_c)
				break;
			++path_start;
			++last_c;
			++same_len;
		}
		path_len -= same_len;
		varint_len = git_encode_varint(NULL, 0, strlen(last) - same_len);
	}

	disk_size = index_entry_size(path_len, varint_len, index->oid_type, entry->flags);

	if (!disk_size || git_filebuf_reserve(file, &mem, disk_size) < 0)
		return -1;

	memset(mem, 0x0, disk_size);

	/**
	 * Yes, we have to truncate.
	 *
	 * The on-disk format for Index entries clearly defines
	 * the time and size fields to be 4 bytes each -- so even if
	 * we store these values with 8 bytes on-memory, they must
	 * be truncated to 4 bytes before writing to disk.
	 *
	 * In 2038 I will be either too dead or too rich to care about this
	 */
	ondisk_common->ctime.seconds = htonl((uint32_t)entry->ctime.seconds);
	ondisk_common->mtime.seconds = htonl((uint32_t)entry->mtime.seconds);
	ondisk_common->ctime.nanoseconds = htonl(entry->ctime.nanoseconds);
	ondisk_common->mtime.nanoseconds = htonl(entry->mtime.nanoseconds);
	ondisk_common->dev = htonl(entry->dev);
	ondisk_common->ino = htonl(entry->ino);
	ondisk_common->mode = htonl(entry->mode);
	ondisk_common->uid = htonl(entry->uid);
	ondisk_common->gid = htonl(entry->gid);
	ondisk_common->file_size = htonl((uint32_t)entry->file_size);

	switch (index->oid_type) {
	case GIT_OID_SHA1:
		git_oid_raw_cpy(ondisk_sha1.oid, entry->id.id, GIT_OID_SHA1_SIZE);
		ondisk_sha1.flags = htons(entry->flags);
		break;
#ifdef GIT_EXPERIMENTAL_SHA256
	case GIT_OID_SHA256:
		git_oid_raw_cpy(ondisk_sha256.oid, entry->id.id, GIT_OID_SHA256_SIZE);
		ondisk_sha256.flags = htons(entry->flags);
		break;
#endif
	default:
		GIT_ASSERT(!"invalid oid type");
	}

	path_offset = index_entry_path_offset(index->oid_type, entry->flags);

	if (entry->flags & GIT_INDEX_ENTRY_EXTENDED) {
		struct entry_common *ondisk_ext;
		uint16_t flags_extended = htons(entry->flags_extended &
			GIT_INDEX_ENTRY_EXTENDED_FLAGS);

		switch (index->oid_type) {
		case GIT_OID_SHA1:
			memcpy(&ondisk_ext_sha1, &ondisk_sha1,
				sizeof(index_entry_short_sha1));
			ondisk_ext_sha1.flags_extended = flags_extended;
			ondisk_ext = &ondisk_ext_sha1.common;
			break;
#ifdef GIT_EXPERIMENTAL_SHA256
		case GIT_OID_SHA256:
			memcpy(&ondisk_ext_sha256, &ondisk_sha256,
				sizeof(index_entry_short_sha256));
			ondisk_ext_sha256.flags_extended = flags_extended;
			ondisk_ext = &ondisk_ext_sha256.common;
			break;
#endif
		default:
			GIT_ASSERT(!"invalid oid type");
		}

		memcpy(mem, ondisk_ext, path_offset);
	} else {
		switch (index->oid_type) {
		case GIT_OID_SHA1:
			memcpy(mem, &ondisk_sha1, path_offset);
			break;
#ifdef GIT_EXPERIMENTAL_SHA256
		case GIT_OID_SHA256:
			memcpy(mem, &ondisk_sha256, path_offset);
			break;
#endif
		default:
			GIT_ASSERT(!"invalid oid type");
		}
	}

	path = (char *)mem + path_offset;
	disk_size -= path_offset;

	if (last) {
		varint_len = git_encode_varint((unsigned char *) path,
					  disk_size, strlen(last) - same_len);
		GIT_ASSERT(varint_len > 0);

		path += varint_len;
		disk_size -= varint_len;

		/*
		 * If using path compression, we are not allowed
		 * to have additional trailing NULs.
		 */
		GIT_ASSERT(disk_size == path_len + 1);
	} else {
		/*
		 * If no path compression is used, we do have
		 * NULs as padding. As such, simply assert that
		 * we have enough space left to write the path.
		 */
		GIT_ASSERT(disk_size > path_len);
	}

	memcpy(path, path_start, path_len + 1);

	return 0;
}

static int write_entries(git_index *index, git_filebuf *file)
{
	int error = 0;
	size_t i;
	git_vector case_sorted = GIT_VECTOR_INIT, *entries = NULL;
	git_index_entry *entry;
	const char *last = NULL;

	/* If index->entries is sorted case-insensitively, then we need
	 * to re-sort it case-sensitively before writing */
	if (index->ignore_case) {
		if ((error = git_vector_dup(&case_sorted, &index->entries, git_index_entry_cmp)) < 0)
			goto done;

		git_vector_sort(&case_sorted);
		entries = &case_sorted;
	} else {
		entries = &index->entries;
	}

	if (index->version >= INDEX_VERSION_NUMBER_COMP)
		last = "";

	git_vector_foreach(entries, i, entry) {
		if ((error = write_disk_entry(index, file, entry, last)) < 0)
			break;
		if (index->version >= INDEX_VERSION_NUMBER_COMP)
			last = entry->path;
	}

done:
	git_vector_dispose(&case_sorted);
	return error;
}

static int write_extension(git_filebuf *file, struct index_extension *header, git_str *data)
{
	struct index_extension ondisk;

	memset(&ondisk, 0x0, sizeof(struct index_extension));
	memcpy(&ondisk, header, 4);
	ondisk.extension_size = htonl(header->extension_size);

	git_filebuf_write(file, &ondisk, sizeof(struct index_extension));
	return git_filebuf_write(file, data->ptr, data->size);
}

static int create_name_extension_data(git_str *name_buf, git_index_name_entry *conflict_name)
{
	int error = 0;

	if (conflict_name->ancestor == NULL)
		error = git_str_put(name_buf, "\0", 1);
	else
		error = git_str_put(name_buf, conflict_name->ancestor, strlen(conflict_name->ancestor) + 1);

	if (error != 0)
		goto on_error;

	if (conflict_name->ours == NULL)
		error = git_str_put(name_buf, "\0", 1);
	else
		error = git_str_put(name_buf, conflict_name->ours, strlen(conflict_name->ours) + 1);

	if (error != 0)
		goto on_error;

	if (conflict_name->theirs == NULL)
		error = git_str_put(name_buf, "\0", 1);
	else
		error = git_str_put(name_buf, conflict_name->theirs, strlen(conflict_name->theirs) + 1);

on_error:
	return error;
}

static int write_name_extension(git_index *index, git_filebuf *file)
{
	git_str name_buf = GIT_STR_INIT;
	git_vector *out = &index->names;
	git_index_name_entry *conflict_name;
	struct index_extension extension;
	size_t i;
	int error = 0;

	git_vector_foreach(out, i, conflict_name) {
		if ((error = create_name_extension_data(&name_buf, conflict_name)) < 0)
			goto done;
	}

	memset(&extension, 0x0, sizeof(struct index_extension));
	memcpy(&extension.signature, INDEX_EXT_CONFLICT_NAME_SIG, 4);
	extension.extension_size = (uint32_t)name_buf.size;

	error = write_extension(file, &extension, &name_buf);

	git_str_dispose(&name_buf);

done:
	return error;
}

static int create_reuc_extension_data(git_str *reuc_buf, git_index *index, git_index_reuc_entry *reuc)
{
	size_t oid_size = git_oid_size(index->oid_type);
	int i;
	int error = 0;

	if ((error = git_str_put(reuc_buf, reuc->path, strlen(reuc->path) + 1)) < 0)
		return error;

	for (i = 0; i < 3; i++) {
		if ((error = git_str_printf(reuc_buf, "%o", reuc->mode[i])) < 0 ||
			(error = git_str_put(reuc_buf, "\0", 1)) < 0)
			return error;
	}

	for (i = 0; i < 3; i++) {
		if (reuc->mode[i] && (error = git_str_put(reuc_buf, (char *)&reuc->oid[i].id, oid_size)) < 0)
			return error;
	}

	return 0;
}

static int write_reuc_extension(git_index *index, git_filebuf *file)
{
	git_str reuc_buf = GIT_STR_INIT;
	git_vector *out = &index->reuc;
	git_index_reuc_entry *reuc;
	struct index_extension extension;
	size_t i;
	int error = 0;

	git_vector_foreach(out, i, reuc) {
		if ((error = create_reuc_extension_data(&reuc_buf, index, reuc)) < 0)
			goto done;
	}

	memset(&extension, 0x0, sizeof(struct index_extension));
	memcpy(&extension.signature, INDEX_EXT_UNMERGED_SIG, 4);
	extension.extension_size = (uint32_t)reuc_buf.size;

	error = write_extension(file, &extension, &reuc_buf);

	git_str_dispose(&reuc_buf);

done:
	return error;
}

static int write_tree_extension(git_index *index, git_filebuf *file)
{
	struct index_extension extension;
	git_str buf = GIT_STR_INIT;
	int error;

	if (index->tree == NULL)
		return 0;

	if ((error = git_tree_cache_write(&buf, index->tree)) < 0)
		return error;

	memset(&extension, 0x0, sizeof(struct index_extension));
	memcpy(&extension.signature, INDEX_EXT_TREECACHE_SIG, 4);
	extension.extension_size = (uint32_t)buf.size;

	error = write_extension(file, &extension, &buf);

	git_str_dispose(&buf);

	return error;
}

static void clear_uptodate(git_index *index)
{
	git_index_entry *entry;
	size_t i;

	git_vector_foreach(&index->entries, i, entry)
		entry->flags_extended &= ~GIT_INDEX_ENTRY_UPTODATE;
}

static int write_index(
	unsigned char checksum[GIT_HASH_MAX_SIZE],
	size_t *checksum_size,
	git_index *index,
	git_filebuf *file)
{
	struct index_header header;
	bool is_extended;
	uint32_t index_version_number;

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(file);

	GIT_ASSERT(index->oid_type);

	*checksum_size = git_hash_size(git_oid_algorithm(index->oid_type));

	if (index->version <= INDEX_VERSION_NUMBER_EXT)  {
		is_extended = is_index_extended(index);
		index_version_number = is_extended ? INDEX_VERSION_NUMBER_EXT : INDEX_VERSION_NUMBER_LB;
	} else {
		index_version_number = index->version;
	}

	header.signature = htonl(INDEX_HEADER_SIG);
	header.version = htonl(index_version_number);
	header.entry_count = htonl((uint32_t)index->entries.length);

	if (git_filebuf_write(file, &header, sizeof(struct index_header)) < 0)
		return -1;

	if (write_entries(index, file) < 0)
		return -1;

	/* write the tree cache extension */
	if (index->tree != NULL && write_tree_extension(index, file) < 0)
		return -1;

	/* write the rename conflict extension */
	if (index->names.length > 0 && write_name_extension(index, file) < 0)
		return -1;

	/* write the reuc extension */
	if (index->reuc.length > 0 && write_reuc_extension(index, file) < 0)
		return -1;

	/* get out the hash for all the contents we've appended to the file */
	git_filebuf_hash(checksum, file);

	/* write it at the end of the file */
	if (git_filebuf_write(file, checksum, *checksum_size) < 0)
		return -1;

	/* file entries are no longer up to date */
	clear_uptodate(index);

	return 0;
}

int git_index_entry_stage(const git_index_entry *entry)
{
	return GIT_INDEX_ENTRY_STAGE(entry);
}

int git_index_entry_is_conflict(const git_index_entry *entry)
{
	return (GIT_INDEX_ENTRY_STAGE(entry) > 0);
}

typedef struct read_tree_data {
	git_index *index;
	git_vector *old_entries;
	git_vector *new_entries;
	git_vector_cmp entry_cmp;
	git_tree_cache *tree;
} read_tree_data;

static int read_tree_cb(
	const char *root, const git_tree_entry *tentry, void *payload)
{
	read_tree_data *data = payload;
	git_index_entry *entry = NULL, *old_entry;
	git_str path = GIT_STR_INIT;
	size_t pos;

	if (git_tree_entry__is_tree(tentry))
		return 0;

	if (git_str_joinpath(&path, root, tentry->filename) < 0)
		return -1;

	if (index_entry_create(&entry, INDEX_OWNER(data->index), path.ptr, NULL, false) < 0)
		return -1;

	entry->mode = tentry->attr;
	git_oid_cpy(&entry->id, git_tree_entry_id(tentry));

	/* look for corresponding old entry and copy data to new entry */
	if (data->old_entries != NULL &&
		!index_find_in_entries(
			&pos, data->old_entries, data->entry_cmp, path.ptr, 0, 0) &&
		(old_entry = git_vector_get(data->old_entries, pos)) != NULL &&
		entry->mode == old_entry->mode &&
		git_oid_equal(&entry->id, &old_entry->id))
	{
		index_entry_cpy(entry, old_entry);
		entry->flags_extended = 0;
	}

	index_entry_adjust_namemask(entry, path.size);
	git_str_dispose(&path);

	if (git_vector_insert(data->new_entries, entry) < 0) {
		index_entry_free(entry);
		return -1;
	}

	return 0;
}

int git_index_read_tree(git_index *index, const git_tree *tree)
{
	int error = 0;
	git_vector entries = GIT_VECTOR_INIT;
	git_index_entrymap entries_map = GIT_INDEX_ENTRYMAP_INIT;
	read_tree_data data;
	size_t i;
	git_index_entry *e;

	git_vector_set_cmp(&entries, index->entries._cmp); /* match sort */

	data.index = index;
	data.old_entries = &index->entries;
	data.new_entries = &entries;
	data.entry_cmp   = index->entries_search;

	index->tree = NULL;
	git_pool_clear(&index->tree_pool);

	git_vector_sort(&index->entries);

	if ((error = git_tree_walk(tree, GIT_TREEWALK_POST, read_tree_cb, &data)) < 0)
		goto cleanup;

	if ((error = git_index_entrymap_resize(&entries_map, entries.length)) < 0)
		goto cleanup;

	git_vector_foreach(&entries, i, e) {
		if ((error = git_index_entrymap_put(&entries_map, e)) < 0) {
			git_error_set(GIT_ERROR_INDEX, "failed to insert entry into map");
			return error;
		}
	}

	error = 0;

	git_vector_sort(&entries);

	if ((error = git_index_clear(index)) < 0)
		goto cleanup;

	git_vector_swap(&entries, &index->entries);
	git_index_entrymap_swap(&entries_map, &index->entries_map);

	index->dirty = 1;

cleanup:
	git_vector_dispose(&entries);
	git_index_entrymap_dispose(&entries_map);

	if (error < 0)
		return error;

	error = git_tree_cache_read_tree(&index->tree, tree, index->oid_type, &index->tree_pool);

	return error;
}

static int git_index_read_iterator(
	git_index *index,
	git_iterator *new_iterator,
	size_t new_length_hint)
{
	git_vector new_entries = GIT_VECTOR_INIT,
		remove_entries = GIT_VECTOR_INIT;
	git_index_entrymap new_entries_map = GIT_INDEX_ENTRYMAP_INIT;
	git_iterator *index_iterator = NULL;
	git_iterator_options opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *old_entry, *new_entry;
	git_index_entry *entry;
	size_t i;
	int error;

	GIT_ASSERT((new_iterator->flags & GIT_ITERATOR_DONT_IGNORE_CASE));

	if ((error = git_vector_init(&new_entries, new_length_hint, index->entries._cmp)) < 0 ||
	    (error = git_vector_init(&remove_entries, index->entries.length, NULL)) < 0)
		goto done;

	if (new_length_hint &&
	    (error = git_index_entrymap_resize(&new_entries_map, new_length_hint)) < 0)
		goto done;

	opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_INCLUDE_CONFLICTS;

	if ((error = git_iterator_for_index(&index_iterator,
			git_index_owner(index), index, &opts)) < 0 ||
		((error = git_iterator_current(&old_entry, index_iterator)) < 0 &&
			error != GIT_ITEROVER) ||
		((error = git_iterator_current(&new_entry, new_iterator)) < 0 &&
			error != GIT_ITEROVER))
		goto done;

	while (true) {
		git_index_entry
			*dup_entry = NULL,
			*add_entry = NULL,
			*remove_entry = NULL;
		int diff;

		error = 0;

		if (old_entry && new_entry)
			diff = git_index_entry_cmp(old_entry, new_entry);
		else if (!old_entry && new_entry)
			diff = 1;
		else if (old_entry && !new_entry)
			diff = -1;
		else
			break;

		if (diff < 0) {
			remove_entry = (git_index_entry *)old_entry;
		} else if (diff > 0) {
			dup_entry = (git_index_entry *)new_entry;
		} else {
			/* Path and stage are equal, if the OID is equal, keep it to
			 * keep the stat cache data.
			 */
			if (git_oid_equal(&old_entry->id, &new_entry->id) &&
				old_entry->mode == new_entry->mode) {
				add_entry = (git_index_entry *)old_entry;
			} else {
				dup_entry = (git_index_entry *)new_entry;
				remove_entry = (git_index_entry *)old_entry;
			}
		}

		if (dup_entry) {
			if ((error = index_entry_dup_nocache(&add_entry, index, dup_entry)) < 0)
				goto done;

			index_entry_adjust_namemask(add_entry,
				((struct entry_internal *)add_entry)->pathlen);
		}

		/* invalidate this path in the tree cache if this is new (to
		 * invalidate the parent trees)
		 */
		if (dup_entry && !remove_entry && index->tree)
			git_tree_cache_invalidate_path(index->tree, dup_entry->path);

		if (add_entry) {
			if ((error = git_vector_insert(&new_entries, add_entry)) == 0)
				error = git_index_entrymap_put(&new_entries_map, add_entry);
		}

		if (remove_entry && error >= 0)
			error = git_vector_insert(&remove_entries, remove_entry);

		if (error < 0) {
			git_error_set(GIT_ERROR_INDEX, "failed to insert entry");
			goto done;
		}

		if (diff <= 0) {
			if ((error = git_iterator_advance(&old_entry, index_iterator)) < 0 &&
				error != GIT_ITEROVER)
				goto done;
		}

		if (diff >= 0) {
			if ((error = git_iterator_advance(&new_entry, new_iterator)) < 0 &&
				error != GIT_ITEROVER)
				goto done;
		}
	}

	if ((error = git_index_name_clear(index)) < 0 ||
		(error = git_index_reuc_clear(index)) < 0)
	    goto done;

	git_vector_swap(&new_entries, &index->entries);
	git_index_entrymap_swap(&index->entries_map, &new_entries_map);

	git_vector_foreach(&remove_entries, i, entry) {
		if (index->tree)
			git_tree_cache_invalidate_path(index->tree, entry->path);

		index_entry_free(entry);
	}

	clear_uptodate(index);

	index->dirty = 1;
	error = 0;

done:
	git_index_entrymap_dispose(&new_entries_map);
	git_vector_dispose(&new_entries);
	git_vector_dispose(&remove_entries);
	git_iterator_free(index_iterator);
	return error;
}

int git_index_read_index(
	git_index *index,
	const git_index *new_index)
{
	git_iterator *new_iterator = NULL;
	git_iterator_options opts = GIT_ITERATOR_OPTIONS_INIT;
	int error;

	opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_INCLUDE_CONFLICTS;

	if ((error = git_iterator_for_index(&new_iterator,
		git_index_owner(new_index), (git_index *)new_index, &opts)) < 0 ||
		(error = git_index_read_iterator(index, new_iterator,
		new_index->entries.length)) < 0)
		goto done;

done:
	git_iterator_free(new_iterator);
	return error;
}

git_repository *git_index_owner(const git_index *index)
{
	return INDEX_OWNER(index);
}

enum {
	INDEX_ACTION_NONE = 0,
	INDEX_ACTION_UPDATE = 1,
	INDEX_ACTION_REMOVE = 2,
	INDEX_ACTION_ADDALL = 3
};

int git_index_add_all(
	git_index *index,
	const git_strarray *paths,
	unsigned int flags,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error;
	git_repository *repo;
	git_pathspec ps;
	bool no_fnmatch = (flags & GIT_INDEX_ADD_DISABLE_PATHSPEC_MATCH) != 0;

	GIT_ASSERT_ARG(index);

	repo = INDEX_OWNER(index);
	if ((error = git_repository__ensure_not_bare(repo, "index add all")) < 0)
		return error;

	if ((error = git_pathspec__init(&ps, paths)) < 0)
		return error;

	/* optionally check that pathspec doesn't mention any ignored files */
	if ((flags & GIT_INDEX_ADD_CHECK_PATHSPEC) != 0 &&
		(flags & GIT_INDEX_ADD_FORCE) == 0 &&
		(error = git_ignore__check_pathspec_for_exact_ignores(
			repo, &ps.pathspec, no_fnmatch)) < 0)
		goto cleanup;

	error = index_apply_to_wd_diff(index, INDEX_ACTION_ADDALL, paths, flags, cb, payload);

	if (error)
		git_error_set_after_callback(error);

cleanup:
	git_pathspec__clear(&ps);

	return error;
}

struct foreach_diff_data {
	git_index *index;
	const git_pathspec *pathspec;
	unsigned int flags;
	git_index_matched_path_cb cb;
	void *payload;
};

static int apply_each_file(const git_diff_delta *delta, float progress, void *payload)
{
	struct foreach_diff_data *data = payload;
	const char *match, *path;
	int error = 0;

	GIT_UNUSED(progress);

	path = delta->old_file.path;

	/* We only want those which match the pathspecs */
	if (!git_pathspec__match(
		    &data->pathspec->pathspec, path, false, (bool)data->index->ignore_case,
		    &match, NULL))
		return 0;

	if (data->cb)
		error = data->cb(path, match, data->payload);

	if (error > 0) /* skip this entry */
		return 0;
	if (error < 0) /* actual error */
		return error;

	/* If the workdir item does not exist, remove it from the index. */
	if ((delta->new_file.flags & GIT_DIFF_FLAG_EXISTS) == 0)
		error = git_index_remove_bypath(data->index, path);
	else
		error = git_index_add_bypath(data->index, delta->new_file.path);

	return error;
}

static int index_apply_to_wd_diff(git_index *index, int action, const git_strarray *paths,
				  unsigned int flags,
				  git_index_matched_path_cb cb, void *payload)
{
	int error;
	git_diff *diff;
	git_pathspec ps;
	git_repository *repo;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	struct foreach_diff_data data = {
		index,
		NULL,
		flags,
		cb,
		payload,
	};

	GIT_ASSERT_ARG(index);
	GIT_ASSERT_ARG(action == INDEX_ACTION_UPDATE || action == INDEX_ACTION_ADDALL);

	repo = INDEX_OWNER(index);

	if (!repo) {
		return create_index_error(-1,
			"cannot run update; the index is not backed up by a repository.");
	}

	/*
	 * We do the matching ourselves instead of passing the list to
	 * diff because we want to tell the callback which one
	 * matched, which we do not know if we ask diff to filter for us.
	 */
	if ((error = git_pathspec__init(&ps, paths)) < 0)
		return error;

	opts.flags = GIT_DIFF_INCLUDE_TYPECHANGE;
	if (action == INDEX_ACTION_ADDALL) {
		opts.flags |= GIT_DIFF_INCLUDE_UNTRACKED |
			GIT_DIFF_RECURSE_UNTRACKED_DIRS;

		if (flags == GIT_INDEX_ADD_FORCE)
			opts.flags |= GIT_DIFF_INCLUDE_IGNORED |
			              GIT_DIFF_RECURSE_IGNORED_DIRS;
	}

	if ((error = git_diff_index_to_workdir(&diff, repo, index, &opts)) < 0)
		goto cleanup;

	data.pathspec = &ps;
	error = git_diff_foreach(diff, apply_each_file, NULL, NULL, NULL, &data);
	git_diff_free(diff);

	if (error) /* make sure error is set if callback stopped iteration */
		git_error_set_after_callback(error);

cleanup:
	git_pathspec__clear(&ps);
	return error;
}

static int index_apply_to_all(
	git_index *index,
	int action,
	const git_strarray *paths,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error = 0;
	size_t i;
	git_pathspec ps;
	const char *match;
	git_str path = GIT_STR_INIT;

	GIT_ASSERT_ARG(index);

	if ((error = git_pathspec__init(&ps, paths)) < 0)
		return error;

	git_vector_sort(&index->entries);

	for (i = 0; !error && i < index->entries.length; ++i) {
		git_index_entry *entry = git_vector_get(&index->entries, i);

		/* check if path actually matches */
		if (!git_pathspec__match(
				&ps.pathspec, entry->path, false, (bool)index->ignore_case,
				&match, NULL))
			continue;

		/* issue notification callback if requested */
		if (cb && (error = cb(entry->path, match, payload)) != 0) {
			if (error > 0) { /* return > 0 means skip this one */
				error = 0;
				continue;
			}
			if (error < 0)   /* return < 0 means abort */
				break;
		}

		/* index manipulation may alter entry, so don't depend on it */
		if ((error = git_str_sets(&path, entry->path)) < 0)
			break;

		switch (action) {
		case INDEX_ACTION_NONE:
			break;
		case INDEX_ACTION_UPDATE:
			error = git_index_add_bypath(index, path.ptr);

			if (error == GIT_ENOTFOUND) {
				git_error_clear();

				error = git_index_remove_bypath(index, path.ptr);

				if (!error) /* back up foreach if we removed this */
					i--;
			}
			break;
		case INDEX_ACTION_REMOVE:
			if (!(error = git_index_remove_bypath(index, path.ptr)))
				i--; /* back up foreach if we removed this */
			break;
		default:
			git_error_set(GIT_ERROR_INVALID, "unknown index action %d", action);
			error = -1;
			break;
		}
	}

	git_str_dispose(&path);
	git_pathspec__clear(&ps);

	return error;
}

int git_index_remove_all(
	git_index *index,
	const git_strarray *pathspec,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error = index_apply_to_all(
		index, INDEX_ACTION_REMOVE, pathspec, cb, payload);

	if (error) /* make sure error is set if callback stopped iteration */
		git_error_set_after_callback(error);

	return error;
}

int git_index_update_all(
	git_index *index,
	const git_strarray *pathspec,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error = index_apply_to_wd_diff(index, INDEX_ACTION_UPDATE, pathspec, 0, cb, payload);
	if (error) /* make sure error is set if callback stopped iteration */
		git_error_set_after_callback(error);

	return error;
}

int git_index_snapshot_new(git_vector *snap, git_index *index)
{
	int error;

	GIT_REFCOUNT_INC(index);

	git_atomic32_inc(&index->readers);
	git_vector_sort(&index->entries);

	error = git_vector_dup(snap, &index->entries, index->entries._cmp);

	if (error < 0)
		git_index_snapshot_release(snap, index);

	return error;
}

void git_index_snapshot_release(git_vector *snap, git_index *index)
{
	git_vector_dispose(snap);

	git_atomic32_dec(&index->readers);

	git_index_free(index);
}

int git_index_snapshot_find(
	size_t *out, git_vector *entries, git_vector_cmp entry_srch,
	const char *path, size_t path_len, int stage)
{
	return index_find_in_entries(out, entries, entry_srch, path, path_len, stage);
}

int git_indexwriter_init(
	git_indexwriter *writer,
	git_index *index)
{
	int filebuf_hash, error;

	GIT_REFCOUNT_INC(index);

	writer->index = index;

	filebuf_hash = git_filebuf_hash_flags(git_oid_algorithm(index->oid_type));
	GIT_ASSERT(filebuf_hash);

	if (!index->index_file_path)
		return create_index_error(-1,
			"failed to write index: The index is in-memory only");

	if ((error = git_filebuf_open(&writer->file,
			index->index_file_path,
			git_filebuf_hash_flags(filebuf_hash),
			GIT_INDEX_FILE_MODE)) < 0) {
		if (error == GIT_ELOCKED)
			git_error_set(GIT_ERROR_INDEX, "the index is locked; this might be due to a concurrent or crashed process");

		return error;
	}

	writer->should_write = 1;

	return 0;
}

int git_indexwriter_init_for_operation(
	git_indexwriter *writer,
	git_repository *repo,
	unsigned int *checkout_strategy)
{
	git_index *index;
	int error;

	if ((error = git_repository_index__weakptr(&index, repo)) < 0 ||
		(error = git_indexwriter_init(writer, index)) < 0)
		return error;

	writer->should_write = (*checkout_strategy & GIT_CHECKOUT_DONT_WRITE_INDEX) == 0;
	*checkout_strategy |= GIT_CHECKOUT_DONT_WRITE_INDEX;

	return 0;
}

int git_indexwriter_commit(git_indexwriter *writer)
{
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	size_t checksum_size;
	int error;

	if (!writer->should_write)
		return 0;

	git_vector_sort(&writer->index->entries);
	git_vector_sort(&writer->index->reuc);

	if ((error = write_index(checksum, &checksum_size, writer->index, &writer->file)) < 0) {
		git_indexwriter_cleanup(writer);
		return error;
	}

	if ((error = git_filebuf_commit(&writer->file)) < 0)
		return error;

	if ((error = git_futils_filestamp_check(
		&writer->index->stamp, writer->index->index_file_path)) < 0) {
		git_error_set(GIT_ERROR_OS, "could not read index timestamp");
		return -1;
	}

	writer->index->dirty = 0;
	writer->index->on_disk = 1;
	memcpy(writer->index->checksum, checksum, checksum_size);

	git_index_free(writer->index);
	writer->index = NULL;

	return 0;
}

void git_indexwriter_cleanup(git_indexwriter *writer)
{
	git_filebuf_cleanup(&writer->file);

	git_index_free(writer->index);
	writer->index = NULL;
}

/* Deprecated functions */

#ifndef GIT_DEPRECATE_HARD
int git_index_add_frombuffer(
    git_index *index, const git_index_entry *source_entry,
    const void *buffer, size_t len)
{
	return git_index_add_from_buffer(index, source_entry, buffer, len);
}
#endif
