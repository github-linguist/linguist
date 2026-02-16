/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include <zlib.h>
#include "git2/repository.h"
#include "git2/indexer.h"
#include "git2/sys/odb_backend.h"
#include "delta.h"
#include "futils.h"
#include "hash.h"
#include "midx.h"
#include "mwindow.h"
#include "odb.h"
#include "pack.h"

#include "git2/odb_backend.h"

/* re-freshen pack files no more than every 2 seconds */
#define FRESHEN_FREQUENCY 2

struct pack_backend {
	git_odb_backend parent;
	git_odb_backend_pack_options opts;
	git_midx_file *midx;
	git_vector midx_packs;
	git_vector packs;
	struct git_pack_file *last_found;
	char *pack_folder;
};

struct pack_writepack {
	struct git_odb_writepack parent;
	git_indexer *indexer;
};

/**
 * The wonderful tale of a Packed Object lookup query
 * ===================================================
 *	A riveting and epic story of epicness and ASCII
 *			art, presented by yours truly,
 *				Sir Vicent of Marti
 *
 *
 *	Chapter 1: Once upon a time...
 *	Initialization of the Pack Backend
 *	--------------------------------------------------
 *
 * # git_odb_backend_pack
 * | Creates the pack backend structure, initializes the
 * | callback pointers to our default read() and exist() methods,
 * | and tries to find the `pack` folder, if it exists. ODBs without a `pack`
 * | folder are ignored altogether. If there is a `pack` folder, it tries to
 * | preload all the known packfiles in the ODB.
 * |
 * |-# pack_backend__refresh
 *   | The `multi-pack-index` is loaded if it exists and is valid.
 *   | Then we run a `dirent` callback through every file in the pack folder,
 *   | even those present in `multi-pack-index`. The unindexed packfiles are
 *   | then sorted according to a sorting callback.
 *   |
 *   |-# refresh_multi_pack_index
 *   |   Detect the presence of the `multi-pack-index` file. If it needs to be
 *   |   refreshed, frees the old copy and tries to load the new one, together
 *   |   with all the packfiles it indexes. If the process fails, fall back to
 *   |   the old behavior, as if the `multi-pack-index` file was not there.
 *   |
 *   |-# packfile_load__cb
 *   | | This callback is called from `dirent` with every single file
 *   | | inside the pack folder. We find the packs by actually locating
 *   | | their index (ends in ".idx"). From that index, we verify that
 *   | | the corresponding packfile exists and is valid, and if so, we
 *   | | add it to the pack list.
 *   | |
 *   | # git_mwindow_get_pack
 *   |   Make sure that there's a packfile to back this index, and store
 *   |   some very basic information regarding the packfile itself,
 *   |   such as the full path, the size, and the modification time.
 *   |   We don't actually open the packfile to check for internal consistency.
 *   |
 *   |-# packfile_sort__cb
 *       Sort all the preloaded packs according to some specific criteria:
 *       we prioritize the "newer" packs because it's more likely they
 *       contain the objects we are looking for, and we prioritize local
 *       packs over remote ones.
 *
 *
 *
 *	Chapter 2: To be, or not to be...
 *	A standard packed `exist` query for an OID
 *	--------------------------------------------------
 *
 * # pack_backend__exists / pack_backend__exists_prefix
 * | Check if the given oid (or an oid prefix) exists in any of the
 * | packs that have been loaded for our ODB.
 * |
 * |-# pack_entry_find / pack_entry_find_prefix
 *   | If there is a multi-pack-index present, search the oid in that
 *   | index first. If it is not found there, iterate through all the unindexed
 *   | packs that have been preloaded (starting by the pack where the latest
 *   | object was found) to try to find the OID in one of them.
 *   |
 *   |-# git_midx_entry_find
 *   |   Search for the oid in the multi-pack-index. See
 *   |   <https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt>
 *   |   for specifics on the multi-pack-index format and how do we find
 *   |   entries in it.
 *   |
 *   |-# git_pack_entry_find
 *     | Check the index of an individual unindexed pack to see if the
 *     | OID can be found. If we can find the offset to that inside of the
 *     | index, that means the object is contained inside of the packfile and
 *     | we can stop searching. Before returning, we verify that the
 *     | packfile behind the index we are searching still exists on disk.
 *     |
 *     |-# pack_entry_find_offset
 *       | Mmap the actual index file to disk if it hasn't been opened
 *       | yet, and run a binary search through it to find the OID.
 *       | See <https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt>
 *       | for specifics on the Packfile Index format and how do we find
 *       | entries in it.
 *       |
 *       |-# pack_index_open
 *         | Guess the name of the index based on the full path to the
 *         | packfile, open it and verify its contents. Only if the index
 *         | has not been opened already.
 *         |
 *         |-# pack_index_check
 *             Mmap the index file and do a quick run through the header
 *             to guess the index version (right now we support v1 and v2),
 *             and to verify that the size of the index makes sense.
 *
 *
 *
 *	Chapter 3: The neverending story...
 *	A standard packed `lookup` query for an OID
 *	--------------------------------------------------
 *
 * # pack_backend__read / pack_backend__read_prefix
 * | Check if the given oid (or an oid prefix) exists in any of the
 * | packs that have been loaded for our ODB. If it does, open the packfile and
 * | read from it.
 * |
 * |-# git_packfile_unpack
 *     Armed with a packfile and the offset within it, we can finally unpack
 *     the object pointed at by the oid. This involves mmapping part of
 *     the `.pack` file, and uncompressing the object within it (if it is
 *     stored in the undelfitied representation), or finding a base object and
 *     applying some deltas to its uncompressed representation (if it is stored
 *     in the deltified representation). See
 *     <https://github.com/git/git/blob/master/Documentation/technical/pack-format.txt>
 *     for specifics on the Packfile format and how do we read from it.
 *
 */


/***********************************************************
 *
 * FORWARD DECLARATIONS
 *
 ***********************************************************/

static int packfile_sort__cb(const void *a_, const void *b_);

static int packfile_load__cb(void *_data, git_str *path);

static int packfile_byname_search_cmp(const void *path, const void *pack_entry);

static int pack_entry_find(struct git_pack_entry *e,
	struct pack_backend *backend, const git_oid *oid);

/* Can find the offset of an object given
 * a prefix of an identifier.
 * Sets GIT_EAMBIGUOUS if short oid is ambiguous.
 * This method assumes that len is between
 * GIT_OID_MINPREFIXLEN and the hexsize for the hash type.
 */
static int pack_entry_find_prefix(
	struct git_pack_entry *e,
	struct pack_backend *backend,
	const git_oid *short_oid,
	size_t len);



/***********************************************************
 *
 * PACK WINDOW MANAGEMENT
 *
 ***********************************************************/

static int packfile_byname_search_cmp(const void *path_, const void *p_)
{
	const git_str *path = (const git_str *)path_;
	const struct git_pack_file *p = (const struct git_pack_file *)p_;

	return strncmp(p->pack_name, git_str_cstr(path), git_str_len(path));
}

static int packfile_sort__cb(const void *a_, const void *b_)
{
	const struct git_pack_file *a = a_;
	const struct git_pack_file *b = b_;
	int st;

	/*
	 * Local packs tend to contain objects specific to our
	 * variant of the project than remote ones. In addition,
	 * remote ones could be on a network mounted filesystem.
	 * Favor local ones for these reasons.
	 */
	st = a->pack_local - b->pack_local;
	if (st)
		return -st;

	/*
	 * Younger packs tend to contain more recent objects,
	 * and more recent objects tend to get accessed more
	 * often.
	 */
	if (a->mtime < b->mtime)
		return 1;
	else if (a->mtime == b->mtime)
		return 0;

	return -1;
}


static int packfile_load__cb(void *data, git_str *path)
{
	struct pack_backend *backend = data;
	struct git_pack_file *pack;
	const char *path_str = git_str_cstr(path);
	git_str index_prefix = GIT_STR_INIT;
	size_t cmp_len = git_str_len(path);
	int error;

	if (cmp_len <= strlen(".idx") || git__suffixcmp(path_str, ".idx") != 0)
		return 0; /* not an index */

	cmp_len -= strlen(".idx");
	git_str_attach_notowned(&index_prefix, path_str, cmp_len);

	if (git_vector_search2(NULL, &backend->midx_packs, packfile_byname_search_cmp, &index_prefix) == 0)
		return 0;
	if (git_vector_search2(NULL, &backend->packs, packfile_byname_search_cmp, &index_prefix) == 0)
		return 0;

	error = git_mwindow_get_pack(&pack, path->ptr, backend->opts.oid_type);

	/* ignore missing .pack file as git does */
	if (error == GIT_ENOTFOUND) {
		git_error_clear();
		return 0;
	}

	if (!error)
		error = git_vector_insert(&backend->packs, pack);

	return error;

}

static int pack_entry_find(struct git_pack_entry *e, struct pack_backend *backend, const git_oid *oid)
{
	struct git_pack_file *last_found = backend->last_found, *p;
	git_midx_entry midx_entry;
	size_t oid_hexsize = git_oid_hexsize(backend->opts.oid_type);
	size_t i;

	if (backend->midx &&
		git_midx_entry_find(&midx_entry, backend->midx, oid, oid_hexsize) == 0 &&
		midx_entry.pack_index < git_vector_length(&backend->midx_packs)) {
		e->offset = midx_entry.offset;
		git_oid_cpy(&e->id, &midx_entry.sha1);
		e->p = git_vector_get(&backend->midx_packs, midx_entry.pack_index);
		return 0;
	}

	if (last_found &&
		git_pack_entry_find(e, last_found, oid, oid_hexsize) == 0)
		return 0;

	git_vector_foreach(&backend->packs, i, p) {
		if (p == last_found)
			continue;

		if (git_pack_entry_find(e, p, oid, oid_hexsize) == 0) {
			backend->last_found = p;
			return 0;
		}
	}

	return git_odb__error_notfound(
		"failed to find pack entry", oid, oid_hexsize);
}

static int pack_entry_find_prefix(
	struct git_pack_entry *e,
	struct pack_backend *backend,
	const git_oid *short_oid,
	size_t len)
{
	int error;
	size_t i;
	git_oid found_full_oid;
	bool found = false;
	struct git_pack_file *last_found = backend->last_found, *p;
	git_midx_entry midx_entry;

#ifdef GIT_EXPERIMENTAL_SHA256
	git_oid_clear(&found_full_oid, short_oid->type);
#else
	git_oid_clear(&found_full_oid, GIT_OID_SHA1);
#endif

	if (backend->midx) {
		error = git_midx_entry_find(&midx_entry, backend->midx, short_oid, len);
		if (error == GIT_EAMBIGUOUS)
			return error;
		if (!error && midx_entry.pack_index < git_vector_length(&backend->midx_packs)) {
			e->offset = midx_entry.offset;
			git_oid_cpy(&e->id, &midx_entry.sha1);
			e->p = git_vector_get(&backend->midx_packs, midx_entry.pack_index);
			git_oid_cpy(&found_full_oid, &e->id);
			found = true;
		}
	}

	if (last_found) {
		error = git_pack_entry_find(e, last_found, short_oid, len);
		if (error == GIT_EAMBIGUOUS)
			return error;
		if (!error) {
			if (found && git_oid_cmp(&e->id, &found_full_oid))
				return git_odb__error_ambiguous("found multiple pack entries");
			git_oid_cpy(&found_full_oid, &e->id);
			found = true;
		}
	}

	git_vector_foreach(&backend->packs, i, p) {
		if (p == last_found)
			continue;

		error = git_pack_entry_find(e, p, short_oid, len);
		if (error == GIT_EAMBIGUOUS)
			return error;
		if (!error) {
			if (found && git_oid_cmp(&e->id, &found_full_oid))
				return git_odb__error_ambiguous("found multiple pack entries");
			git_oid_cpy(&found_full_oid, &e->id);
			found = true;
			backend->last_found = p;
		}
	}

	if (!found)
		return git_odb__error_notfound("no matching pack entry for prefix",
			short_oid, len);
	else
		return 0;
}

/***********************************************************
 *
 * MULTI-PACK-INDEX SUPPORT
 *
 * Functions needed to support the multi-pack-index.
 *
 ***********************************************************/

/*
 * Remove the multi-pack-index, and move all midx_packs to packs.
 */
static int remove_multi_pack_index(struct pack_backend *backend)
{
	size_t i, j = git_vector_length(&backend->packs);
	struct pack_backend *p;
	int error = git_vector_size_hint(
			&backend->packs,
			j + git_vector_length(&backend->midx_packs));
	if (error < 0)
		return error;

	git_vector_foreach(&backend->midx_packs, i, p)
		git_vector_set(NULL, &backend->packs, j++, p);
	git_vector_clear(&backend->midx_packs);

	git_midx_free(backend->midx);
	backend->midx = NULL;

	return 0;
}

/*
 * Loads a single .pack file referred to by the multi-pack-index. These must
 * match the order in which they are declared in the multi-pack-index file,
 * since these files are referred to by their index.
 */
static int process_multi_pack_index_pack(
		struct pack_backend *backend,
		size_t i,
		const char *packfile_name)
{
	int error;
	struct git_pack_file *pack;
	size_t found_position;
	git_str pack_path = GIT_STR_INIT, index_prefix = GIT_STR_INIT;

	error = git_str_joinpath(&pack_path, backend->pack_folder, packfile_name);
	if (error < 0)
		return error;

	/* This is ensured by midx_parse_packfile_name() */
	if (git_str_len(&pack_path) <= strlen(".idx") || git__suffixcmp(git_str_cstr(&pack_path), ".idx") != 0)
		return git_odb__error_notfound("midx file contained a non-index", NULL, 0);

	git_str_attach_notowned(&index_prefix, git_str_cstr(&pack_path), git_str_len(&pack_path) - strlen(".idx"));

	if (git_vector_search2(&found_position, &backend->packs, packfile_byname_search_cmp, &index_prefix) == 0) {
		/* Pack was found in the packs list. Moving it to the midx_packs list. */
		git_str_dispose(&pack_path);
		git_vector_set(NULL, &backend->midx_packs, i, git_vector_get(&backend->packs, found_position));
		git_vector_remove(&backend->packs, found_position);
		return 0;
	}

	/* Pack was not found. Allocate a new one. */
	error = git_mwindow_get_pack(
		&pack,
		git_str_cstr(&pack_path),
		backend->opts.oid_type);
	git_str_dispose(&pack_path);
	if (error < 0)
		return error;

	git_vector_set(NULL, &backend->midx_packs, i, pack);
	return 0;
}

/*
 * Reads the multi-pack-index. If this fails for whatever reason, the
 * multi-pack-index object is freed, and all the packfiles that are related to
 * it are moved to the unindexed packfiles vector.
 */
static int refresh_multi_pack_index(struct pack_backend *backend)
{
	int error;
	git_str midx_path = GIT_STR_INIT;
	const char *packfile_name;
	size_t i;

	error = git_str_joinpath(&midx_path, backend->pack_folder, "multi-pack-index");
	if (error < 0)
		return error;

	/*
	 * Check whether the multi-pack-index has changed. If it has, close any
	 * old multi-pack-index and move all the packfiles to the unindexed
	 * packs. This is done to prevent losing any open packfiles in case
	 * refreshing the new multi-pack-index fails, or the file is deleted.
	 */
	if (backend->midx) {
		if (!git_midx_needs_refresh(backend->midx, git_str_cstr(&midx_path))) {
			git_str_dispose(&midx_path);
			return 0;
		}
		error = remove_multi_pack_index(backend);
		if (error < 0) {
			git_str_dispose(&midx_path);
			return error;
		}
	}

	error = git_midx_open(&backend->midx, git_str_cstr(&midx_path),
		backend->opts.oid_type);

	git_str_dispose(&midx_path);
	if (error < 0)
		return error;

	git_vector_resize_to(&backend->midx_packs, git_vector_length(&backend->midx->packfile_names));

	git_vector_foreach(&backend->midx->packfile_names, i, packfile_name) {
		error = process_multi_pack_index_pack(backend, i, packfile_name);
		if (error < 0) {
			/*
			 * Something failed during reading multi-pack-index.
			 * Restore the state of backend as if the
			 * multi-pack-index was never there, and move all
			 * packfiles that have been processed so far to the
			 * unindexed packs.
			 */
			git_vector_resize_to(&backend->midx_packs, i);
			remove_multi_pack_index(backend);
			return error;
		}
	}

	return 0;
}

/***********************************************************
 *
 * PACKED BACKEND PUBLIC API
 *
 * Implement the git_odb_backend API calls
 *
 ***********************************************************/
static int pack_backend__refresh(git_odb_backend *backend_)
{
	int error;
	struct stat st;
	git_str path = GIT_STR_INIT;
	struct pack_backend *backend = (struct pack_backend *)backend_;

	if (backend->pack_folder == NULL)
		return 0;

	if (p_stat(backend->pack_folder, &st) < 0 || !S_ISDIR(st.st_mode))
		return git_odb__error_notfound("failed to refresh packfiles", NULL, 0);

	if (refresh_multi_pack_index(backend) < 0) {
		/*
		 * It is okay if this fails. We will just not use the
		 * multi-pack-index in this case.
		 */
		git_error_clear();
	}

	/* reload all packs */
	git_str_sets(&path, backend->pack_folder);
	error = git_fs_path_direach(&path, 0, packfile_load__cb, backend);

	git_str_dispose(&path);
	git_vector_sort(&backend->packs);

	return error;
}

static int pack_backend__read_header(
	size_t *len_p, git_object_t *type_p,
	struct git_odb_backend *backend, const git_oid *oid)
{
	struct git_pack_entry e;
	int error;

	GIT_ASSERT_ARG(len_p);
	GIT_ASSERT_ARG(type_p);
	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(oid);

	if ((error = pack_entry_find(&e, (struct pack_backend *)backend, oid)) < 0)
		return error;

	return git_packfile_resolve_header(len_p, type_p, e.p, e.offset);
}

static int pack_backend__freshen(
	git_odb_backend *backend, const git_oid *oid)
{
	struct git_pack_entry e;
	time_t now;
	int error;

	if ((error = pack_entry_find(&e, (struct pack_backend *)backend, oid)) < 0)
		return error;

	now = time(NULL);

	if (e.p->last_freshen > now - FRESHEN_FREQUENCY)
		return 0;

	if ((error = git_futils_touch(e.p->pack_name, &now)) < 0)
		return error;

	e.p->last_freshen = now;
	return 0;
}

static int pack_backend__read(
	void **buffer_p, size_t *len_p, git_object_t *type_p,
	git_odb_backend *backend, const git_oid *oid)
{
	struct git_pack_entry e;
	git_rawobj raw = {NULL};
	int error;

	if ((error = pack_entry_find(&e, (struct pack_backend *)backend, oid)) < 0 ||
		(error = git_packfile_unpack(&raw, e.p, &e.offset)) < 0)
		return error;

	*buffer_p = raw.data;
	*len_p = raw.len;
	*type_p = raw.type;

	return 0;
}

static int pack_backend__read_prefix(
	git_oid *out_oid,
	void **buffer_p,
	size_t *len_p,
	git_object_t *type_p,
	git_odb_backend *_backend,
	const git_oid *short_oid,
	size_t len)
{
	struct pack_backend *backend = (struct pack_backend *)_backend;
	int error = 0;

	if (len < GIT_OID_MINPREFIXLEN)
		error = git_odb__error_ambiguous("prefix length too short");

	else if (len >= git_oid_hexsize(backend->opts.oid_type)) {
		/* We can fall back to regular read method */
		error = pack_backend__read(buffer_p, len_p, type_p, _backend, short_oid);
		if (!error)
			git_oid_cpy(out_oid, short_oid);
	} else {
		struct git_pack_entry e;
		git_rawobj raw = {NULL};

		if ((error = pack_entry_find_prefix(&e,
				backend, short_oid, len)) == 0 &&
		    (error = git_packfile_unpack(&raw, e.p, &e.offset)) == 0)
		{
			*buffer_p = raw.data;
			*len_p = raw.len;
			*type_p = raw.type;
			git_oid_cpy(out_oid, &e.id);
		}
	}

	return error;
}

static int pack_backend__exists(git_odb_backend *backend, const git_oid *oid)
{
	struct git_pack_entry e;
	return pack_entry_find(&e, (struct pack_backend *)backend, oid) == 0;
}

static int pack_backend__exists_prefix(
	git_oid *out, git_odb_backend *backend, const git_oid *short_id, size_t len)
{
	int error;
	struct pack_backend *pb = (struct pack_backend *)backend;
	struct git_pack_entry e = {0};

	error = pack_entry_find_prefix(&e, pb, short_id, len);
	git_oid_cpy(out, &e.id);
	return error;
}

static int pack_backend__foreach(git_odb_backend *_backend, git_odb_foreach_cb cb, void *data)
{
	int error;
	struct git_pack_file *p;
	struct pack_backend *backend;
	unsigned int i;

	GIT_ASSERT_ARG(_backend);
	GIT_ASSERT_ARG(cb);

	backend = (struct pack_backend *)_backend;

	/* Make sure we know about the packfiles */
	if ((error = pack_backend__refresh(_backend)) != 0)
		return error;

	if (backend->midx && (error = git_midx_foreach_entry(backend->midx, cb, data)) != 0)
		return error;
	git_vector_foreach(&backend->packs, i, p) {
		if ((error = git_pack_foreach_entry(p, cb, data)) != 0)
			return error;
	}

	return 0;
}

static int pack_backend__writepack_append(struct git_odb_writepack *_writepack, const void *data, size_t size, git_indexer_progress *stats)
{
	struct pack_writepack *writepack = (struct pack_writepack *)_writepack;

	GIT_ASSERT_ARG(writepack);

	return git_indexer_append(writepack->indexer, data, size, stats);
}

static int pack_backend__writepack_commit(struct git_odb_writepack *_writepack, git_indexer_progress *stats)
{
	struct pack_writepack *writepack = (struct pack_writepack *)_writepack;

	GIT_ASSERT_ARG(writepack);

	return git_indexer_commit(writepack->indexer, stats);
}

static void pack_backend__writepack_free(struct git_odb_writepack *_writepack)
{
	struct pack_writepack *writepack;

	if (!_writepack)
		return;

	writepack = (struct pack_writepack *)_writepack;

	git_indexer_free(writepack->indexer);
	git__free(writepack);
}

static int pack_backend__writepack(struct git_odb_writepack **out,
	git_odb_backend *_backend,
        git_odb *odb,
	git_indexer_progress_cb progress_cb,
	void *progress_payload)
{
	git_indexer_options opts = GIT_INDEXER_OPTIONS_INIT;
	struct pack_backend *backend;
	struct pack_writepack *writepack;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(_backend);

	*out = NULL;

	opts.progress_cb = progress_cb;
	opts.progress_cb_payload = progress_payload;

	backend = (struct pack_backend *)_backend;

	writepack = git__calloc(1, sizeof(struct pack_writepack));
	GIT_ERROR_CHECK_ALLOC(writepack);

#ifdef GIT_EXPERIMENTAL_SHA256
	opts.odb = odb;
	opts.oid_type = backend->opts.oid_type;

	error = git_indexer_new(&writepack->indexer,
		backend->pack_folder,
		&opts);
#else
	error = git_indexer_new(&writepack->indexer,
		backend->pack_folder, 0, odb, &opts);
#endif

	if (error < 0)
		return -1;

	writepack->parent.backend = _backend;
	writepack->parent.append = pack_backend__writepack_append;
	writepack->parent.commit = pack_backend__writepack_commit;
	writepack->parent.free = pack_backend__writepack_free;

	*out = (git_odb_writepack *)writepack;

	return 0;
}

static int get_idx_path(
		git_str *idx_path,
		struct pack_backend *backend,
		struct git_pack_file *p)
{
	size_t path_len;
	int error;

	error = git_fs_path_prettify(idx_path, p->pack_name, backend->pack_folder);
	if (error < 0)
		return error;
	path_len = git_str_len(idx_path);
	if (path_len <= strlen(".pack") || git__suffixcmp(git_str_cstr(idx_path), ".pack") != 0)
		return git_odb__error_notfound("packfile does not end in .pack", NULL, 0);
	path_len -= strlen(".pack");
	error = git_str_splice(idx_path, path_len, strlen(".pack"), ".idx", strlen(".idx"));
	if (error < 0)
		return error;

	return 0;
}

static int pack_backend__writemidx(git_odb_backend *_backend)
{
	struct pack_backend *backend;
	git_midx_writer *w = NULL;
	struct git_pack_file *p;
	size_t i;
	int error = 0;

#ifdef GIT_EXPERIMENTAL_SHA256
	git_midx_writer_options midx_opts = GIT_MIDX_WRITER_OPTIONS_INIT;
#endif

	GIT_ASSERT_ARG(_backend);

	backend = (struct pack_backend *)_backend;

#ifdef GIT_EXPERIMENTAL_SHA256
	midx_opts.oid_type = backend->opts.oid_type;
#endif

	error = git_midx_writer_new(&w, backend->pack_folder
#ifdef GIT_EXPERIMENTAL_SHA256
		, &midx_opts
#endif
		);

	if (error < 0)
		return error;

	git_vector_foreach(&backend->midx_packs, i, p) {
		git_str idx_path = GIT_STR_INIT;
		error = get_idx_path(&idx_path, backend, p);
		if (error < 0)
			goto cleanup;
		error = git_midx_writer_add(w, git_str_cstr(&idx_path));
		git_str_dispose(&idx_path);
		if (error < 0)
			goto cleanup;
	}
	git_vector_foreach(&backend->packs, i, p) {
		git_str idx_path = GIT_STR_INIT;
		error = get_idx_path(&idx_path, backend, p);
		if (error < 0)
			goto cleanup;
		error = git_midx_writer_add(w, git_str_cstr(&idx_path));
		git_str_dispose(&idx_path);
		if (error < 0)
			goto cleanup;
	}

	/*
	 * Invalidate the previous midx before writing the new one.
	 */
	error = remove_multi_pack_index(backend);
	if (error < 0)
		goto cleanup;
	error = git_midx_writer_commit(w);
	if (error < 0)
		goto cleanup;
	error = refresh_multi_pack_index(backend);

cleanup:
	git_midx_writer_free(w);
	return error;
}

static void pack_backend__free(git_odb_backend *_backend)
{
	struct pack_backend *backend;
	struct git_pack_file *p;
	size_t i;

	if (!_backend)
		return;

	backend = (struct pack_backend *)_backend;

	git_vector_foreach(&backend->midx_packs, i, p)
		git_mwindow_put_pack(p);
	git_vector_foreach(&backend->packs, i, p)
		git_mwindow_put_pack(p);

	git_midx_free(backend->midx);
	git_vector_dispose(&backend->midx_packs);
	git_vector_dispose(&backend->packs);
	git__free(backend->pack_folder);
	git__free(backend);
}

static int pack_backend__alloc(
	struct pack_backend **out,
	size_t initial_size,
	const git_odb_backend_pack_options *opts)
{
	struct pack_backend *backend = git__calloc(1, sizeof(struct pack_backend));
	GIT_ERROR_CHECK_ALLOC(backend);

	if (git_vector_init(&backend->midx_packs, 0, NULL) < 0) {
		git__free(backend);
		return -1;
	}

	if (git_vector_init(&backend->packs, initial_size, packfile_sort__cb) < 0) {
		git_vector_dispose(&backend->midx_packs);
		git__free(backend);
		return -1;
	}

	if (opts)
		memcpy(&backend->opts, opts, sizeof(git_odb_backend_pack_options));

	if (!backend->opts.oid_type)
		backend->opts.oid_type = GIT_OID_DEFAULT;

	backend->parent.version = GIT_ODB_BACKEND_VERSION;

	backend->parent.read = &pack_backend__read;
	backend->parent.read_prefix = &pack_backend__read_prefix;
	backend->parent.read_header = &pack_backend__read_header;
	backend->parent.exists = &pack_backend__exists;
	backend->parent.exists_prefix = &pack_backend__exists_prefix;
	backend->parent.refresh = &pack_backend__refresh;
	backend->parent.foreach = &pack_backend__foreach;
	backend->parent.writepack = &pack_backend__writepack;
	backend->parent.writemidx = &pack_backend__writemidx;
	backend->parent.freshen = &pack_backend__freshen;
	backend->parent.free = &pack_backend__free;

	*out = backend;
	return 0;
}

#ifdef GIT_EXPERIMENTAL_SHA256
int git_odb_backend_one_pack(
	git_odb_backend **backend_out,
	const char *idx,
	const git_odb_backend_pack_options *opts)
#else
int git_odb_backend_one_pack(
	git_odb_backend **backend_out,
	const char *idx)
#endif
{
	struct pack_backend *backend = NULL;
	struct git_pack_file *packfile = NULL;

#ifndef GIT_EXPERIMENTAL_SHA256
	git_odb_backend_pack_options *opts = NULL;
#endif

	git_oid_t oid_type = opts ? opts->oid_type : 0;

	if (pack_backend__alloc(&backend, 1, opts) < 0)
		return -1;

	if (git_mwindow_get_pack(&packfile, idx, oid_type) < 0 ||
	    git_vector_insert(&backend->packs, packfile) < 0) {
		pack_backend__free((git_odb_backend *)backend);
		return -1;
	}

	*backend_out = (git_odb_backend *)backend;
	return 0;
}

#ifdef GIT_EXPERIMENTAL_SHA256
int git_odb_backend_pack(
	git_odb_backend **backend_out,
	const char *objects_dir,
	const git_odb_backend_pack_options *opts)
#else
int git_odb_backend_pack(
	git_odb_backend **backend_out,
	const char *objects_dir)
#endif
{
	int error = 0;
	struct pack_backend *backend = NULL;
	git_str path = GIT_STR_INIT;

#ifndef GIT_EXPERIMENTAL_SHA256
	git_odb_backend_pack_options *opts = NULL;
#endif

	if (pack_backend__alloc(&backend, 8, opts) < 0)
		return -1;

	if (!(error = git_str_joinpath(&path, objects_dir, "pack")) &&
	    git_fs_path_isdir(git_str_cstr(&path))) {
		backend->pack_folder = git_str_detach(&path);
		error = pack_backend__refresh((git_odb_backend *)backend);
	}

	if (error < 0) {
		pack_backend__free((git_odb_backend *)backend);
		backend = NULL;
	}

	*backend_out = (git_odb_backend *)backend;

	git_str_dispose(&path);

	return error;
}
