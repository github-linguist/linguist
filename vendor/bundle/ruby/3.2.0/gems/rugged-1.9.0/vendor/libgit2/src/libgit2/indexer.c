/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "indexer.h"

#include "git2/indexer.h"
#include "git2/object.h"

#include "commit.h"
#include "tree.h"
#include "tag.h"
#include "pack.h"
#include "mwindow.h"
#include "posix.h"
#include "pack.h"
#include "filebuf.h"
#include "oid.h"
#include "oidarray.h"
#include "zstream.h"
#include "object.h"
#include "hashmap_oid.h"

size_t git_indexer__max_objects = UINT32_MAX;

#define UINT31_MAX (0x7FFFFFFF)

GIT_HASHMAP_OID_SETUP(git_indexer_oidmap, git_oid *);

struct entry {
	git_oid oid;
	uint32_t crc;
	uint32_t offset;
	uint64_t offset_long;
};

struct git_indexer {
	unsigned int parsed_header :1,
		pack_committed :1,
		have_stream :1,
		have_delta :1,
		do_fsync :1,
		do_verify :1;
	git_oid_t oid_type;
	struct git_pack_header hdr;
	struct git_pack_file *pack;
	unsigned int mode;
	off64_t off;
	off64_t entry_start;
	git_object_t entry_type;
	git_str entry_data;
	git_packfile_stream stream;
	size_t nr_objects;
	git_vector objects;
	git_vector deltas;
	unsigned int fanout[256];
	git_hash_ctx hash_ctx;
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	char name[(GIT_HASH_MAX_SIZE * 2) + 1];
	git_indexer_progress_cb progress_cb;
	void *progress_payload;
	char objbuf[8*1024];

	/* OIDs referenced from pack objects. Used for verification. */
	git_indexer_oidmap expected_oids;

	/* Needed to look up objects which we want to inject to fix a thin pack */
	git_odb *odb;

	/* Fields for calculating the packfile trailer (hash of everything before it) */
	char inbuf[GIT_HASH_MAX_SIZE];
	size_t inbuf_len;
	git_hash_ctx trailer;
};

struct delta_info {
	off64_t delta_off;
};

#ifndef GIT_DEPRECATE_HARD
const git_oid *git_indexer_hash(const git_indexer *idx)
{
	return (git_oid *)idx->checksum;
}
#endif

const char *git_indexer_name(const git_indexer *idx)
{
	return idx->name;
}

static int parse_header(struct git_pack_header *hdr, struct git_pack_file *pack)
{
	int error;
	git_map map;

	if ((error = p_mmap(&map, sizeof(*hdr), GIT_PROT_READ, GIT_MAP_SHARED, pack->mwf.fd, 0)) < 0)
		return error;

	memcpy(hdr, map.data, sizeof(*hdr));
	p_munmap(&map);

	/* Verify we recognize this pack file format. */
	if (hdr->hdr_signature != ntohl(PACK_SIGNATURE)) {
		git_error_set(GIT_ERROR_INDEXER, "wrong pack signature");
		return -1;
	}

	if (!pack_version_ok(hdr->hdr_version)) {
		git_error_set(GIT_ERROR_INDEXER, "wrong pack version");
		return -1;
	}

	return 0;
}

static int objects_cmp(const void *a, const void *b)
{
	const struct entry *entrya = a;
	const struct entry *entryb = b;

	return git_oid__cmp(&entrya->oid, &entryb->oid);
}

int git_indexer_options_init(git_indexer_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_indexer_options, GIT_INDEXER_OPTIONS_INIT);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_indexer_init_options(git_indexer_options *opts, unsigned int version)
{
	return git_indexer_options_init(opts, version);
}
#endif

GIT_INLINE(git_hash_algorithm_t) indexer_hash_algorithm(git_indexer *idx)
{
	switch (idx->oid_type) {
		case GIT_OID_SHA1:
			return GIT_HASH_ALGORITHM_SHA1;
#ifdef GIT_EXPERIMENTAL_SHA256
		case GIT_OID_SHA256:
			return GIT_HASH_ALGORITHM_SHA256;
#endif
	}

	return GIT_HASH_ALGORITHM_NONE;
}

static int indexer_new(
	git_indexer **out,
	const char *prefix,
	git_oid_t oid_type,
	unsigned int mode,
	git_odb *odb,
	git_indexer_options *in_opts)
{
	git_indexer_options opts = GIT_INDEXER_OPTIONS_INIT;
	git_indexer *idx;
	git_str path = GIT_STR_INIT, tmp_path = GIT_STR_INIT;
	static const char suff[] = "/pack";
	git_hash_algorithm_t checksum_type;
	int error, fd = -1;

	if (in_opts)
		memcpy(&opts, in_opts, sizeof(opts));

	if (oid_type)
		GIT_ASSERT_ARG(git_oid_type_is_valid(oid_type));

	idx = git__calloc(1, sizeof(git_indexer));
	GIT_ERROR_CHECK_ALLOC(idx);
	idx->oid_type = oid_type ? oid_type : GIT_OID_DEFAULT;
	idx->odb = odb;
	idx->progress_cb = opts.progress_cb;
	idx->progress_payload = opts.progress_cb_payload;
	idx->mode = mode ? mode : GIT_PACK_FILE_MODE;
	git_str_init(&idx->entry_data, 0);

	checksum_type = indexer_hash_algorithm(idx);

	if ((error = git_hash_ctx_init(&idx->hash_ctx, checksum_type)) < 0 ||
	    (error = git_hash_ctx_init(&idx->trailer, checksum_type)) < 0)
		goto cleanup;

	idx->do_verify = opts.verify;

	if (git_repository__fsync_gitdir)
		idx->do_fsync = 1;

	error = git_str_joinpath(&path, prefix, suff);
	if (error < 0)
		goto cleanup;

	fd = git_futils_mktmp(&tmp_path, git_str_cstr(&path), idx->mode);
	git_str_dispose(&path);
	if (fd < 0)
		goto cleanup;

	error = git_packfile_alloc(&idx->pack, git_str_cstr(&tmp_path), oid_type);
	git_str_dispose(&tmp_path);

	if (error < 0)
		goto cleanup;

	idx->pack->mwf.fd = fd;
	if ((error = git_mwindow_file_register(&idx->pack->mwf)) < 0)
		goto cleanup;

	*out = idx;
	return 0;

cleanup:
	if (fd != -1)
		p_close(fd);

	if (git_str_len(&tmp_path) > 0)
		p_unlink(git_str_cstr(&tmp_path));

	if (idx->pack != NULL)
		p_unlink(idx->pack->pack_name);

	git_str_dispose(&path);
	git_str_dispose(&tmp_path);
	git__free(idx);
	return -1;
}

#ifdef GIT_EXPERIMENTAL_SHA256
int git_indexer_new(
	git_indexer **out,
	const char *prefix,
	git_indexer_options *opts)
{
	return indexer_new(
		out,
		prefix,
		opts ? opts->oid_type : 0,
		opts ? opts->mode : 0,
		opts ? opts->odb : NULL,
		opts);
}
#else
int git_indexer_new(
	git_indexer **out,
	const char *prefix,
	unsigned int mode,
	git_odb *odb,
	git_indexer_options *opts)
{
	return indexer_new(out, prefix, GIT_OID_SHA1, mode, odb, opts);
}
#endif

void git_indexer__set_fsync(git_indexer *idx, int do_fsync)
{
	idx->do_fsync = !!do_fsync;
}

/* Try to store the delta so we can try to resolve it later */
static int store_delta(git_indexer *idx)
{
	struct delta_info *delta;

	delta = git__calloc(1, sizeof(struct delta_info));
	GIT_ERROR_CHECK_ALLOC(delta);
	delta->delta_off = idx->entry_start;

	if (git_vector_insert(&idx->deltas, delta) < 0)
		return -1;

	return 0;
}

static int hash_header(git_hash_ctx *ctx, off64_t len, git_object_t type)
{
	char buffer[64];
	size_t hdrlen;
	int error;

	if ((error = git_odb__format_object_header(&hdrlen,
		buffer, sizeof(buffer), (size_t)len, type)) < 0)
		return error;

	return git_hash_update(ctx, buffer, hdrlen);
}

static int hash_object_stream(git_indexer*idx, git_packfile_stream *stream)
{
	ssize_t read;

	GIT_ASSERT_ARG(idx);
	GIT_ASSERT_ARG(stream);

	do {
		if ((read = git_packfile_stream_read(stream, idx->objbuf, sizeof(idx->objbuf))) < 0)
			break;

		if (idx->do_verify)
			git_str_put(&idx->entry_data, idx->objbuf, read);

		git_hash_update(&idx->hash_ctx, idx->objbuf, read);
	} while (read > 0);

	if (read < 0)
		return (int)read;

	return 0;
}

/* In order to create the packfile stream, we need to skip over the delta base description */
static int advance_delta_offset(git_indexer *idx, git_object_t type)
{
	git_mwindow *w = NULL;

	GIT_ASSERT_ARG(type == GIT_OBJECT_REF_DELTA || type == GIT_OBJECT_OFS_DELTA);

	if (type == GIT_OBJECT_REF_DELTA) {
		idx->off += git_oid_size(idx->oid_type);
	} else {
		off64_t base_off;
		int error = get_delta_base(&base_off, idx->pack, &w, &idx->off, type, idx->entry_start);
		git_mwindow_close(&w);
		if (error < 0)
			return error;
	}

	return 0;
}

/* Read from the stream and discard any output */
static int read_object_stream(git_indexer *idx, git_packfile_stream *stream)
{
	ssize_t read;

	GIT_ASSERT_ARG(stream);

	do {
		read = git_packfile_stream_read(stream, idx->objbuf, sizeof(idx->objbuf));
	} while (read > 0);

	if (read < 0)
		return (int)read;

	return 0;
}

static int crc_object(uint32_t *crc_out, git_mwindow_file *mwf, off64_t start, off64_t size)
{
	void *ptr;
	uint32_t crc;
	unsigned int left, len;
	git_mwindow *w = NULL;

	crc = crc32(0L, Z_NULL, 0);
	while (size) {
		ptr = git_mwindow_open(mwf, &w, start, (size_t)size, &left);
		if (ptr == NULL)
			return -1;

		len = min(left, (unsigned int)size);
		crc = crc32(crc, ptr, len);
		size -= len;
		start += len;
		git_mwindow_close(&w);
	}

	*crc_out = htonl(crc);
	return 0;
}

static int add_expected_oid(git_indexer *idx, const git_oid *oid)
{
	/*
	 * If we know about that object because it is stored in our ODB or
	 * because we have already processed it as part of our pack file, we do
	 * not have to expect it.
	 */
	if ((!idx->odb || !git_odb_exists(idx->odb, oid)) &&
	    !git_pack_oidmap_contains(&idx->pack->idx_cache, oid) &&
	    !git_indexer_oidmap_contains(&idx->expected_oids, oid)) {
		    git_oid *dup = git__malloc(sizeof(*oid));
		    GIT_ERROR_CHECK_ALLOC(dup);
		    git_oid_cpy(dup, oid);
		    return git_indexer_oidmap_put(&idx->expected_oids, dup, dup);
	}

	return 0;
}

static int check_object_connectivity(git_indexer *idx, const git_rawobj *obj)
{
	git_object *object;
	git_oid *expected;
	int error = 0;

	if (obj->type != GIT_OBJECT_BLOB &&
	    obj->type != GIT_OBJECT_TREE &&
	    obj->type != GIT_OBJECT_COMMIT &&
	    obj->type != GIT_OBJECT_TAG)
		return 0;

	if (git_object__from_raw(&object, obj->data, obj->len, obj->type, idx->oid_type) < 0) {
		/*
		 * parse_raw returns EINVALID on invalid data; downgrade
		 * that to a normal -1 error code.
		 */
		error = -1;
		goto out;
	}

	if (git_indexer_oidmap_get(&expected, &idx->expected_oids, &object->cached.oid) == 0) {
		git_indexer_oidmap_remove(&idx->expected_oids, &object->cached.oid);
		git__free(expected);
	}

	/*
	 * Check whether this is a known object. If so, we can just continue as
	 * we assume that the ODB has a complete graph.
	 */
	if (idx->odb && git_odb_exists(idx->odb, &object->cached.oid))
		return 0;

	switch (obj->type) {
		case GIT_OBJECT_TREE:
		{
			git_tree *tree = (git_tree *) object;
			git_tree_entry *entry;
			size_t i;

			git_array_foreach(tree->entries, i, entry)
				if (add_expected_oid(idx, &entry->oid) < 0)
					goto out;

			break;
		}
		case GIT_OBJECT_COMMIT:
		{
			git_commit *commit = (git_commit *) object;
			git_oid *parent_oid;
			size_t i;

			git_array_foreach(commit->parent_ids, i, parent_oid)
				if (add_expected_oid(idx, parent_oid) < 0)
					goto out;

			if (add_expected_oid(idx, &commit->tree_id) < 0)
				goto out;

			break;
		}
		case GIT_OBJECT_TAG:
		{
			git_tag *tag = (git_tag *) object;

			if (add_expected_oid(idx, &tag->target) < 0)
				goto out;

			break;
		}
		case GIT_OBJECT_BLOB:
		default:
			break;
	}

out:
	git_object_free(object);

	return error;
}

static int store_object(git_indexer *idx)
{
	int i, error;
	git_oid oid;
	struct entry *entry;
	off64_t entry_size;
	struct git_pack_entry *pentry;
	off64_t entry_start = idx->entry_start;

	entry = git__calloc(1, sizeof(*entry));
	GIT_ERROR_CHECK_ALLOC(entry);

	pentry = git__calloc(1, sizeof(struct git_pack_entry));
	GIT_ERROR_CHECK_ALLOC(pentry);

	if (git_hash_final(oid.id, &idx->hash_ctx)) {
		git__free(pentry);
		goto on_error;
	}

#ifdef GIT_EXPERIMENTAL_SHA256
	oid.type = idx->oid_type;
#endif

	entry_size = idx->off - entry_start;
	if (entry_start > UINT31_MAX) {
		entry->offset = UINT32_MAX;
		entry->offset_long = entry_start;
	} else {
		entry->offset = (uint32_t)entry_start;
	}

	if (idx->do_verify) {
		git_rawobj rawobj = {
		    idx->entry_data.ptr,
		    idx->entry_data.size,
		    idx->entry_type
		};

		if ((error = check_object_connectivity(idx, &rawobj)) < 0)
			goto on_error;
	}

	git_oid_cpy(&pentry->id, &oid);
	pentry->offset = entry_start;

	if (git_pack_oidmap_contains(&idx->pack->idx_cache, &pentry->id)) {
		const char *idstr = git_oid_tostr_s(&pentry->id);

		if (!idstr)
			git_error_set(GIT_ERROR_INDEXER, "failed to parse object id");
		else
			git_error_set(GIT_ERROR_INDEXER, "duplicate object %s found in pack", idstr);

		git__free(pentry);
		goto on_error;
	}

	if ((error = git_pack_oidmap_put(&idx->pack->idx_cache, &pentry->id, pentry)) < 0) {
		git__free(pentry);
		git_error_set_oom();
		goto on_error;
	}

	git_oid_cpy(&entry->oid, &oid);

	if (crc_object(&entry->crc, &idx->pack->mwf, entry_start, entry_size) < 0)
		goto on_error;

	/* Add the object to the list */
	if (git_vector_insert(&idx->objects, entry) < 0)
		goto on_error;

	for (i = oid.id[0]; i < 256; ++i) {
		idx->fanout[i]++;
	}

	return 0;

on_error:
	git__free(entry);

	return -1;
}

GIT_INLINE(bool) has_entry(git_indexer *idx, git_oid *id)
{
	return git_pack_oidmap_contains(&idx->pack->idx_cache, id);
}

static int save_entry(git_indexer *idx, struct entry *entry, struct git_pack_entry *pentry, off64_t entry_start)
{
	int i;

	if (entry_start > UINT31_MAX) {
		entry->offset = UINT32_MAX;
		entry->offset_long = entry_start;
	} else {
		entry->offset = (uint32_t)entry_start;
	}

	pentry->offset = entry_start;

	if (git_pack_oidmap_contains(&idx->pack->idx_cache, &pentry->id) ||
	    git_pack_oidmap_put(&idx->pack->idx_cache, &pentry->id, pentry) < 0) {
		git_error_set(GIT_ERROR_INDEXER, "cannot insert object into pack");
		return -1;
	}

	/* Add the object to the list */
	if (git_vector_insert(&idx->objects, entry) < 0)
		return -1;

	for (i = entry->oid.id[0]; i < 256; ++i) {
		idx->fanout[i]++;
	}

	return 0;
}

static int hash_and_save(git_indexer *idx, git_rawobj *obj, off64_t entry_start)
{
	git_oid oid;
	size_t entry_size;
	struct entry *entry;
	struct git_pack_entry *pentry = NULL;

	entry = git__calloc(1, sizeof(*entry));
	GIT_ERROR_CHECK_ALLOC(entry);

	if (git_odb__hashobj(&oid, obj, idx->oid_type) < 0) {
		git_error_set(GIT_ERROR_INDEXER, "failed to hash object");
		goto on_error;
	}

	pentry = git__calloc(1, sizeof(struct git_pack_entry));
	GIT_ERROR_CHECK_ALLOC(pentry);

	git_oid_cpy(&pentry->id, &oid);
	git_oid_cpy(&entry->oid, &oid);
	entry->crc = crc32(0L, Z_NULL, 0);

	entry_size = (size_t)(idx->off - entry_start);
	if (crc_object(&entry->crc, &idx->pack->mwf, entry_start, entry_size) < 0)
		goto on_error;

	return save_entry(idx, entry, pentry, entry_start);

on_error:
	git__free(pentry);
	git__free(entry);
	git__free(obj->data);
	return -1;
}

static int do_progress_callback(git_indexer *idx, git_indexer_progress *stats)
{
	if (idx->progress_cb)
		return git_error_set_after_callback_function(
			idx->progress_cb(stats, idx->progress_payload),
			"indexer progress");
	return 0;
}

/* Hash everything but the checksum trailer */
static void hash_partially(git_indexer *idx, const uint8_t *data, size_t size)
{
	size_t to_expell, to_keep;
	size_t oid_size = git_oid_size(idx->oid_type);

	if (size == 0)
		return;

	/*
	 * Easy case, dump the buffer and the data minus the trailing
	 * checksum (SHA1 or SHA256).
	 */
	if (size >= oid_size) {
		git_hash_update(&idx->trailer, idx->inbuf, idx->inbuf_len);
		git_hash_update(&idx->trailer, data, size - oid_size);

		data += size - oid_size;
		memcpy(idx->inbuf, data, oid_size);
		idx->inbuf_len = oid_size;
		return;
	}

	/* We can just append */
	if (idx->inbuf_len + size <= oid_size) {
		memcpy(idx->inbuf + idx->inbuf_len, data, size);
		idx->inbuf_len += size;
		return;
	}

	/* We need to partially drain the buffer and then append */
	to_keep   = oid_size - size;
	to_expell = idx->inbuf_len - to_keep;

	git_hash_update(&idx->trailer, idx->inbuf, to_expell);

	memmove(idx->inbuf, idx->inbuf + to_expell, to_keep);
	memcpy(idx->inbuf + to_keep, data, size);
	idx->inbuf_len += size - to_expell;
}

#if defined(NO_MMAP) || !defined(GIT_WIN32)

static int write_at(git_indexer *idx, const void *data, off64_t offset, size_t size)
{
	size_t remaining_size = size;
	const char *ptr = (const char *)data;

	/* Handle data size larger that ssize_t */
	while (remaining_size > 0) {
		ssize_t nb;
		HANDLE_EINTR(nb, p_pwrite(idx->pack->mwf.fd, (void *)ptr,
					  remaining_size, offset));
		if (nb <= 0)
			return -1;

		ptr += nb;
		offset += nb;
		remaining_size -= nb;
	}

	return 0;
}

static int append_to_pack(git_indexer *idx, const void *data, size_t size)
{
	if (write_at(idx, data, idx->pack->mwf.size, size) < 0) {
		git_error_set(GIT_ERROR_OS, "cannot extend packfile '%s'", idx->pack->pack_name);
		return -1;
	}

	return 0;
}

#else

/*
 * Windows may keep different views to a networked file for the mmap- and
 * open-accessed versions of a file, so any writes done through
 * `write(2)`/`pwrite(2)` may not be reflected on the data that `mmap(2)` is
 * able to read.
 */

static int write_at(git_indexer *idx, const void *data, off64_t offset, size_t size)
{
	git_file fd = idx->pack->mwf.fd;
	size_t mmap_alignment;
	size_t page_offset;
	off64_t page_start;
	unsigned char *map_data;
	git_map map;
	int error;

	GIT_ASSERT_ARG(data);
	GIT_ASSERT_ARG(size);

	if ((error = git__mmap_alignment(&mmap_alignment)) < 0)
		return error;

	/* the offset needs to be at the mmap boundary for the platform */
	page_offset = offset % mmap_alignment;
	page_start = offset - page_offset;

	if ((error = p_mmap(&map, page_offset + size, GIT_PROT_WRITE, GIT_MAP_SHARED, fd, page_start)) < 0)
		return error;

	map_data = (unsigned char *)map.data;
	memcpy(map_data + page_offset, data, size);
	p_munmap(&map);

	return 0;
}

static int append_to_pack(git_indexer *idx, const void *data, size_t size)
{
	off64_t new_size;
	size_t mmap_alignment;
	size_t page_offset;
	off64_t page_start;
	off64_t current_size = idx->pack->mwf.size;
	int error;

	if (!size)
		return 0;

	if ((error = git__mmap_alignment(&mmap_alignment)) < 0)
		return error;

	/* Write a single byte to force the file system to allocate space now or
	 * report an error, since we can't report errors when writing using mmap.
	 * Round the size up to the nearest page so that we only need to perform file
	 * I/O when we add a page, instead of whenever we write even a single byte. */
	new_size = current_size + size;
	page_offset = new_size % mmap_alignment;
	page_start = new_size - page_offset;

	if (p_pwrite(idx->pack->mwf.fd, data, 1, page_start + mmap_alignment - 1) < 0) {
		git_error_set(GIT_ERROR_OS, "cannot extend packfile '%s'", idx->pack->pack_name);
		return -1;
	}

	return write_at(idx, data, idx->pack->mwf.size, size);
}

#endif

static int read_stream_object(git_indexer *idx, git_indexer_progress *stats)
{
	git_packfile_stream *stream = &idx->stream;
	off64_t entry_start = idx->off;
	size_t oid_size, entry_size;
	git_object_t type;
	git_mwindow *w = NULL;
	int error;

	oid_size = git_oid_size(idx->oid_type);

	if (idx->pack->mwf.size <= idx->off + (long long)oid_size)
		return GIT_EBUFS;

	if (!idx->have_stream) {
		error = git_packfile_unpack_header(&entry_size, &type, idx->pack, &w, &idx->off);
		if (error == GIT_EBUFS) {
			idx->off = entry_start;
			return error;
		}
		if (error < 0)
			return error;

		git_mwindow_close(&w);
		idx->entry_start = entry_start;
		git_hash_init(&idx->hash_ctx);
		git_str_clear(&idx->entry_data);

		if (type == GIT_OBJECT_REF_DELTA || type == GIT_OBJECT_OFS_DELTA) {
			error = advance_delta_offset(idx, type);
			if (error == GIT_EBUFS) {
				idx->off = entry_start;
				return error;
			}
			if (error < 0)
				return error;

			idx->have_delta = 1;
		} else {
			idx->have_delta = 0;

			error = hash_header(&idx->hash_ctx, entry_size, type);
			if (error < 0)
				return error;
		}

		idx->have_stream = 1;
		idx->entry_type = type;

		error = git_packfile_stream_open(stream, idx->pack, idx->off);
		if (error < 0)
			return error;
	}

	if (idx->have_delta) {
		error = read_object_stream(idx, stream);
	} else {
		error = hash_object_stream(idx, stream);
	}

	idx->off = stream->curpos;
	if (error == GIT_EBUFS)
		return error;

	/* We want to free the stream reasorces no matter what here */
	idx->have_stream = 0;
	git_packfile_stream_dispose(stream);

	if (error < 0)
		return error;

	if (idx->have_delta) {
		error = store_delta(idx);
	} else {
		error = store_object(idx);
	}

	if (error < 0)
		return error;

	if (!idx->have_delta) {
		stats->indexed_objects++;
	}
	stats->received_objects++;

	if ((error = do_progress_callback(idx, stats)) != 0)
		return error;

	return 0;
}

int git_indexer_append(git_indexer *idx, const void *data, size_t size, git_indexer_progress *stats)
{
	int error = -1;
	struct git_pack_header *hdr = &idx->hdr;
	git_mwindow_file *mwf = &idx->pack->mwf;

	GIT_ASSERT_ARG(idx);
	GIT_ASSERT_ARG(data);
	GIT_ASSERT_ARG(stats);

	if ((error = append_to_pack(idx, data, size)) < 0)
		return error;

	hash_partially(idx, data, (int)size);

	/* Make sure we set the new size of the pack */
	idx->pack->mwf.size += size;

	if (!idx->parsed_header) {
		unsigned int total_objects;

		if ((unsigned)idx->pack->mwf.size < sizeof(struct git_pack_header))
			return 0;

		if ((error = parse_header(&idx->hdr, idx->pack)) < 0)
			return error;

		idx->parsed_header = 1;
		idx->nr_objects = ntohl(hdr->hdr_entries);
		idx->off = sizeof(struct git_pack_header);

		if (idx->nr_objects <= git_indexer__max_objects) {
			total_objects = (unsigned int)idx->nr_objects;
		} else {
			git_error_set(GIT_ERROR_INDEXER, "too many objects");
			return -1;
		}

		idx->pack->has_cache = 1;
		if (git_vector_init(&idx->objects, total_objects, objects_cmp) < 0)
			return -1;

		if (git_vector_init(&idx->deltas, total_objects / 2, NULL) < 0)
			return -1;

		stats->received_objects = 0;
		stats->local_objects = 0;
		stats->total_deltas = 0;
		stats->indexed_deltas = 0;
		stats->indexed_objects = 0;
		stats->total_objects = total_objects;

		if ((error = do_progress_callback(idx, stats)) != 0)
			return error;
	}

	/* Now that we have data in the pack, let's try to parse it */

	/* As the file grows any windows we try to use will be out of date */
	if ((error = git_mwindow_free_all(mwf)) < 0)
		goto on_error;

	while (stats->indexed_objects < idx->nr_objects) {
		if ((error = read_stream_object(idx, stats)) != 0) {
			if (error == GIT_EBUFS)
				break;
			else
				goto on_error;
		}
	}

	return 0;

on_error:
	git_mwindow_free_all(mwf);
	return error;
}

static int index_path(git_str *path, git_indexer *idx, const char *suffix)
{
	const char prefix[] = "pack-";
	size_t slash = (size_t)path->size;

	/* search backwards for '/' */
	while (slash > 0 && path->ptr[slash - 1] != '/')
		slash--;

	if (git_str_grow(path, slash + 1 + strlen(prefix) +
		git_oid_hexsize(idx->oid_type) + strlen(suffix) + 1) < 0)
		return -1;

	git_str_truncate(path, slash);
	git_str_puts(path, prefix);
	git_str_puts(path, idx->name);
	git_str_puts(path, suffix);

	return git_str_oom(path) ? -1 : 0;
}

/**
 * Rewind the packfile by the trailer, as we might need to fix the
 * packfile by injecting objects at the tail and must overwrite it.
 */
static int seek_back_trailer(git_indexer *idx)
{
	idx->pack->mwf.size -= git_oid_size(idx->oid_type);
	return git_mwindow_free_all(&idx->pack->mwf);
}

static int inject_object(git_indexer *idx, git_oid *id)
{
	git_odb_object *obj = NULL;
	struct entry *entry = NULL;
	struct git_pack_entry *pentry = NULL;
	unsigned char empty_checksum[GIT_HASH_MAX_SIZE] = {0};
	unsigned char hdr[64];
	git_str buf = GIT_STR_INIT;
	off64_t entry_start;
	const void *data;
	size_t len, hdr_len;
	size_t checksum_size;
	int error;

	checksum_size = git_hash_size(indexer_hash_algorithm(idx));

	if ((error = seek_back_trailer(idx)) < 0)
		goto cleanup;

	entry_start = idx->pack->mwf.size;

	if ((error = git_odb_read(&obj, idx->odb, id)) < 0) {
		git_error_set(GIT_ERROR_INDEXER, "missing delta bases");
		goto cleanup;
	}

	data = git_odb_object_data(obj);
	len = git_odb_object_size(obj);

	entry = git__calloc(1, sizeof(*entry));
	GIT_ERROR_CHECK_ALLOC(entry);

	entry->crc = crc32(0L, Z_NULL, 0);

	/* Write out the object header */
	if ((error = git_packfile__object_header(&hdr_len, hdr, len, git_odb_object_type(obj))) < 0 ||
	    (error = append_to_pack(idx, hdr, hdr_len)) < 0)
		goto cleanup;

	idx->pack->mwf.size += hdr_len;
	entry->crc = crc32(entry->crc, hdr, (uInt)hdr_len);

	if ((error = git_zstream_deflatebuf(&buf, data, len)) < 0)
		goto cleanup;

	/* And then the compressed object */
	if ((error = append_to_pack(idx, buf.ptr, buf.size)) < 0)
		goto cleanup;

	idx->pack->mwf.size += buf.size;
	entry->crc = htonl(crc32(entry->crc, (unsigned char *)buf.ptr, (uInt)buf.size));
	git_str_dispose(&buf);

	/* Write a fake trailer so the pack functions play ball */

	if ((error = append_to_pack(idx, empty_checksum, checksum_size)) < 0)
		goto cleanup;

	idx->pack->mwf.size += git_oid_size(idx->oid_type);

	pentry = git__calloc(1, sizeof(struct git_pack_entry));
	GIT_ERROR_CHECK_ALLOC(pentry);

	git_oid_cpy(&pentry->id, id);
	git_oid_cpy(&entry->oid, id);
	idx->off = entry_start + hdr_len + len;

	error = save_entry(idx, entry, pentry, entry_start);

cleanup:
	if (error) {
		git__free(entry);
		git__free(pentry);
	}

	git_odb_object_free(obj);
	return error;
}

static int fix_thin_pack(git_indexer *idx, git_indexer_progress *stats)
{
	int error, found_ref_delta = 0;
	unsigned int i;
	struct delta_info *delta;
	size_t size;
	git_object_t type;
	git_mwindow *w = NULL;
	off64_t curpos = 0;
	unsigned char *base_info;
	unsigned int left = 0;
	git_oid base;

	GIT_ASSERT(git_vector_length(&idx->deltas) > 0);

	if (idx->odb == NULL) {
		git_error_set(GIT_ERROR_INDEXER, "cannot fix a thin pack without an ODB");
		return -1;
	}

	/* Loop until we find the first REF delta */
	git_vector_foreach(&idx->deltas, i, delta) {
		if (!delta)
			continue;

		curpos = delta->delta_off;
		error = git_packfile_unpack_header(&size, &type, idx->pack, &w, &curpos);
		if (error < 0)
			return error;

		if (type == GIT_OBJECT_REF_DELTA) {
			found_ref_delta = 1;
			break;
		}
	}

	if (!found_ref_delta) {
		git_error_set(GIT_ERROR_INDEXER, "no REF_DELTA found, cannot inject object");
		return -1;
	}

	/* curpos now points to the base information, which is an OID */
	base_info = git_mwindow_open(&idx->pack->mwf, &w, curpos, git_oid_size(idx->oid_type), &left);
	if (base_info == NULL) {
		git_error_set(GIT_ERROR_INDEXER, "failed to map delta information");
		return -1;
	}

	git_oid__fromraw(&base, base_info, idx->oid_type);
	git_mwindow_close(&w);

	if (has_entry(idx, &base))
		return 0;

	if (inject_object(idx, &base) < 0)
		return -1;

	stats->local_objects++;

	return 0;
}

static int resolve_deltas(git_indexer *idx, git_indexer_progress *stats)
{
	unsigned int i;
	int error;
	struct delta_info *delta;
	int progressed = 0, non_null = 0, progress_cb_result;

	while (idx->deltas.length > 0) {
		progressed = 0;
		non_null = 0;
		git_vector_foreach(&idx->deltas, i, delta) {
			git_rawobj obj = {0};

			if (!delta)
				continue;

			non_null = 1;
			idx->off = delta->delta_off;
			if ((error = git_packfile_unpack(&obj, idx->pack, &idx->off)) < 0) {
				if (error == GIT_PASSTHROUGH) {
					/* We have not seen the base object, we'll try again later. */
					continue;
				}
				return -1;
			}

			if (idx->do_verify && check_object_connectivity(idx, &obj) < 0)
				/* TODO: error? continue? */
				continue;

			if (hash_and_save(idx, &obj, delta->delta_off) < 0)
				continue;

			git__free(obj.data);
			stats->indexed_objects++;
			stats->indexed_deltas++;
			progressed = 1;
			if ((progress_cb_result = do_progress_callback(idx, stats)) < 0)
				return progress_cb_result;

			/* remove from the list */
			git_vector_set(NULL, &idx->deltas, i, NULL);
			git__free(delta);
		}

		/* if none were actually set, we're done */
		if (!non_null)
			break;

		if (!progressed && (fix_thin_pack(idx, stats) < 0)) {
			return -1;
		}
	}

	return 0;
}

static int update_header_and_rehash(git_indexer *idx, git_indexer_progress *stats)
{
	void *ptr;
	size_t chunk = 1024*1024;
	off64_t hashed = 0;
	git_mwindow *w = NULL;
	git_mwindow_file *mwf;
	unsigned int left;

	mwf = &idx->pack->mwf;

	git_hash_init(&idx->trailer);


	/* Update the header to include the number of local objects we injected */
	idx->hdr.hdr_entries = htonl(stats->total_objects + stats->local_objects);
	if (write_at(idx, &idx->hdr, 0, sizeof(struct git_pack_header)) < 0)
		return -1;

	/*
	 * We now use the same technique as before to determine the
	 * hash. We keep reading up to the end and let
	 * hash_partially() keep the existing trailer out of the
	 * calculation.
	 */
	if (git_mwindow_free_all(mwf) < 0)
		return -1;

	idx->inbuf_len = 0;
	while (hashed < mwf->size) {
		ptr = git_mwindow_open(mwf, &w, hashed, chunk, &left);
		if (ptr == NULL)
			return -1;

		hash_partially(idx, ptr, left);
		hashed += left;

		git_mwindow_close(&w);
	}

	return 0;
}

int git_indexer_commit(git_indexer *idx, git_indexer_progress *stats)
{
	git_mwindow *w = NULL;
	unsigned int i, long_offsets = 0, left;
	int error;
	struct git_pack_idx_header hdr;
	git_str filename = GIT_STR_INIT;
	struct entry *entry;
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	git_filebuf index_file = {0};
	void *packfile_trailer;
	size_t checksum_size;
	int filebuf_hash;
	bool mismatch;

	if (!idx->parsed_header) {
		git_error_set(GIT_ERROR_INDEXER, "incomplete pack header");
		return -1;
	}

	checksum_size = git_hash_size(indexer_hash_algorithm(idx));
	filebuf_hash = git_filebuf_hash_flags(indexer_hash_algorithm(idx));
	GIT_ASSERT(checksum_size);

	/* Test for this before resolve_deltas(), as it plays with idx->off */
	if (idx->off + (ssize_t)checksum_size < idx->pack->mwf.size) {
		git_error_set(GIT_ERROR_INDEXER, "unexpected data at the end of the pack");
		return -1;
	}
	if (idx->off + (ssize_t)checksum_size > idx->pack->mwf.size) {
		git_error_set(GIT_ERROR_INDEXER, "missing trailer at the end of the pack");
		return -1;
	}

	packfile_trailer = git_mwindow_open(&idx->pack->mwf, &w, idx->pack->mwf.size - checksum_size, checksum_size, &left);
	if (packfile_trailer == NULL) {
		git_mwindow_close(&w);
		goto on_error;
	}

	/* Compare the packfile trailer as it was sent to us and what we calculated */
	git_hash_final(checksum, &idx->trailer);
	mismatch = !!memcmp(checksum, packfile_trailer, checksum_size);
	git_mwindow_close(&w);

	if (mismatch) {
		git_error_set(GIT_ERROR_INDEXER, "packfile trailer mismatch");
		return -1;
	}

	/* Freeze the number of deltas */
	stats->total_deltas = stats->total_objects - stats->indexed_objects;

	if ((error = resolve_deltas(idx, stats)) < 0)
		return error;

	if (stats->indexed_objects != stats->total_objects) {
		git_error_set(GIT_ERROR_INDEXER, "early EOF");
		return -1;
	}

	if (stats->local_objects > 0) {
		if (update_header_and_rehash(idx, stats) < 0)
			return -1;

		git_hash_final(checksum, &idx->trailer);
		write_at(idx, checksum, idx->pack->mwf.size - checksum_size, checksum_size);
	}

	/*
	 * Is the resulting graph fully connected or are we still
	 * missing some objects? In the second case, we can
	 * bail out due to an incomplete and thus corrupt
	 * packfile.
	 */
	if (git_indexer_oidmap_size(&idx->expected_oids) > 0) {
		git_error_set(GIT_ERROR_INDEXER, "packfile is missing %"PRIuZ" objects",
			(size_t)git_indexer_oidmap_size(&idx->expected_oids));
		return -1;
	}

	git_vector_sort(&idx->objects);

	/* Use the trailer hash as the pack file name to ensure
	 * files with different contents have different names */
	memcpy(idx->checksum, checksum, checksum_size);
	if (git_hash_fmt(idx->name, checksum, checksum_size) < 0)
		return -1;

	git_str_sets(&filename, idx->pack->pack_name);
	git_str_shorten(&filename, strlen("pack"));
	git_str_puts(&filename, "idx");
	if (git_str_oom(&filename))
		return -1;

	if (git_filebuf_open(&index_file, filename.ptr,
		filebuf_hash | (idx->do_fsync ? GIT_FILEBUF_FSYNC : 0),
		idx->mode) < 0)
		goto on_error;

	/* Write out the header */
	hdr.idx_signature = htonl(PACK_IDX_SIGNATURE);
	hdr.idx_version = htonl(2);
	git_filebuf_write(&index_file, &hdr, sizeof(hdr));

	/* Write out the fanout table */
	for (i = 0; i < 256; ++i) {
		uint32_t n = htonl(idx->fanout[i]);
		git_filebuf_write(&index_file, &n, sizeof(n));
	}

	/* Write out the object names (SHA-1 hashes) */
	git_vector_foreach(&idx->objects, i, entry) {
		git_filebuf_write(&index_file, &entry->oid.id, git_oid_size(idx->oid_type));
	}

	/* Write out the CRC32 values */
	git_vector_foreach(&idx->objects, i, entry) {
		git_filebuf_write(&index_file, &entry->crc, sizeof(uint32_t));
	}

	/* Write out the offsets */
	git_vector_foreach(&idx->objects, i, entry) {
		uint32_t n;

		if (entry->offset == UINT32_MAX)
			n = htonl(0x80000000 | long_offsets++);
		else
			n = htonl(entry->offset);

		git_filebuf_write(&index_file, &n, sizeof(uint32_t));
	}

	/* Write out the long offsets */
	git_vector_foreach(&idx->objects, i, entry) {
		uint32_t split[2];

		if (entry->offset != UINT32_MAX)
			continue;

		split[0] = htonl(entry->offset_long >> 32);
		split[1] = htonl(entry->offset_long & 0xffffffff);

		git_filebuf_write(&index_file, &split, sizeof(uint32_t) * 2);
	}

	/* Write out the packfile trailer to the index */
	if (git_filebuf_write(&index_file, checksum, checksum_size) < 0)
		goto on_error;

	/* Write out the hash of the idx */
	if (git_filebuf_hash(checksum, &index_file) < 0)
		goto on_error;

	git_filebuf_write(&index_file, checksum, checksum_size);

	/* Figure out what the final name should be */
	if (index_path(&filename, idx, ".idx") < 0)
		goto on_error;

	/* Commit file */
	if (git_filebuf_commit_at(&index_file, filename.ptr) < 0)
		goto on_error;

	if (git_mwindow_free_all(&idx->pack->mwf) < 0)
		goto on_error;

#if !defined(NO_MMAP) && defined(GIT_WIN32)
	/*
	 * Some non-Windows remote filesystems fail when truncating files if the
	 * file permissions change after opening the file (done by p_mkstemp).
	 *
	 * Truncation is only needed when mmap is used to undo rounding up to next
	 * page_size in append_to_pack.
	 */
	if (p_ftruncate(idx->pack->mwf.fd, idx->pack->mwf.size) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to truncate pack file '%s'", idx->pack->pack_name);
		return -1;
	}
#endif

	if (idx->do_fsync && p_fsync(idx->pack->mwf.fd) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to fsync packfile");
		goto on_error;
	}

	/* We need to close the descriptor here so Windows doesn't choke on commit_at */
	if (p_close(idx->pack->mwf.fd) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to close packfile");
		goto on_error;
	}

	idx->pack->mwf.fd = -1;

	if (index_path(&filename, idx, ".pack") < 0)
		goto on_error;

	/* And don't forget to rename the packfile to its new place. */
	if (p_rename(idx->pack->pack_name, git_str_cstr(&filename)) < 0)
		goto on_error;

	/* And fsync the parent directory if we're asked to. */
	if (idx->do_fsync &&
		git_futils_fsync_parent(git_str_cstr(&filename)) < 0)
		goto on_error;

	idx->pack_committed = 1;

	git_str_dispose(&filename);
	return 0;

on_error:
	git_mwindow_free_all(&idx->pack->mwf);
	git_filebuf_cleanup(&index_file);
	git_str_dispose(&filename);
	return -1;
}

void git_indexer_free(git_indexer *idx)
{
	struct git_pack_entry *pentry;
	git_oid *id;
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;

	if (idx == NULL)
		return;

	if (idx->have_stream)
		git_packfile_stream_dispose(&idx->stream);

	git_vector_dispose_deep(&idx->objects);

	while (git_pack_oidmap_iterate(&iter, NULL, &pentry, &idx->pack->idx_cache) == 0)
		git__free(pentry);

	git_pack_oidmap_dispose(&idx->pack->idx_cache);

	git_vector_dispose_deep(&idx->deltas);

	git_packfile_free(idx->pack, !idx->pack_committed);

	iter = GIT_HASHMAP_ITER_INIT;
	while (git_indexer_oidmap_iterate(&iter, NULL, &id, &idx->expected_oids) == 0)
		git__free(id);

	git_hash_ctx_cleanup(&idx->trailer);
	git_hash_ctx_cleanup(&idx->hash_ctx);
	git_str_dispose(&idx->entry_data);
	git_indexer_oidmap_dispose(&idx->expected_oids);
	git__free(idx);
}
