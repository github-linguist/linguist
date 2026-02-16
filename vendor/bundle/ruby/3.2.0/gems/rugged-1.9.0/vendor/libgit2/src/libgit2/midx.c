/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "midx.h"

#include "array.h"
#include "buf.h"
#include "filebuf.h"
#include "futils.h"
#include "hash.h"
#include "odb.h"
#include "pack.h"
#include "fs_path.h"
#include "repository.h"
#include "str.h"

#define MIDX_SIGNATURE 0x4d494458 /* "MIDX" */
#define MIDX_VERSION 1
#define MIDX_OBJECT_ID_VERSION 1
struct git_midx_header {
	uint32_t signature;
	uint8_t version;
	uint8_t object_id_version;
	uint8_t chunks;
	uint8_t base_midx_files;
	uint32_t packfiles;
};

#define MIDX_PACKFILE_NAMES_ID 0x504e414d	   /* "PNAM" */
#define MIDX_OID_FANOUT_ID 0x4f494446	   /* "OIDF" */
#define MIDX_OID_LOOKUP_ID 0x4f49444c	   /* "OIDL" */
#define MIDX_OBJECT_OFFSETS_ID 0x4f4f4646	   /* "OOFF" */
#define MIDX_OBJECT_LARGE_OFFSETS_ID 0x4c4f4646 /* "LOFF" */

struct git_midx_chunk {
	off64_t offset;
	size_t length;
};

typedef int (*midx_write_cb)(const char *buf, size_t size, void *cb_data);

static int midx_error(const char *message)
{
	git_error_set(GIT_ERROR_ODB, "invalid multi-pack-index file - %s", message);
	return -1;
}

static int midx_parse_packfile_names(
		git_midx_file *idx,
		const unsigned char *data,
		uint32_t packfiles,
		struct git_midx_chunk *chunk)
{
	int error;
	uint32_t i;
	char *packfile_name = (char *)(data + chunk->offset);
	size_t chunk_size = chunk->length, len;
	if (chunk->offset == 0)
		return midx_error("missing Packfile Names chunk");
	if (chunk->length == 0)
		return midx_error("empty Packfile Names chunk");
	if ((error = git_vector_init(&idx->packfile_names, packfiles, git__strcmp_cb)) < 0)
		return error;
	for (i = 0; i < packfiles; ++i) {
		len = p_strnlen(packfile_name, chunk_size);
		if (len == 0)
			return midx_error("empty packfile name");
		if (len + 1 > chunk_size)
			return midx_error("unterminated packfile name");
		git_vector_insert(&idx->packfile_names, packfile_name);
		if (i && strcmp(git_vector_get(&idx->packfile_names, i - 1), packfile_name) >= 0)
			return midx_error("packfile names are not sorted");
		if (strlen(packfile_name) <= strlen(".idx") || git__suffixcmp(packfile_name, ".idx") != 0)
			return midx_error("non-.idx packfile name");
		if (strchr(packfile_name, '/') != NULL || strchr(packfile_name, '\\') != NULL)
			return midx_error("non-local packfile");
		packfile_name += len + 1;
		chunk_size -= len + 1;
	}
	return 0;
}

static int midx_parse_oid_fanout(
		git_midx_file *idx,
		const unsigned char *data,
		struct git_midx_chunk *chunk_oid_fanout)
{
	uint32_t i, nr;
	if (chunk_oid_fanout->offset == 0)
		return midx_error("missing OID Fanout chunk");
	if (chunk_oid_fanout->length == 0)
		return midx_error("empty OID Fanout chunk");
	if (chunk_oid_fanout->length != 256 * 4)
		return midx_error("OID Fanout chunk has wrong length");

	idx->oid_fanout = (const uint32_t *)(data + chunk_oid_fanout->offset);
	nr = 0;
	for (i = 0; i < 256; ++i) {
		uint32_t n = ntohl(idx->oid_fanout[i]);
		if (n < nr)
			return midx_error("index is non-monotonic");
		nr = n;
	}
	idx->num_objects = nr;
	return 0;
}

static int midx_parse_oid_lookup(
		git_midx_file *idx,
		const unsigned char *data,
		struct git_midx_chunk *chunk_oid_lookup)
{
	size_t oid_size = git_oid_size(idx->oid_type);

	if (chunk_oid_lookup->offset == 0)
		return midx_error("missing OID Lookup chunk");
	if (chunk_oid_lookup->length == 0)
		return midx_error("empty OID Lookup chunk");
	if (chunk_oid_lookup->length != idx->num_objects * oid_size)
		return midx_error("OID Lookup chunk has wrong length");

	idx->oid_lookup = (unsigned char *)(data + chunk_oid_lookup->offset);

	return 0;
}

static int midx_parse_object_offsets(
		git_midx_file *idx,
		const unsigned char *data,
		struct git_midx_chunk *chunk_object_offsets)
{
	if (chunk_object_offsets->offset == 0)
		return midx_error("missing Object Offsets chunk");
	if (chunk_object_offsets->length == 0)
		return midx_error("empty Object Offsets chunk");
	if (chunk_object_offsets->length != idx->num_objects * 8)
		return midx_error("Object Offsets chunk has wrong length");

	idx->object_offsets = data + chunk_object_offsets->offset;

	return 0;
}

static int midx_parse_object_large_offsets(
		git_midx_file *idx,
		const unsigned char *data,
		struct git_midx_chunk *chunk_object_large_offsets)
{
	if (chunk_object_large_offsets->length == 0)
		return 0;
	if (chunk_object_large_offsets->length % 8 != 0)
		return midx_error("malformed Object Large Offsets chunk");

	idx->object_large_offsets = data + chunk_object_large_offsets->offset;
	idx->num_object_large_offsets = chunk_object_large_offsets->length / 8;

	return 0;
}

int git_midx_parse(
		git_midx_file *idx,
		const unsigned char *data,
		size_t size)
{
	struct git_midx_header *hdr;
	const unsigned char *chunk_hdr;
	struct git_midx_chunk *last_chunk;
	uint32_t i;
	off64_t last_chunk_offset, chunk_offset, trailer_offset;
	size_t checksum_size, oid_size;
	int error;
	struct git_midx_chunk chunk_packfile_names = {0},
					 chunk_oid_fanout = {0},
					 chunk_oid_lookup = {0},
					 chunk_object_offsets = {0},
					 chunk_object_large_offsets = {0},
					 chunk_unknown = {0};

	GIT_ASSERT_ARG(idx);

	oid_size = git_oid_size(idx->oid_type);

	if (size < sizeof(struct git_midx_header) + oid_size)
		return midx_error("multi-pack index is too short");

	hdr = ((struct git_midx_header *)data);

	if (hdr->signature != htonl(MIDX_SIGNATURE) ||
	    hdr->version != MIDX_VERSION ||
	    hdr->object_id_version != MIDX_OBJECT_ID_VERSION) {
		return midx_error("unsupported multi-pack index version");
	}
	if (hdr->chunks == 0)
		return midx_error("no chunks in multi-pack index");

	/*
	 * The very first chunk's offset should be after the header, all the chunk
	 * headers, and a special zero chunk.
	 */
	last_chunk_offset =
			sizeof(struct git_midx_header) +
			(1 + hdr->chunks) * 12;

	checksum_size = oid_size;
	trailer_offset = size - checksum_size;

	if (trailer_offset < last_chunk_offset)
		return midx_error("wrong index size");
	memcpy(idx->checksum, data + trailer_offset, checksum_size);

	chunk_hdr = data + sizeof(struct git_midx_header);
	last_chunk = NULL;
	for (i = 0; i < hdr->chunks; ++i, chunk_hdr += 12) {
		uint32_t chunk_id = ntohl(*((uint32_t *)(chunk_hdr + 0)));
		uint64_t high_offset = ((uint64_t)ntohl(*((uint32_t *)(chunk_hdr + 4)))) & 0xffffffffu;
		uint64_t low_offset = ((uint64_t)ntohl(*((uint32_t *)(chunk_hdr + 8)))) & 0xffffffffu;

		if (high_offset >= INT32_MAX)
			return midx_error("chunk offset out of range");
		chunk_offset = (off64_t)(high_offset << 32 | low_offset);
		if (chunk_offset < last_chunk_offset)
			return midx_error("chunks are non-monotonic");
		if (chunk_offset >= trailer_offset)
			return midx_error("chunks extend beyond the trailer");
		if (last_chunk != NULL)
			last_chunk->length = (size_t)(chunk_offset - last_chunk_offset);
		last_chunk_offset = chunk_offset;

		switch (chunk_id) {
		case MIDX_PACKFILE_NAMES_ID:
			chunk_packfile_names.offset = last_chunk_offset;
			last_chunk = &chunk_packfile_names;
			break;

		case MIDX_OID_FANOUT_ID:
			chunk_oid_fanout.offset = last_chunk_offset;
			last_chunk = &chunk_oid_fanout;
			break;

		case MIDX_OID_LOOKUP_ID:
			chunk_oid_lookup.offset = last_chunk_offset;
			last_chunk = &chunk_oid_lookup;
			break;

		case MIDX_OBJECT_OFFSETS_ID:
			chunk_object_offsets.offset = last_chunk_offset;
			last_chunk = &chunk_object_offsets;
			break;

		case MIDX_OBJECT_LARGE_OFFSETS_ID:
			chunk_object_large_offsets.offset = last_chunk_offset;
			last_chunk = &chunk_object_large_offsets;
			break;

		default:
			chunk_unknown.offset = last_chunk_offset;
			last_chunk = &chunk_unknown;
			break;
		}
	}
	last_chunk->length = (size_t)(trailer_offset - last_chunk_offset);

	error = midx_parse_packfile_names(
			idx, data, ntohl(hdr->packfiles), &chunk_packfile_names);
	if (error < 0)
		return error;
	error = midx_parse_oid_fanout(idx, data, &chunk_oid_fanout);
	if (error < 0)
		return error;
	error = midx_parse_oid_lookup(idx, data, &chunk_oid_lookup);
	if (error < 0)
		return error;
	error = midx_parse_object_offsets(idx, data, &chunk_object_offsets);
	if (error < 0)
		return error;
	error = midx_parse_object_large_offsets(idx, data, &chunk_object_large_offsets);
	if (error < 0)
		return error;

	return 0;
}

int git_midx_open(
	git_midx_file **idx_out,
	const char *path,
	git_oid_t oid_type)
{
	git_midx_file *idx;
	git_file fd = -1;
	size_t idx_size;
	struct stat st;
	int error;

	GIT_ASSERT_ARG(idx_out && path && oid_type);

	/* TODO: properly open the file without access time using O_NOATIME */
	fd = git_futils_open_ro(path);
	if (fd < 0)
		return fd;

	if (p_fstat(fd, &st) < 0) {
		p_close(fd);
		git_error_set(GIT_ERROR_ODB, "multi-pack-index file not found - '%s'", path);
		return -1;
	}

	if (!S_ISREG(st.st_mode) || !git__is_sizet(st.st_size)) {
		p_close(fd);
		git_error_set(GIT_ERROR_ODB, "invalid pack index '%s'", path);
		return -1;
	}
	idx_size = (size_t)st.st_size;

	idx = git__calloc(1, sizeof(git_midx_file));
	GIT_ERROR_CHECK_ALLOC(idx);

	idx->oid_type = oid_type;

	error = git_str_sets(&idx->filename, path);
	if (error < 0)
		return error;

	error = git_futils_mmap_ro(&idx->index_map, fd, 0, idx_size);
	p_close(fd);
	if (error < 0) {
		git_midx_free(idx);
		return error;
	}

	if ((error = git_midx_parse(idx, idx->index_map.data, idx_size)) < 0) {
		git_midx_free(idx);
		return error;
	}

	*idx_out = idx;
	return 0;
}

bool git_midx_needs_refresh(
		const git_midx_file *idx,
		const char *path)
{
	git_file fd = -1;
	struct stat st;
	ssize_t bytes_read;
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	size_t checksum_size;

	/* TODO: properly open the file without access time using O_NOATIME */
	fd = git_futils_open_ro(path);
	if (fd < 0)
		return true;

	if (p_fstat(fd, &st) < 0) {
		p_close(fd);
		return true;
	}

	if (!S_ISREG(st.st_mode) ||
	    !git__is_sizet(st.st_size) ||
	    (size_t)st.st_size != idx->index_map.len) {
		p_close(fd);
		return true;
	}

	checksum_size = git_oid_size(idx->oid_type);
	bytes_read = p_pread(fd, checksum, checksum_size, st.st_size - checksum_size);
	p_close(fd);

	if (bytes_read != (ssize_t)checksum_size)
		return true;

	return (memcmp(checksum, idx->checksum, checksum_size) != 0);
}

int git_midx_entry_find(
		git_midx_entry *e,
		git_midx_file *idx,
		const git_oid *short_oid,
		size_t len)
{
	int pos, found = 0;
	size_t pack_index, oid_size, oid_hexsize;
	uint32_t hi, lo;
	unsigned char *current = NULL;
	const unsigned char *object_offset;
	off64_t offset;

	GIT_ASSERT_ARG(idx);

	oid_size = git_oid_size(idx->oid_type);
	oid_hexsize = git_oid_hexsize(idx->oid_type);

	hi = ntohl(idx->oid_fanout[(int)short_oid->id[0]]);
	lo = ((short_oid->id[0] == 0x0) ? 0 : ntohl(idx->oid_fanout[(int)short_oid->id[0] - 1]));

	pos = git_pack__lookup_id(idx->oid_lookup, oid_size, lo, hi, short_oid->id, idx->oid_type);

	if (pos >= 0) {
		/* An object matching exactly the oid was found */
		found = 1;
		current = idx->oid_lookup + (pos * oid_size);
	} else {
		/* No object was found */
		/* pos refers to the object with the "closest" oid to short_oid */
		pos = -1 - pos;
		if (pos < (int)idx->num_objects) {
			current = idx->oid_lookup + (pos * oid_size);

			if (!git_oid_raw_ncmp(short_oid->id, current, len))
				found = 1;
		}
	}

	if (found && len != oid_hexsize && pos + 1 < (int)idx->num_objects) {
		/* Check for ambiguousity */
		const unsigned char *next = current + oid_size;

		if (!git_oid_raw_ncmp(short_oid->id, next, len))
			found = 2;
	}

	if (!found)
		return git_odb__error_notfound("failed to find offset for multi-pack index entry", short_oid, len);
	if (found > 1)
		return git_odb__error_ambiguous("found multiple offsets for multi-pack index entry");

	object_offset = idx->object_offsets + pos * 8;
	offset = ntohl(*((uint32_t *)(object_offset + 4)));
	if (idx->object_large_offsets && offset & 0x80000000) {
		uint32_t object_large_offsets_pos = (uint32_t) (offset ^ 0x80000000);
		const unsigned char *object_large_offsets_index = idx->object_large_offsets;

		/* Make sure we're not being sent out of bounds */
		if (object_large_offsets_pos >= idx->num_object_large_offsets)
			return git_odb__error_notfound("invalid index into the object large offsets table", short_oid, len);

		object_large_offsets_index += 8 * object_large_offsets_pos;

		offset = (((uint64_t)ntohl(*((uint32_t *)(object_large_offsets_index + 0)))) << 32) |
				ntohl(*((uint32_t *)(object_large_offsets_index + 4)));
	}
	pack_index = ntohl(*((uint32_t *)(object_offset + 0)));
	if (pack_index >= git_vector_length(&idx->packfile_names))
		return midx_error("invalid index into the packfile names table");
	e->pack_index = pack_index;
	e->offset = offset;
	git_oid__fromraw(&e->sha1, current, idx->oid_type);
	return 0;
}

int git_midx_foreach_entry(
		git_midx_file *idx,
		git_odb_foreach_cb cb,
		void *data)
{
	git_oid oid;
	size_t oid_size, i;
	int error;

	GIT_ASSERT_ARG(idx);

	oid_size = git_oid_size(idx->oid_type);

	for (i = 0; i < idx->num_objects; ++i) {
		if ((error = git_oid__fromraw(&oid, &idx->oid_lookup[i * oid_size], idx->oid_type)) < 0)
			return error;

		if ((error = cb(&oid, data)) != 0)
			return git_error_set_after_callback(error);
	}

	return error;
}

int git_midx_close(git_midx_file *idx)
{
	GIT_ASSERT_ARG(idx);

	if (idx->index_map.data)
		git_futils_mmap_free(&idx->index_map);

	git_vector_dispose(&idx->packfile_names);

	return 0;
}

void git_midx_free(git_midx_file *idx)
{
	if (!idx)
		return;

	git_str_dispose(&idx->filename);
	git_midx_close(idx);
	git__free(idx);
}

static int packfile__cmp(const void *a_, const void *b_)
{
	const struct git_pack_file *a = a_;
	const struct git_pack_file *b = b_;

	return strcmp(a->pack_name, b->pack_name);
}

int git_midx_writer_new(
	git_midx_writer **out,
	const char *pack_dir
#ifdef GIT_EXPERIMENTAL_SHA256
	, git_midx_writer_options *opts
#endif
		)
{
	git_midx_writer *w;
	git_oid_t oid_type;

	GIT_ASSERT_ARG(out && pack_dir);

#ifdef GIT_EXPERIMENTAL_SHA256
	GIT_ERROR_CHECK_VERSION(opts,
		GIT_MIDX_WRITER_OPTIONS_VERSION,
		"git_midx_writer_options");

	oid_type = opts && opts->oid_type ? opts->oid_type : GIT_OID_DEFAULT;
	GIT_ASSERT_ARG(git_oid_type_is_valid(oid_type));
#else
	oid_type = GIT_OID_SHA1;
#endif

	w = git__calloc(1, sizeof(git_midx_writer));
	GIT_ERROR_CHECK_ALLOC(w);

	if (git_str_sets(&w->pack_dir, pack_dir) < 0) {
		git__free(w);
		return -1;
	}
	git_fs_path_squash_slashes(&w->pack_dir);

	if (git_vector_init(&w->packs, 0, packfile__cmp) < 0) {
		git_str_dispose(&w->pack_dir);
		git__free(w);
		return -1;
	}

	w->oid_type = oid_type;

	*out = w;
	return 0;
}

void git_midx_writer_free(git_midx_writer *w)
{
	struct git_pack_file *p;
	size_t i;

	if (!w)
		return;

	git_vector_foreach (&w->packs, i, p)
		git_mwindow_put_pack(p);
	git_vector_dispose(&w->packs);
	git_str_dispose(&w->pack_dir);
	git__free(w);
}

int git_midx_writer_add(
		git_midx_writer *w,
		const char *idx_path)
{
	git_str idx_path_buf = GIT_STR_INIT;
	int error;
	struct git_pack_file *p;

	error = git_fs_path_prettify(&idx_path_buf, idx_path, git_str_cstr(&w->pack_dir));
	if (error < 0)
		return error;

	/* TODO: SHA256 */
	error = git_mwindow_get_pack(&p, git_str_cstr(&idx_path_buf), 0);
	git_str_dispose(&idx_path_buf);
	if (error < 0)
		return error;

	error = git_vector_insert(&w->packs, p);
	if (error < 0) {
		git_mwindow_put_pack(p);
		return error;
	}

	return 0;
}

typedef git_array_t(git_midx_entry) object_entry_array_t;

struct object_entry_cb_state {
	uint32_t pack_index;
	object_entry_array_t *object_entries_array;
};

static int object_entry__cb(const git_oid *oid, off64_t offset, void *data)
{
	struct object_entry_cb_state *state = (struct object_entry_cb_state *)data;

	git_midx_entry *entry = git_array_alloc(*state->object_entries_array);
	GIT_ERROR_CHECK_ALLOC(entry);

	git_oid_cpy(&entry->sha1, oid);
	entry->offset = offset;
	entry->pack_index = state->pack_index;

	return 0;
}

static int object_entry__cmp(const void *a_, const void *b_)
{
	const git_midx_entry *a = (const git_midx_entry *)a_;
	const git_midx_entry *b = (const git_midx_entry *)b_;

	return git_oid_cmp(&a->sha1, &b->sha1);
}

static int write_offset(off64_t offset, midx_write_cb write_cb, void *cb_data)
{
	int error;
	uint32_t word;

	word = htonl((uint32_t)((offset >> 32) & 0xffffffffu));
	error = write_cb((const char *)&word, sizeof(word), cb_data);
	if (error < 0)
		return error;
	word = htonl((uint32_t)((offset >> 0) & 0xffffffffu));
	error = write_cb((const char *)&word, sizeof(word), cb_data);
	if (error < 0)
		return error;

	return 0;
}

static int write_chunk_header(int chunk_id, off64_t offset, midx_write_cb write_cb, void *cb_data)
{
	uint32_t word = htonl(chunk_id);
	int error = write_cb((const char *)&word, sizeof(word), cb_data);
	if (error < 0)
		return error;
	return write_offset(offset, write_cb, cb_data);

	return 0;
}

static int midx_write_buf(const char *buf, size_t size, void *data)
{
	git_str *b = (git_str *)data;
	return git_str_put(b, buf, size);
}

struct midx_write_hash_context {
	midx_write_cb write_cb;
	void *cb_data;
	git_hash_ctx *ctx;
};

static int midx_write_hash(const char *buf, size_t size, void *data)
{
	struct midx_write_hash_context *ctx = (struct midx_write_hash_context *)data;
	int error;

	if (ctx->ctx) {
		error = git_hash_update(ctx->ctx, buf, size);
		if (error < 0)
			return error;
	}

	return ctx->write_cb(buf, size, ctx->cb_data);
}

static int midx_write(
		git_midx_writer *w,
		midx_write_cb write_cb,
		void *cb_data)
{
	int error = 0;
	size_t i;
	struct git_pack_file *p;
	struct git_midx_header hdr = {0};
	uint32_t oid_fanout_count;
	uint32_t object_large_offsets_count;
	uint32_t oid_fanout[256];
	off64_t offset;
	git_str packfile_names = GIT_STR_INIT,
		oid_lookup = GIT_STR_INIT,
		object_offsets = GIT_STR_INIT,
		object_large_offsets = GIT_STR_INIT;
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	size_t checksum_size, oid_size;
	git_midx_entry *entry;
	object_entry_array_t object_entries_array = GIT_ARRAY_INIT;
	git_vector object_entries = GIT_VECTOR_INIT;
	git_hash_ctx ctx;
	git_hash_algorithm_t checksum_type;
	struct midx_write_hash_context hash_cb_data = {0};

	hdr.signature = htonl(MIDX_SIGNATURE);
	hdr.version = MIDX_VERSION;
	hdr.object_id_version = MIDX_OBJECT_ID_VERSION;
	hdr.base_midx_files = 0;

	hash_cb_data.write_cb = write_cb;
	hash_cb_data.cb_data = cb_data;
	hash_cb_data.ctx = &ctx;

	oid_size = git_oid_size(w->oid_type);
	checksum_type = git_oid_algorithm(w->oid_type);
	checksum_size = git_hash_size(checksum_type);
	GIT_ASSERT(oid_size && checksum_type && checksum_size);

	if ((error = git_hash_ctx_init(&ctx, checksum_type)) < 0)
		return error;

	cb_data = &hash_cb_data;
	write_cb = midx_write_hash;

	git_vector_sort(&w->packs);
	git_vector_foreach (&w->packs, i, p) {
		git_str relative_index = GIT_STR_INIT;
		struct object_entry_cb_state state = {0};
		size_t path_len;

		state.pack_index = (uint32_t)i;
		state.object_entries_array = &object_entries_array;

		error = git_str_sets(&relative_index, p->pack_name);
		if (error < 0)
			goto cleanup;
		error = git_fs_path_make_relative(&relative_index, git_str_cstr(&w->pack_dir));
		if (error < 0) {
			git_str_dispose(&relative_index);
			goto cleanup;
		}
		path_len = git_str_len(&relative_index);
		if (path_len <= strlen(".pack") || git__suffixcmp(git_str_cstr(&relative_index), ".pack") != 0) {
			git_str_dispose(&relative_index);
			git_error_set(GIT_ERROR_INVALID, "invalid packfile name: '%s'", p->pack_name);
			error = -1;
			goto cleanup;
		}
		path_len -= strlen(".pack");

		git_str_put(&packfile_names, git_str_cstr(&relative_index), path_len);
		git_str_puts(&packfile_names, ".idx");
		git_str_putc(&packfile_names, '\0');
		git_str_dispose(&relative_index);

		error = git_pack_foreach_entry_offset(p, object_entry__cb, &state);
		if (error < 0)
			goto cleanup;
	}

	/* Sort the object entries. */
	error = git_vector_init(&object_entries, git_array_size(object_entries_array), object_entry__cmp);
	if (error < 0)
		goto cleanup;
	git_array_foreach (object_entries_array, i, entry) {
		if ((error = git_vector_set(NULL, &object_entries, i, entry)) < 0)
			goto cleanup;
	}
	git_vector_set_sorted(&object_entries, 0);
	git_vector_sort(&object_entries);
	git_vector_uniq(&object_entries, NULL);

	/* Pad the packfile names so it is a multiple of four. */
	while (git_str_len(&packfile_names) & 3)
		git_str_putc(&packfile_names, '\0');

	/* Fill the OID Fanout table. */
	oid_fanout_count = 0;
	for (i = 0; i < 256; i++) {
		while (oid_fanout_count < git_vector_length(&object_entries) &&
		       ((const git_midx_entry *)git_vector_get(&object_entries, oid_fanout_count))->sha1.id[0] <= i)
			++oid_fanout_count;
		oid_fanout[i] = htonl(oid_fanout_count);
	}

	/* Fill the OID Lookup table. */
	git_vector_foreach (&object_entries, i, entry) {
		error = git_str_put(&oid_lookup,
			(char *)&entry->sha1.id, oid_size);

		if (error < 0)
			goto cleanup;
	}

	/* Fill the Object Offsets and Object Large Offsets tables. */
	object_large_offsets_count = 0;
	git_vector_foreach (&object_entries, i, entry) {
		uint32_t word;

		word = htonl((uint32_t)entry->pack_index);
		error = git_str_put(&object_offsets, (const char *)&word, sizeof(word));
		if (error < 0)
			goto cleanup;
		if (entry->offset >= 0x80000000l) {
			word = htonl(0x80000000u | object_large_offsets_count++);
			if ((error = write_offset(entry->offset, midx_write_buf, &object_large_offsets)) < 0)
				goto cleanup;
		} else {
			word = htonl((uint32_t)entry->offset & 0x7fffffffu);
		}

		error = git_str_put(&object_offsets, (const char *)&word, sizeof(word));
		if (error < 0)
			goto cleanup;
	}

	/* Write the header. */
	hdr.packfiles = htonl((uint32_t)git_vector_length(&w->packs));
	hdr.chunks = 4;
	if (git_str_len(&object_large_offsets) > 0)
		hdr.chunks++;
	error = write_cb((const char *)&hdr, sizeof(hdr), cb_data);
	if (error < 0)
		goto cleanup;

	/* Write the chunk headers. */
	offset = sizeof(hdr) + (hdr.chunks + 1) * 12;
	error = write_chunk_header(MIDX_PACKFILE_NAMES_ID, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;
	offset += git_str_len(&packfile_names);
	error = write_chunk_header(MIDX_OID_FANOUT_ID, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;
	offset += sizeof(oid_fanout);
	error = write_chunk_header(MIDX_OID_LOOKUP_ID, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;
	offset += git_str_len(&oid_lookup);
	error = write_chunk_header(MIDX_OBJECT_OFFSETS_ID, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;
	offset += git_str_len(&object_offsets);
	if (git_str_len(&object_large_offsets) > 0) {
		error = write_chunk_header(MIDX_OBJECT_LARGE_OFFSETS_ID, offset, write_cb, cb_data);
		if (error < 0)
			goto cleanup;
		offset += git_str_len(&object_large_offsets);
	}
	error = write_chunk_header(0, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;

	/* Write all the chunks. */
	error = write_cb(git_str_cstr(&packfile_names), git_str_len(&packfile_names), cb_data);
	if (error < 0)
		goto cleanup;
	error = write_cb((const char *)oid_fanout, sizeof(oid_fanout), cb_data);
	if (error < 0)
		goto cleanup;
	error = write_cb(git_str_cstr(&oid_lookup), git_str_len(&oid_lookup), cb_data);
	if (error < 0)
		goto cleanup;
	error = write_cb(git_str_cstr(&object_offsets), git_str_len(&object_offsets), cb_data);
	if (error < 0)
		goto cleanup;
	error = write_cb(git_str_cstr(&object_large_offsets), git_str_len(&object_large_offsets), cb_data);
	if (error < 0)
		goto cleanup;

	/* Finalize the checksum and write the trailer. */
	error = git_hash_final(checksum, &ctx);
	if (error < 0)
		goto cleanup;

	hash_cb_data.ctx = NULL;

	error = write_cb((char *)checksum, checksum_size, cb_data);
	if (error < 0)
		goto cleanup;

cleanup:
	git_array_clear(object_entries_array);
	git_vector_dispose(&object_entries);
	git_str_dispose(&packfile_names);
	git_str_dispose(&oid_lookup);
	git_str_dispose(&object_offsets);
	git_str_dispose(&object_large_offsets);
	git_hash_ctx_cleanup(&ctx);
	return error;
}

static int midx_write_filebuf(const char *buf, size_t size, void *data)
{
	git_filebuf *f = (git_filebuf *)data;
	return git_filebuf_write(f, buf, size);
}

int git_midx_writer_commit(
		git_midx_writer *w)
{
	int error;
	int filebuf_flags = GIT_FILEBUF_DO_NOT_BUFFER;
	git_str midx_path = GIT_STR_INIT;
	git_filebuf output = GIT_FILEBUF_INIT;

	error = git_str_joinpath(&midx_path, git_str_cstr(&w->pack_dir), "multi-pack-index");
	if (error < 0)
		return error;

	if (git_repository__fsync_gitdir)
		filebuf_flags |= GIT_FILEBUF_FSYNC;
	error = git_filebuf_open(&output, git_str_cstr(&midx_path), filebuf_flags, 0644);
	git_str_dispose(&midx_path);
	if (error < 0)
		return error;

	error = midx_write(w, midx_write_filebuf, &output);
	if (error < 0) {
		git_filebuf_cleanup(&output);
		return error;
	}

	return git_filebuf_commit(&output);
}

int git_midx_writer_dump(
		git_buf *midx,
		git_midx_writer *w)
{
	git_str str = GIT_STR_INIT;
	int error;

	if ((error = git_buf_tostr(&str, midx)) < 0 ||
	    (error = midx_write(w, midx_write_buf, &str)) == 0)
		error = git_buf_fromstr(midx, &str);

	git_str_dispose(&str);
	return error;
}
