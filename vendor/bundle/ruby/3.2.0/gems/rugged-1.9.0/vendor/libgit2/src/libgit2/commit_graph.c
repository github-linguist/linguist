/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "commit_graph.h"

#include "array.h"
#include "buf.h"
#include "filebuf.h"
#include "futils.h"
#include "hash.h"
#include "oidarray.h"
#include "pack.h"
#include "repository.h"
#include "revwalk.h"

#define GIT_COMMIT_GRAPH_MISSING_PARENT 0x70000000
#define GIT_COMMIT_GRAPH_GENERATION_NUMBER_MAX 0x3FFFFFFF
#define GIT_COMMIT_GRAPH_GENERATION_NUMBER_INFINITY 0xFFFFFFFF

#define COMMIT_GRAPH_SIGNATURE 0x43475048 /* "CGPH" */
#define COMMIT_GRAPH_VERSION 1
#define COMMIT_GRAPH_OBJECT_ID_VERSION 1

struct git_commit_graph_header {
	uint32_t signature;
	uint8_t version;
	uint8_t object_id_version;
	uint8_t chunks;
	uint8_t base_graph_files;
};

#define COMMIT_GRAPH_OID_FANOUT_ID 0x4f494446	      /* "OIDF" */
#define COMMIT_GRAPH_OID_LOOKUP_ID 0x4f49444c	      /* "OIDL" */
#define COMMIT_GRAPH_COMMIT_DATA_ID 0x43444154	      /* "CDAT" */
#define COMMIT_GRAPH_EXTRA_EDGE_LIST_ID 0x45444745    /* "EDGE" */
#define COMMIT_GRAPH_BLOOM_FILTER_INDEX_ID 0x42494458 /* "BIDX" */
#define COMMIT_GRAPH_BLOOM_FILTER_DATA_ID 0x42444154  /* "BDAT" */

struct git_commit_graph_chunk {
	off64_t offset;
	size_t length;
};

typedef git_array_t(size_t) parent_index_array_t;

struct packed_commit {
	size_t index;
	git_oid sha1;
	git_oid tree_oid;
	uint32_t generation;
	git_time_t commit_time;
	git_array_oid_t parents;
	parent_index_array_t parent_indices;
};

static void packed_commit_free(struct packed_commit *p)
{
	if (!p)
		return;

	git_array_clear(p->parents);
	git_array_clear(p->parent_indices);
	git__free(p);
}

static struct packed_commit *packed_commit_new(git_commit *commit)
{
	unsigned int i, parentcount = git_commit_parentcount(commit);
	struct packed_commit *p = git__calloc(1, sizeof(struct packed_commit));
	if (!p)
		goto cleanup;

	git_array_init_to_size(p->parents, parentcount);
	if (parentcount && !p->parents.ptr)
		goto cleanup;

	if (git_oid_cpy(&p->sha1, git_commit_id(commit)) < 0)
		goto cleanup;
	if (git_oid_cpy(&p->tree_oid, git_commit_tree_id(commit)) < 0)
		goto cleanup;
	p->commit_time = git_commit_time(commit);

	for (i = 0; i < parentcount; ++i) {
		git_oid *parent_id = git_array_alloc(p->parents);
		if (!parent_id)
			goto cleanup;
		if (git_oid_cpy(parent_id, git_commit_parent_id(commit, i)) < 0)
			goto cleanup;
	}

	return p;

cleanup:
	packed_commit_free(p);
	return NULL;
}

typedef int (*commit_graph_write_cb)(const char *buf, size_t size, void *cb_data);

static int commit_graph_error(const char *message)
{
	git_error_set(GIT_ERROR_ODB, "invalid commit-graph file - %s", message);
	return -1;
}

static int commit_graph_parse_oid_fanout(
		git_commit_graph_file *file,
		const unsigned char *data,
		struct git_commit_graph_chunk *chunk_oid_fanout)
{
	uint32_t i, nr;
	if (chunk_oid_fanout->offset == 0)
		return commit_graph_error("missing OID Fanout chunk");
	if (chunk_oid_fanout->length == 0)
		return commit_graph_error("empty OID Fanout chunk");
	if (chunk_oid_fanout->length != 256 * 4)
		return commit_graph_error("OID Fanout chunk has wrong length");

	file->oid_fanout = (const uint32_t *)(data + chunk_oid_fanout->offset);
	nr = 0;
	for (i = 0; i < 256; ++i) {
		uint32_t n = ntohl(file->oid_fanout[i]);
		if (n < nr)
			return commit_graph_error("index is non-monotonic");
		nr = n;
	}
	file->num_commits = nr;
	return 0;
}

static int commit_graph_parse_oid_lookup(
		git_commit_graph_file *file,
		const unsigned char *data,
		struct git_commit_graph_chunk *chunk_oid_lookup)
{
	uint32_t i;
	unsigned char *oid, *prev_oid, zero_oid[GIT_OID_MAX_SIZE] = {0};
	size_t oid_size;

	oid_size = git_oid_size(file->oid_type);

	if (chunk_oid_lookup->offset == 0)
		return commit_graph_error("missing OID Lookup chunk");
	if (chunk_oid_lookup->length == 0)
		return commit_graph_error("empty OID Lookup chunk");
	if (chunk_oid_lookup->length != file->num_commits * oid_size)
		return commit_graph_error("OID Lookup chunk has wrong length");

	file->oid_lookup = oid = (unsigned char *)(data + chunk_oid_lookup->offset);
	prev_oid = zero_oid;
	for (i = 0; i < file->num_commits; ++i, oid += oid_size) {
		if (git_oid_raw_cmp(prev_oid, oid, oid_size) >= 0)
			return commit_graph_error("OID Lookup index is non-monotonic");
		prev_oid = oid;
	}

	return 0;
}

static int commit_graph_parse_commit_data(
		git_commit_graph_file *file,
		const unsigned char *data,
		struct git_commit_graph_chunk *chunk_commit_data)
{
	size_t oid_size = git_oid_size(file->oid_type);

	if (chunk_commit_data->offset == 0)
		return commit_graph_error("missing Commit Data chunk");
	if (chunk_commit_data->length == 0)
		return commit_graph_error("empty Commit Data chunk");
	if (chunk_commit_data->length != file->num_commits * (oid_size + 16))
		return commit_graph_error("Commit Data chunk has wrong length");

	file->commit_data = data + chunk_commit_data->offset;

	return 0;
}

static int commit_graph_parse_extra_edge_list(
		git_commit_graph_file *file,
		const unsigned char *data,
		struct git_commit_graph_chunk *chunk_extra_edge_list)
{
	if (chunk_extra_edge_list->length == 0)
		return 0;
	if (chunk_extra_edge_list->length % 4 != 0)
		return commit_graph_error("malformed Extra Edge List chunk");

	file->extra_edge_list = data + chunk_extra_edge_list->offset;
	file->num_extra_edge_list = chunk_extra_edge_list->length / 4;

	return 0;
}

int git_commit_graph_file_parse(
		git_commit_graph_file *file,
		const unsigned char *data,
		size_t size)
{
	struct git_commit_graph_header *hdr;
	const unsigned char *chunk_hdr;
	struct git_commit_graph_chunk *last_chunk;
	uint32_t i;
	uint64_t last_chunk_offset, chunk_offset, trailer_offset;
	size_t checksum_size;
	int error;
	struct git_commit_graph_chunk chunk_oid_fanout = {0}, chunk_oid_lookup = {0},
				      chunk_commit_data = {0}, chunk_extra_edge_list = {0},
				      chunk_unsupported = {0};

	GIT_ASSERT_ARG(file);

	checksum_size = git_oid_size(file->oid_type);

	if (size < sizeof(struct git_commit_graph_header) + checksum_size)
		return commit_graph_error("commit-graph is too short");

	hdr = ((struct git_commit_graph_header *)data);

	if (hdr->signature != htonl(COMMIT_GRAPH_SIGNATURE) || hdr->version != COMMIT_GRAPH_VERSION
	    || hdr->object_id_version != COMMIT_GRAPH_OBJECT_ID_VERSION) {
		return commit_graph_error("unsupported commit-graph version");
	}
	if (hdr->chunks == 0)
		return commit_graph_error("no chunks in commit-graph");

	/*
	 * The very first chunk's offset should be after the header, all the chunk
	 * headers, and a special zero chunk.
	 */
	last_chunk_offset = sizeof(struct git_commit_graph_header) + (1 + hdr->chunks) * 12;
	trailer_offset = size - checksum_size;

	if (trailer_offset < last_chunk_offset)
		return commit_graph_error("wrong commit-graph size");
	memcpy(file->checksum, (data + trailer_offset), checksum_size);

	chunk_hdr = data + sizeof(struct git_commit_graph_header);
	last_chunk = NULL;
	for (i = 0; i < hdr->chunks; ++i, chunk_hdr += 12) {
		chunk_offset = ((uint64_t)ntohl(*((uint32_t *)(chunk_hdr + 4)))) << 32
				| ((uint64_t)ntohl(*((uint32_t *)(chunk_hdr + 8))));
		if (chunk_offset < last_chunk_offset)
			return commit_graph_error("chunks are non-monotonic");
		if (chunk_offset >= trailer_offset)
			return commit_graph_error("chunks extend beyond the trailer");
		if (last_chunk != NULL)
			last_chunk->length = (size_t)(chunk_offset - last_chunk_offset);
		last_chunk_offset = chunk_offset;

		switch (ntohl(*((uint32_t *)(chunk_hdr + 0)))) {
		case COMMIT_GRAPH_OID_FANOUT_ID:
			chunk_oid_fanout.offset = last_chunk_offset;
			last_chunk = &chunk_oid_fanout;
			break;

		case COMMIT_GRAPH_OID_LOOKUP_ID:
			chunk_oid_lookup.offset = last_chunk_offset;
			last_chunk = &chunk_oid_lookup;
			break;

		case COMMIT_GRAPH_COMMIT_DATA_ID:
			chunk_commit_data.offset = last_chunk_offset;
			last_chunk = &chunk_commit_data;
			break;

		case COMMIT_GRAPH_EXTRA_EDGE_LIST_ID:
			chunk_extra_edge_list.offset = last_chunk_offset;
			last_chunk = &chunk_extra_edge_list;
			break;

		case COMMIT_GRAPH_BLOOM_FILTER_INDEX_ID:
		case COMMIT_GRAPH_BLOOM_FILTER_DATA_ID:
			chunk_unsupported.offset = last_chunk_offset;
			last_chunk = &chunk_unsupported;
			break;

		default:
			return commit_graph_error("unrecognized chunk ID");
		}
	}
	last_chunk->length = (size_t)(trailer_offset - last_chunk_offset);

	error = commit_graph_parse_oid_fanout(file, data, &chunk_oid_fanout);
	if (error < 0)
		return error;
	error = commit_graph_parse_oid_lookup(file, data, &chunk_oid_lookup);
	if (error < 0)
		return error;
	error = commit_graph_parse_commit_data(file, data, &chunk_commit_data);
	if (error < 0)
		return error;
	error = commit_graph_parse_extra_edge_list(file, data, &chunk_extra_edge_list);
	if (error < 0)
		return error;

	return 0;
}

int git_commit_graph_new(
	git_commit_graph **cgraph_out,
	const char *objects_dir,
	bool open_file,
	git_oid_t oid_type)
{
	git_commit_graph *cgraph = NULL;
	int error = 0;

	GIT_ASSERT_ARG(cgraph_out);
	GIT_ASSERT_ARG(objects_dir);
	GIT_ASSERT_ARG(oid_type);

	cgraph = git__calloc(1, sizeof(git_commit_graph));
	GIT_ERROR_CHECK_ALLOC(cgraph);

	cgraph->oid_type = oid_type;

	error = git_str_joinpath(&cgraph->filename, objects_dir, "info/commit-graph");
	if (error < 0)
		goto error;

	if (open_file) {
		error = git_commit_graph_file_open(&cgraph->file,
				git_str_cstr(&cgraph->filename), oid_type);

		if (error < 0)
			goto error;

		cgraph->checked = 1;
	}

	*cgraph_out = cgraph;
	return 0;

error:
	git_commit_graph_free(cgraph);
	return error;
}

int git_commit_graph_validate(git_commit_graph *cgraph) {
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	git_hash_algorithm_t checksum_type;
	size_t checksum_size, trailer_offset;

	checksum_type = git_oid_algorithm(cgraph->oid_type);
	checksum_size = git_hash_size(checksum_type);
	trailer_offset = cgraph->file->graph_map.len - checksum_size;

	if (cgraph->file->graph_map.len < checksum_size)
		return commit_graph_error("map length too small");

	if (git_hash_buf(checksum, cgraph->file->graph_map.data, trailer_offset, checksum_type) < 0)
		return commit_graph_error("could not calculate signature");
	if (memcmp(checksum, cgraph->file->checksum, checksum_size) != 0)
		return commit_graph_error("index signature mismatch");

	return 0;
}

int git_commit_graph_open(
	git_commit_graph **cgraph_out,
	const char *objects_dir
#ifdef GIT_EXPERIMENTAL_SHA256
	, const git_commit_graph_open_options *opts
#endif
	)
{
	git_oid_t oid_type;
	int error;

#ifdef GIT_EXPERIMENTAL_SHA256
	GIT_ERROR_CHECK_VERSION(opts,
		GIT_COMMIT_GRAPH_OPEN_OPTIONS_VERSION,
		"git_commit_graph_open_options");

	oid_type = opts && opts->oid_type ? opts->oid_type : GIT_OID_DEFAULT;
	GIT_ASSERT_ARG(git_oid_type_is_valid(oid_type));
#else
	oid_type = GIT_OID_SHA1;
#endif

	error = git_commit_graph_new(cgraph_out, objects_dir, true,
			oid_type);

	if (!error)
		return git_commit_graph_validate(*cgraph_out);

	return error;
}

int git_commit_graph_file_open(
	git_commit_graph_file **file_out,
	const char *path,
	git_oid_t oid_type)
{
	git_commit_graph_file *file;
	git_file fd = -1;
	size_t cgraph_size;
	struct stat st;
	int error;

	/* TODO: properly open the file without access time using O_NOATIME */
	fd = git_futils_open_ro(path);
	if (fd < 0)
		return fd;

	if (p_fstat(fd, &st) < 0) {
		p_close(fd);
		git_error_set(GIT_ERROR_ODB, "commit-graph file not found - '%s'", path);
		return GIT_ENOTFOUND;
	}

	if (!S_ISREG(st.st_mode) || !git__is_sizet(st.st_size)) {
		p_close(fd);
		git_error_set(GIT_ERROR_ODB, "invalid pack index '%s'", path);
		return GIT_ENOTFOUND;
	}
	cgraph_size = (size_t)st.st_size;

	file = git__calloc(1, sizeof(git_commit_graph_file));
	GIT_ERROR_CHECK_ALLOC(file);

	file->oid_type = oid_type;

	error = git_futils_mmap_ro(&file->graph_map, fd, 0, cgraph_size);
	p_close(fd);
	if (error < 0) {
		git_commit_graph_file_free(file);
		return error;
	}

	if ((error = git_commit_graph_file_parse(file, file->graph_map.data, cgraph_size)) < 0) {
		git_commit_graph_file_free(file);
		return error;
	}

	*file_out = file;
	return 0;
}

int git_commit_graph_get_file(
	git_commit_graph_file **file_out,
	git_commit_graph *cgraph)
{
	if (!cgraph->checked) {
		int error = 0;
		git_commit_graph_file *result = NULL;

		/* We only check once, no matter the result. */
		cgraph->checked = 1;

		/* Best effort */
		error = git_commit_graph_file_open(&result,
			git_str_cstr(&cgraph->filename), cgraph->oid_type);

		if (error < 0)
			return error;

		cgraph->file = result;
	}
	if (!cgraph->file)
		return GIT_ENOTFOUND;

	*file_out = cgraph->file;
	return 0;
}

void git_commit_graph_refresh(git_commit_graph *cgraph)
{
	if (!cgraph->checked)
		return;

	if (cgraph->file
	    && git_commit_graph_file_needs_refresh(cgraph->file, git_str_cstr(&cgraph->filename))) {
		/* We just free the commit graph. The next time it is requested, it will be
		 * re-loaded. */
		git_commit_graph_file_free(cgraph->file);
		cgraph->file = NULL;
	}
	/* Force a lazy re-check next time it is needed. */
	cgraph->checked = 0;
}

static int git_commit_graph_entry_get_byindex(
		git_commit_graph_entry *e,
		const git_commit_graph_file *file,
		size_t pos)
{
	const unsigned char *commit_data;
	size_t oid_size = git_oid_size(file->oid_type);

	GIT_ASSERT_ARG(e);
	GIT_ASSERT_ARG(file);

	if (pos >= file->num_commits) {
		git_error_set(GIT_ERROR_INVALID, "commit index %zu does not exist", pos);
		return GIT_ENOTFOUND;
	}

	commit_data = file->commit_data + pos * (oid_size + 4 * sizeof(uint32_t));
	git_oid__fromraw(&e->tree_oid, commit_data, file->oid_type);
	e->parent_indices[0] = ntohl(*((uint32_t *)(commit_data + oid_size)));
	e->parent_indices[1] = ntohl(
			*((uint32_t *)(commit_data + oid_size + sizeof(uint32_t))));
	e->parent_count = (e->parent_indices[0] != GIT_COMMIT_GRAPH_MISSING_PARENT)
			+ (e->parent_indices[1] != GIT_COMMIT_GRAPH_MISSING_PARENT);
	e->generation = ntohl(*((uint32_t *)(commit_data + oid_size + 2 * sizeof(uint32_t))));
	e->commit_time = ntohl(*((uint32_t *)(commit_data + oid_size + 3 * sizeof(uint32_t))));

	e->commit_time |= (e->generation & UINT64_C(0x3)) << UINT64_C(32);
	e->generation >>= 2u;
	if (e->parent_indices[1] & 0x80000000u) {
		uint32_t extra_edge_list_pos = e->parent_indices[1] & 0x7fffffff;

		/* Make sure we're not being sent out of bounds */
		if (extra_edge_list_pos >= file->num_extra_edge_list) {
			git_error_set(GIT_ERROR_INVALID,
				      "commit %u does not exist",
				      extra_edge_list_pos);
			return GIT_ENOTFOUND;
		}

		e->extra_parents_index = extra_edge_list_pos;
		while (extra_edge_list_pos < file->num_extra_edge_list
		       && (ntohl(*(
					   (uint32_t *)(file->extra_edge_list
							+ extra_edge_list_pos * sizeof(uint32_t))))
			   & 0x80000000u)
				       == 0) {
			extra_edge_list_pos++;
			e->parent_count++;
		}
	}

	git_oid__fromraw(&e->sha1, &file->oid_lookup[pos * oid_size], file->oid_type);
	return 0;
}

bool git_commit_graph_file_needs_refresh(const git_commit_graph_file *file, const char *path)
{
	git_file fd = -1;
	struct stat st;
	ssize_t bytes_read;
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	size_t checksum_size = git_oid_size(file->oid_type);

	/* TODO: properly open the file without access time using O_NOATIME */
	fd = git_futils_open_ro(path);
	if (fd < 0)
		return true;

	if (p_fstat(fd, &st) < 0) {
		p_close(fd);
		return true;
	}

	if (!S_ISREG(st.st_mode) || !git__is_sizet(st.st_size)
	    || (size_t)st.st_size != file->graph_map.len) {
		p_close(fd);
		return true;
	}

	bytes_read = p_pread(fd, checksum, checksum_size, st.st_size - checksum_size);
	p_close(fd);
	if (bytes_read != (ssize_t)checksum_size)
		return true;

	return (memcmp(checksum, file->checksum, checksum_size) != 0);
}

int git_commit_graph_entry_find(
		git_commit_graph_entry *e,
		const git_commit_graph_file *file,
		const git_oid *short_oid,
		size_t len)
{
	int pos, found = 0;
	uint32_t hi, lo;
	const unsigned char *current = NULL;
	size_t oid_size, oid_hexsize;

	GIT_ASSERT_ARG(e);
	GIT_ASSERT_ARG(file);
	GIT_ASSERT_ARG(short_oid);

	oid_size = git_oid_size(file->oid_type);
	oid_hexsize = git_oid_hexsize(file->oid_type);

	hi = ntohl(file->oid_fanout[(int)short_oid->id[0]]);
	lo = ((short_oid->id[0] == 0x0) ? 0 : ntohl(file->oid_fanout[(int)short_oid->id[0] - 1]));

	pos = git_pack__lookup_id(file->oid_lookup, oid_size, lo, hi,
		short_oid->id, file->oid_type);

	if (pos >= 0) {
		/* An object matching exactly the oid was found */
		found = 1;
		current = file->oid_lookup + (pos * oid_size);
	} else {
		/* No object was found */
		/* pos refers to the object with the "closest" oid to short_oid */
		pos = -1 - pos;
		if (pos < (int)file->num_commits) {
			current = file->oid_lookup + (pos * oid_size);

			if (!git_oid_raw_ncmp(short_oid->id, current, len))
				found = 1;
		}
	}

	if (found && len != oid_hexsize && pos + 1 < (int)file->num_commits) {
		/* Check for ambiguousity */
		const unsigned char *next = current + oid_size;

		if (!git_oid_raw_ncmp(short_oid->id, next, len))
			found = 2;
	}

	if (!found)
		return git_odb__error_notfound(
				"failed to find offset for commit-graph index entry", short_oid, len);
	if (found > 1)
		return git_odb__error_ambiguous(
				"found multiple offsets for commit-graph index entry");

	return git_commit_graph_entry_get_byindex(e, file, pos);
}

int git_commit_graph_entry_parent(
		git_commit_graph_entry *parent,
		const git_commit_graph_file *file,
		const git_commit_graph_entry *entry,
		size_t n)
{
	GIT_ASSERT_ARG(parent);
	GIT_ASSERT_ARG(file);

	if (n >= entry->parent_count) {
		git_error_set(GIT_ERROR_INVALID, "parent index %zu does not exist", n);
		return GIT_ENOTFOUND;
	}

	if (n == 0 || (n == 1 && entry->parent_count == 2))
		return git_commit_graph_entry_get_byindex(parent, file, entry->parent_indices[n]);

	return git_commit_graph_entry_get_byindex(
			parent,
			file,
			ntohl(
					*(uint32_t *)(file->extra_edge_list
						      + (entry->extra_parents_index + n - 1)
								      * sizeof(uint32_t)))
					& 0x7fffffff);
}

int git_commit_graph_file_close(git_commit_graph_file *file)
{
	GIT_ASSERT_ARG(file);

	if (file->graph_map.data)
		git_futils_mmap_free(&file->graph_map);

	return 0;
}

void git_commit_graph_free(git_commit_graph *cgraph)
{
	if (!cgraph)
		return;

	git_str_dispose(&cgraph->filename);
	git_commit_graph_file_free(cgraph->file);
	git__free(cgraph);
}

void git_commit_graph_file_free(git_commit_graph_file *file)
{
	if (!file)
		return;

	git_commit_graph_file_close(file);
	git__free(file);
}

static int packed_commit__cmp(const void *a_, const void *b_)
{
	const struct packed_commit *a = a_;
	const struct packed_commit *b = b_;
	return git_oid_cmp(&a->sha1, &b->sha1);
}

int git_commit_graph_writer_options_init(
	git_commit_graph_writer_options *opts,
	unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts,
		version,
		git_commit_graph_writer_options,
		GIT_COMMIT_GRAPH_WRITER_OPTIONS_INIT);
	return 0;
}

int git_commit_graph_writer_new(
	git_commit_graph_writer **out,
	const char *objects_info_dir,
	const git_commit_graph_writer_options *opts
	)
{
	git_commit_graph_writer *w;
	git_oid_t oid_type;

#ifdef GIT_EXPERIMENTAL_SHA256
	GIT_ERROR_CHECK_VERSION(opts,
		GIT_COMMIT_GRAPH_WRITER_OPTIONS_VERSION,
		"git_commit_graph_writer_options");

	oid_type = opts && opts->oid_type ? opts->oid_type : GIT_OID_DEFAULT;
	GIT_ASSERT_ARG(git_oid_type_is_valid(oid_type));
#else
	GIT_UNUSED(opts);
	oid_type = GIT_OID_SHA1;
#endif

	GIT_ASSERT_ARG(out && objects_info_dir);

	w = git__calloc(1, sizeof(git_commit_graph_writer));
	GIT_ERROR_CHECK_ALLOC(w);

	w->oid_type = oid_type;

	if (git_str_sets(&w->objects_info_dir, objects_info_dir) < 0) {
		git__free(w);
		return -1;
	}

	if (git_vector_init(&w->commits, 0, packed_commit__cmp) < 0) {
		git_str_dispose(&w->objects_info_dir);
		git__free(w);
		return -1;
	}

	*out = w;
	return 0;
}

void git_commit_graph_writer_free(git_commit_graph_writer *w)
{
	struct packed_commit *packed_commit;
	size_t i;

	if (!w)
		return;

	git_vector_foreach (&w->commits, i, packed_commit)
		packed_commit_free(packed_commit);
	git_vector_dispose(&w->commits);
	git_str_dispose(&w->objects_info_dir);
	git__free(w);
}

struct object_entry_cb_state {
	git_repository *repo;
	git_odb *db;
	git_vector *commits;
};

static int object_entry__cb(const git_oid *id, void *data)
{
	struct object_entry_cb_state *state = (struct object_entry_cb_state *)data;
	git_commit *commit = NULL;
	struct packed_commit *packed_commit = NULL;
	size_t header_len;
	git_object_t header_type;
	int error = 0;

	error = git_odb_read_header(&header_len, &header_type, state->db, id);
	if (error < 0)
		return error;

	if (header_type != GIT_OBJECT_COMMIT)
		return 0;

	error = git_commit_lookup(&commit, state->repo, id);
	if (error < 0)
		return error;

	packed_commit = packed_commit_new(commit);
	git_commit_free(commit);
	GIT_ERROR_CHECK_ALLOC(packed_commit);

	error = git_vector_insert(state->commits, packed_commit);
	if (error < 0) {
		packed_commit_free(packed_commit);
		return error;
	}

	return 0;
}

int git_commit_graph_writer_add_index_file(
	git_commit_graph_writer *w,
	git_repository *repo,
	const char *idx_path)
{
	int error;
	struct git_pack_file *p = NULL;
	struct object_entry_cb_state state = {0};
	state.repo = repo;
	state.commits = &w->commits;

	error = git_repository_odb(&state.db, repo);
	if (error < 0)
		goto cleanup;

	/* TODO: SHA256 */
	error = git_mwindow_get_pack(&p, idx_path, 0);
	if (error < 0)
		goto cleanup;

	error = git_pack_foreach_entry(p, object_entry__cb, &state);
	if (error < 0)
		goto cleanup;

cleanup:
	if (p)
		git_mwindow_put_pack(p);
	git_odb_free(state.db);
	return error;
}

int git_commit_graph_writer_add_revwalk(git_commit_graph_writer *w, git_revwalk *walk)
{
	int error;
	git_oid id;
	git_repository *repo = git_revwalk_repository(walk);
	git_commit *commit;
	struct packed_commit *packed_commit;

	while ((git_revwalk_next(&id, walk)) == 0) {
		error = git_commit_lookup(&commit, repo, &id);
		if (error < 0)
			return error;

		packed_commit = packed_commit_new(commit);
		git_commit_free(commit);
		GIT_ERROR_CHECK_ALLOC(packed_commit);

		error = git_vector_insert(&w->commits, packed_commit);
		if (error < 0) {
			packed_commit_free(packed_commit);
			return error;
		}
	}

	return 0;
}

enum generation_number_commit_state {
	GENERATION_NUMBER_COMMIT_STATE_UNVISITED = 0,
	GENERATION_NUMBER_COMMIT_STATE_ADDED = 1,
	GENERATION_NUMBER_COMMIT_STATE_EXPANDED = 2,
	GENERATION_NUMBER_COMMIT_STATE_VISITED = 3
};

GIT_HASHMAP_OID_SETUP(git_commit_graph_oidmap, struct packed_commit *);

static int compute_generation_numbers(git_vector *commits)
{
	git_array_t(size_t) index_stack = GIT_ARRAY_INIT;
	size_t i, j;
	size_t *parent_idx;
	enum generation_number_commit_state *commit_states = NULL;
	struct packed_commit *child_packed_commit;
	git_commit_graph_oidmap packed_commit_map = GIT_HASHMAP_INIT;
	int error = 0;

	/* First populate the parent indices fields */
	git_vector_foreach (commits, i, child_packed_commit) {
		child_packed_commit->index = i;
		error = git_commit_graph_oidmap_put(&packed_commit_map,
				&child_packed_commit->sha1, child_packed_commit);
		if (error < 0)
			goto cleanup;
	}

	git_vector_foreach (commits, i, child_packed_commit) {
		size_t parent_i, *parent_idx_ptr;
		struct packed_commit *parent_packed_commit;
		git_oid *parent_id;
		git_array_init_to_size(
				child_packed_commit->parent_indices,
				git_array_size(child_packed_commit->parents));
		if (git_array_size(child_packed_commit->parents)
		    && !child_packed_commit->parent_indices.ptr) {
			error = -1;
			goto cleanup;
		}
		git_array_foreach (child_packed_commit->parents, parent_i, parent_id) {
			if (git_commit_graph_oidmap_get(&parent_packed_commit, &packed_commit_map, parent_id) != 0) {
				git_error_set(GIT_ERROR_ODB,
					      "parent commit %s not found in commit graph",
					      git_oid_tostr_s(parent_id));
				error = GIT_ENOTFOUND;
				goto cleanup;
			}
			parent_idx_ptr = git_array_alloc(child_packed_commit->parent_indices);
			if (!parent_idx_ptr) {
				error = -1;
				goto cleanup;
			}
			*parent_idx_ptr = parent_packed_commit->index;
		}
	}

	/*
	 * We copy all the commits to the stack and then during visitation,
	 * each node can be added up to two times to the stack.
	 */
	git_array_init_to_size(index_stack, 3 * git_vector_length(commits));
	if (!index_stack.ptr) {
		error = -1;
		goto cleanup;
	}

	commit_states = (enum generation_number_commit_state *)git__calloc(
			git_vector_length(commits), sizeof(enum generation_number_commit_state));
	if (!commit_states) {
		error = -1;
		goto cleanup;
	}

	/*
	 * Perform a Post-Order traversal so that all parent nodes are fully
	 * visited before the child node.
	 */
	git_vector_foreach (commits, i, child_packed_commit)
		*(size_t *)git_array_alloc(index_stack) = i;

	while (git_array_size(index_stack)) {
		size_t *index_ptr = git_array_pop(index_stack);
		i = *index_ptr;
		child_packed_commit = git_vector_get(commits, i);

		if (commit_states[i] == GENERATION_NUMBER_COMMIT_STATE_VISITED) {
			/* This commit has already been fully visited. */
			continue;
		}
		if (commit_states[i] == GENERATION_NUMBER_COMMIT_STATE_EXPANDED) {
			/* All of the commits parents have been visited. */
			child_packed_commit->generation = 0;
			git_array_foreach (child_packed_commit->parent_indices, j, parent_idx) {
				struct packed_commit *parent = git_vector_get(commits, *parent_idx);
				if (child_packed_commit->generation < parent->generation)
					child_packed_commit->generation = parent->generation;
			}
			if (child_packed_commit->generation
			    < GIT_COMMIT_GRAPH_GENERATION_NUMBER_MAX) {
				++child_packed_commit->generation;
			}
			commit_states[i] = GENERATION_NUMBER_COMMIT_STATE_VISITED;
			continue;
		}

		/*
		 * This is the first time we see this commit. We need
		 * to visit all its parents before we can fully visit
		 * it.
		 */
		if (git_array_size(child_packed_commit->parent_indices) == 0) {
			/*
			 * Special case: if the commit has no parents, there's
			 * no need to add it to the stack just to immediately
			 * remove it.
			 */
			commit_states[i] = GENERATION_NUMBER_COMMIT_STATE_VISITED;
			child_packed_commit->generation = 1;
			continue;
		}

		/*
		 * Add this current commit again so that it is visited
		 * again once all its children have been visited.
		 */
		*(size_t *)git_array_alloc(index_stack) = i;
		git_array_foreach (child_packed_commit->parent_indices, j, parent_idx) {
			if (commit_states[*parent_idx]
			    != GENERATION_NUMBER_COMMIT_STATE_UNVISITED) {
				/* This commit has already been considered. */
				continue;
			}

			commit_states[*parent_idx] = GENERATION_NUMBER_COMMIT_STATE_ADDED;
			*(size_t *)git_array_alloc(index_stack) = *parent_idx;
		}
		commit_states[i] = GENERATION_NUMBER_COMMIT_STATE_EXPANDED;
	}

cleanup:
	git_commit_graph_oidmap_dispose(&packed_commit_map);
	git__free(commit_states);
	git_array_clear(index_stack);

	return error;
}

static int write_offset(off64_t offset, commit_graph_write_cb write_cb, void *cb_data)
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

static int write_chunk_header(
		int chunk_id,
		off64_t offset,
		commit_graph_write_cb write_cb,
		void *cb_data)
{
	uint32_t word = htonl(chunk_id);
	int error = write_cb((const char *)&word, sizeof(word), cb_data);
	if (error < 0)
		return error;
	return write_offset(offset, write_cb, cb_data);
}

static int commit_graph_write_buf(const char *buf, size_t size, void *data)
{
	git_str *b = (git_str *)data;
	return git_str_put(b, buf, size);
}

struct commit_graph_write_hash_context {
	commit_graph_write_cb write_cb;
	void *cb_data;
	git_hash_ctx *ctx;
};

static int commit_graph_write_hash(const char *buf, size_t size, void *data)
{
	struct commit_graph_write_hash_context *ctx = data;
	int error;

	if (ctx->ctx) {
		error = git_hash_update(ctx->ctx, buf, size);

		if (error < 0)
			return error;
	}

	return ctx->write_cb(buf, size, ctx->cb_data);
}

static void packed_commit_free_dup(void *packed_commit)
{
	packed_commit_free(packed_commit);
}

static int commit_graph_write(
	git_commit_graph_writer *w,
	commit_graph_write_cb write_cb,
	void *cb_data)
{
	int error = 0;
	size_t i;
	struct packed_commit *packed_commit;
	struct git_commit_graph_header hdr = {0};
	uint32_t oid_fanout_count;
	uint32_t extra_edge_list_count;
	uint32_t oid_fanout[256];
	off64_t offset;
	git_str oid_lookup = GIT_STR_INIT, commit_data = GIT_STR_INIT,
		extra_edge_list = GIT_STR_INIT;
	unsigned char checksum[GIT_HASH_MAX_SIZE];
	git_hash_algorithm_t checksum_type;
	size_t checksum_size, oid_size;
	git_hash_ctx ctx;
	struct commit_graph_write_hash_context hash_cb_data = {0};

	hdr.signature = htonl(COMMIT_GRAPH_SIGNATURE);
	hdr.version = COMMIT_GRAPH_VERSION;
	hdr.object_id_version = COMMIT_GRAPH_OBJECT_ID_VERSION;
	hdr.chunks = 0;
	hdr.base_graph_files = 0;
	hash_cb_data.write_cb = write_cb;
	hash_cb_data.cb_data = cb_data;
	hash_cb_data.ctx = &ctx;

	oid_size = git_oid_size(w->oid_type);
	checksum_type = git_oid_algorithm(w->oid_type);
	checksum_size = git_hash_size(checksum_type);

	error = git_hash_ctx_init(&ctx, checksum_type);
	if (error < 0)
		return error;
	cb_data = &hash_cb_data;
	write_cb = commit_graph_write_hash;

	/* Sort the commits. */
	git_vector_sort(&w->commits);
	git_vector_uniq(&w->commits, packed_commit_free_dup);
	error = compute_generation_numbers(&w->commits);
	if (error < 0)
		goto cleanup;

	/* Fill the OID Fanout table. */
	oid_fanout_count = 0;
	for (i = 0; i < 256; i++) {
		while (oid_fanout_count < git_vector_length(&w->commits) &&
		       (packed_commit = (struct packed_commit *)git_vector_get(&w->commits, oid_fanout_count)) &&
		       packed_commit->sha1.id[0] <= i)
			++oid_fanout_count;
		oid_fanout[i] = htonl(oid_fanout_count);
	}

	/* Fill the OID Lookup table. */
	git_vector_foreach (&w->commits, i, packed_commit) {
		error = git_str_put(&oid_lookup,
			(const char *)&packed_commit->sha1.id,
			oid_size);

		if (error < 0)
			goto cleanup;
	}

	/* Fill the Commit Data and Extra Edge List tables. */
	extra_edge_list_count = 0;
	git_vector_foreach (&w->commits, i, packed_commit) {
		uint64_t commit_time;
		uint32_t generation;
		uint32_t word;
		size_t *packed_index;
		unsigned int parentcount = (unsigned int)git_array_size(packed_commit->parents);

		error = git_str_put(&commit_data,
			(const char *)&packed_commit->tree_oid.id,
			oid_size);

		if (error < 0)
			goto cleanup;

		if (parentcount == 0) {
			word = htonl(GIT_COMMIT_GRAPH_MISSING_PARENT);
		} else {
			packed_index = git_array_get(packed_commit->parent_indices, 0);
			word = htonl((uint32_t)*packed_index);
		}
		error = git_str_put(&commit_data, (const char *)&word, sizeof(word));
		if (error < 0)
			goto cleanup;

		if (parentcount < 2) {
			word = htonl(GIT_COMMIT_GRAPH_MISSING_PARENT);
		} else if (parentcount == 2) {
			packed_index = git_array_get(packed_commit->parent_indices, 1);
			word = htonl((uint32_t)*packed_index);
		} else {
			word = htonl(0x80000000u | extra_edge_list_count);
		}
		error = git_str_put(&commit_data, (const char *)&word, sizeof(word));
		if (error < 0)
			goto cleanup;

		if (parentcount > 2) {
			unsigned int parent_i;
			for (parent_i = 1; parent_i < parentcount; ++parent_i) {
				packed_index = git_array_get(
					packed_commit->parent_indices, parent_i);
				word = htonl((uint32_t)(*packed_index | (parent_i + 1 == parentcount ? 0x80000000u : 0)));

				error = git_str_put(&extra_edge_list,
						(const char *)&word,
						sizeof(word));
				if (error < 0)
					goto cleanup;
			}
			extra_edge_list_count += parentcount - 1;
		}

		generation = packed_commit->generation;
		commit_time = (uint64_t)packed_commit->commit_time;
		if (generation > GIT_COMMIT_GRAPH_GENERATION_NUMBER_MAX)
			generation = GIT_COMMIT_GRAPH_GENERATION_NUMBER_MAX;
		word = ntohl((uint32_t)((generation << 2) | (((uint32_t)(commit_time >> 32)) & 0x3) ));
		error = git_str_put(&commit_data, (const char *)&word, sizeof(word));
		if (error < 0)
			goto cleanup;
		word = ntohl((uint32_t)(commit_time & 0xfffffffful));
		error = git_str_put(&commit_data, (const char *)&word, sizeof(word));
		if (error < 0)
			goto cleanup;
	}

	/* Write the header. */
	hdr.chunks = 3;
	if (git_str_len(&extra_edge_list) > 0)
		hdr.chunks++;
	error = write_cb((const char *)&hdr, sizeof(hdr), cb_data);
	if (error < 0)
		goto cleanup;

	/* Write the chunk headers. */
	offset = sizeof(hdr) + (hdr.chunks + 1) * 12;
	error = write_chunk_header(COMMIT_GRAPH_OID_FANOUT_ID, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;
	offset += sizeof(oid_fanout);
	error = write_chunk_header(COMMIT_GRAPH_OID_LOOKUP_ID, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;
	offset += git_str_len(&oid_lookup);
	error = write_chunk_header(COMMIT_GRAPH_COMMIT_DATA_ID, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;
	offset += git_str_len(&commit_data);
	if (git_str_len(&extra_edge_list) > 0) {
		error = write_chunk_header(
				COMMIT_GRAPH_EXTRA_EDGE_LIST_ID, offset, write_cb, cb_data);
		if (error < 0)
			goto cleanup;
		offset += git_str_len(&extra_edge_list);
	}
	error = write_chunk_header(0, offset, write_cb, cb_data);
	if (error < 0)
		goto cleanup;

	/* Write all the chunks. */
	error = write_cb((const char *)oid_fanout, sizeof(oid_fanout), cb_data);
	if (error < 0)
		goto cleanup;
	error = write_cb(git_str_cstr(&oid_lookup), git_str_len(&oid_lookup), cb_data);
	if (error < 0)
		goto cleanup;
	error = write_cb(git_str_cstr(&commit_data), git_str_len(&commit_data), cb_data);
	if (error < 0)
		goto cleanup;
	error = write_cb(git_str_cstr(&extra_edge_list), git_str_len(&extra_edge_list), cb_data);
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
	git_str_dispose(&oid_lookup);
	git_str_dispose(&commit_data);
	git_str_dispose(&extra_edge_list);
	git_hash_ctx_cleanup(&ctx);
	return error;
}

static int commit_graph_write_filebuf(const char *buf, size_t size, void *data)
{
	git_filebuf *f = (git_filebuf *)data;
	return git_filebuf_write(f, buf, size);
}

int git_commit_graph_writer_commit(git_commit_graph_writer *w)
{
	int error;
	int filebuf_flags = GIT_FILEBUF_DO_NOT_BUFFER;
	git_str commit_graph_path = GIT_STR_INIT;
	git_filebuf output = GIT_FILEBUF_INIT;

	error = git_str_joinpath(
			&commit_graph_path, git_str_cstr(&w->objects_info_dir), "commit-graph");
	if (error < 0)
		return error;

	if (git_repository__fsync_gitdir)
		filebuf_flags |= GIT_FILEBUF_FSYNC;
	error = git_filebuf_open(&output, git_str_cstr(&commit_graph_path), filebuf_flags, 0644);
	git_str_dispose(&commit_graph_path);
	if (error < 0)
		return error;

	error = commit_graph_write(w, commit_graph_write_filebuf, &output);
	if (error < 0) {
		git_filebuf_cleanup(&output);
		return error;
	}

	return git_filebuf_commit(&output);
}

int git_commit_graph_writer_dump(
	git_buf *cgraph,
	git_commit_graph_writer *w)
{
	GIT_BUF_WRAP_PRIVATE(cgraph, git_commit_graph__writer_dump, w);
}

int git_commit_graph__writer_dump(
	git_str *cgraph,
	git_commit_graph_writer *w)
{
	return commit_graph_write(w, commit_graph_write_buf, cgraph);
}
