/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include <zlib.h>
#include "git2/object.h"
#include "git2/sys/odb_backend.h"
#include "futils.h"
#include "hash.h"
#include "odb.h"
#include "delta.h"
#include "filebuf.h"
#include "object.h"
#include "zstream.h"

#include "git2/odb_backend.h"
#include "git2/types.h"

/* maximum possible header length */
#define MAX_HEADER_LEN 64

typedef struct { /* object header data */
	git_object_t type; /* object type */
	size_t	size; /* object size */
} obj_hdr;

typedef struct {
	git_odb_stream stream;
	git_filebuf fbuf;
} loose_writestream;

typedef struct {
	git_odb_stream stream;
	git_map map;
	char start[MAX_HEADER_LEN];
	size_t start_len;
	size_t start_read;
	git_zstream zstream;
} loose_readstream;

typedef struct loose_backend {
	git_odb_backend parent;

	git_odb_backend_loose_options options;
	size_t oid_hexsize;

	size_t objects_dirlen;
	char objects_dir[GIT_FLEX_ARRAY];
} loose_backend;

/* State structure for exploring directories,
 * in order to locate objects matching a short oid.
 */
typedef struct {
	loose_backend *backend;

	size_t dir_len;

	/* Hex formatted oid to match (and its length) */
	unsigned char short_oid[GIT_OID_MAX_HEXSIZE];
	size_t short_oid_len;

	/* Number of matching objects found so far */
	int found;

	/* Hex formatted oid of the object found */
	unsigned char res_oid[GIT_OID_MAX_HEXSIZE];
} loose_locate_object_state;


/***********************************************************
 *
 * MISCELLANEOUS HELPER FUNCTIONS
 *
 ***********************************************************/

static int object_file_name(
	git_str *name, const loose_backend *be, const git_oid *id)
{
	/* append loose object filename: aa/aaa... (41 bytes plus NUL) */
	size_t path_size = be->oid_hexsize + 1;

	git_str_set(name, be->objects_dir, be->objects_dirlen);
	git_fs_path_to_dir(name);

	if (git_str_grow_by(name, path_size + 1) < 0)
		return -1;

	git_oid_pathfmt(name->ptr + name->size, id);
	name->size += path_size;
	name->ptr[name->size] = '\0';

	return 0;
}

static int object_mkdir(const git_str *name, const loose_backend *be)
{
	return git_futils_mkdir_relative(
		name->ptr + be->objects_dirlen,
		be->objects_dir,
		be->options.dir_mode,
		GIT_MKDIR_PATH | GIT_MKDIR_SKIP_LAST | GIT_MKDIR_VERIFY_DIR, NULL);
}

static int parse_header_packlike(
	obj_hdr *out, size_t *out_len, const unsigned char *data, size_t len)
{
	unsigned long c;
	size_t shift, size, used = 0;

	if (len == 0)
		goto on_error;

	c = data[used++];
	out->type = (c >> 4) & 7;

	size = c & 15;
	shift = 4;
	while (c & 0x80) {
		if (len <= used)
			goto on_error;

		if (sizeof(size_t) * 8 <= shift)
			goto on_error;

		c = data[used++];
		size += (c & 0x7f) << shift;
		shift += 7;
	}

	out->size = size;

	if (out_len)
		*out_len = used;

	return 0;

on_error:
	git_error_set(GIT_ERROR_OBJECT, "failed to parse loose object: invalid header");
	return -1;
}

static int parse_header(
	obj_hdr *out,
       	size_t *out_len,
	const unsigned char *_data,
	size_t data_len)
{
	const char *data = (char *)_data;
	size_t i, typename_len, size_idx, size_len;
	int64_t size;

	*out_len = 0;

	/* find the object type name */
	for (i = 0, typename_len = 0; i < data_len; i++, typename_len++) {
		if (data[i] == ' ')
			break;
	}

	if (typename_len == data_len)
		goto on_error;

	out->type = git_object_stringn2type(data, typename_len);

	size_idx = typename_len + 1;
	for (i = size_idx, size_len = 0; i < data_len; i++, size_len++) {
		if (data[i] == '\0')
			break;
	}

	if (i == data_len)
		goto on_error;

	if (git__strntol64(&size, &data[size_idx], size_len, NULL, 10) < 0 ||
		size < 0)
		goto on_error;

	if ((uint64_t)size > SIZE_MAX) {
		git_error_set(GIT_ERROR_OBJECT, "object is larger than available memory");
		return -1;
	}

	out->size = (size_t)size;

	if (GIT_ADD_SIZET_OVERFLOW(out_len, i, 1))
		goto on_error;

	return 0;

on_error:
	git_error_set(GIT_ERROR_OBJECT, "failed to parse loose object: invalid header");
	return -1;
}

static int is_zlib_compressed_data(unsigned char *data, size_t data_len)
{
	unsigned int w;

	if (data_len < 2)
		return 0;

	w = ((unsigned int)(data[0]) << 8) + data[1];
	return (data[0] & 0x8F) == 0x08 && !(w % 31);
}

/***********************************************************
 *
 * ODB OBJECT READING & WRITING
 *
 * Backend for the public API; read headers and full objects
 * from the ODB. Write raw data to the ODB.
 *
 ***********************************************************/


/*
 * At one point, there was a loose object format that was intended to
 * mimic the format used in pack-files. This was to allow easy copying
 * of loose object data into packs. This format is no longer used, but
 * we must still read it.
 */
static int read_loose_packlike(git_rawobj *out, git_str *obj)
{
	git_str body = GIT_STR_INIT;
	const unsigned char *obj_data;
	obj_hdr hdr;
	size_t obj_len, head_len, alloc_size;
	int error;

	obj_data = (unsigned char *)obj->ptr;
	obj_len = obj->size;

	/*
	 * read the object header, which is an (uncompressed)
	 * binary encoding of the object type and size.
	 */
	if ((error = parse_header_packlike(&hdr, &head_len, obj_data, obj_len)) < 0)
		goto done;

	if (!git_object_typeisloose(hdr.type) || head_len > obj_len) {
		git_error_set(GIT_ERROR_ODB, "failed to inflate loose object");
		error = -1;
		goto done;
	}

	obj_data += head_len;
	obj_len -= head_len;

	/*
	 * allocate a buffer and inflate the data into it
	 */
	if (GIT_ADD_SIZET_OVERFLOW(&alloc_size, hdr.size, 1) ||
		git_str_init(&body, alloc_size) < 0) {
		error = -1;
		goto done;
	}

	if ((error = git_zstream_inflatebuf(&body, obj_data, obj_len)) < 0)
		goto done;

	out->len = hdr.size;
	out->type = hdr.type;
	out->data = git_str_detach(&body);

done:
	git_str_dispose(&body);
	return error;
}

static int read_loose_standard(git_rawobj *out, git_str *obj)
{
	git_zstream zstream = GIT_ZSTREAM_INIT;
	unsigned char head[MAX_HEADER_LEN], *body = NULL;
	size_t decompressed, head_len, body_len, alloc_size;
	obj_hdr hdr;
	int error;

	if ((error = git_zstream_init(&zstream, GIT_ZSTREAM_INFLATE)) < 0 ||
		(error = git_zstream_set_input(&zstream, git_str_cstr(obj), git_str_len(obj))) < 0)
		goto done;

	decompressed = sizeof(head);

	/*
	 * inflate the initial part of the compressed buffer in order to
	 * parse the header; read the largest header possible, then push the
	 * remainder into the body buffer.
	 */
	if ((error = git_zstream_get_output(head, &decompressed, &zstream)) < 0 ||
		(error = parse_header(&hdr, &head_len, head, decompressed)) < 0)
		goto done;

	if (!git_object_typeisloose(hdr.type)) {
		git_error_set(GIT_ERROR_ODB, "failed to inflate disk object");
		error = -1;
		goto done;
	}

	/*
	 * allocate a buffer and inflate the object data into it
	 * (including the initial sequence in the head buffer).
	 */
	if (GIT_ADD_SIZET_OVERFLOW(&alloc_size, hdr.size, 1) ||
		(body = git__calloc(1, alloc_size)) == NULL) {
		error = -1;
		goto done;
	}

	GIT_ASSERT(decompressed >= head_len);
	body_len = decompressed - head_len;

	if (body_len)
		memcpy(body, head + head_len, body_len);

	decompressed = hdr.size - body_len;
	if ((error = git_zstream_get_output(body + body_len, &decompressed, &zstream)) < 0)
		goto done;

	if (!git_zstream_done(&zstream)) {
		git_error_set(GIT_ERROR_ZLIB, "failed to finish zlib inflation: stream aborted prematurely");
		error = -1;
		goto done;
	}

	body[hdr.size] = '\0';

	out->data = body;
	out->len = hdr.size;
	out->type = hdr.type;

done:
	if (error < 0)
		git__free(body);

	git_zstream_free(&zstream);
	return error;
}

static int read_loose(git_rawobj *out, git_str *loc)
{
	int error;
	git_str obj = GIT_STR_INIT;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(loc);

	if (git_str_oom(loc))
		return -1;

	out->data = NULL;
	out->len = 0;
	out->type = GIT_OBJECT_INVALID;

	if ((error = git_futils_readbuffer(&obj, loc->ptr)) < 0)
		goto done;

	if (!is_zlib_compressed_data((unsigned char *)obj.ptr, obj.size))
		error = read_loose_packlike(out, &obj);
	else
		error = read_loose_standard(out, &obj);

done:
	git_str_dispose(&obj);
	return error;
}

static int read_header_loose_packlike(
	git_rawobj *out, const unsigned char *data, size_t len)
{
	obj_hdr hdr;
	size_t header_len;
	int error;

	if ((error = parse_header_packlike(&hdr, &header_len, data, len)) < 0)
		return error;

	out->len = hdr.size;
	out->type = hdr.type;

	return error;
}

static int read_header_loose_standard(
	git_rawobj *out, const unsigned char *data, size_t len)
{
	git_zstream zs = GIT_ZSTREAM_INIT;
	obj_hdr hdr = {0};
	unsigned char inflated[MAX_HEADER_LEN] = {0};
	size_t header_len, inflated_len = sizeof(inflated);
	int error;

	if ((error = git_zstream_init(&zs, GIT_ZSTREAM_INFLATE)) < 0 ||
		(error = git_zstream_set_input(&zs, data, len)) < 0 ||
		(error = git_zstream_get_output_chunk(inflated, &inflated_len, &zs)) < 0 ||
		(error = parse_header(&hdr, &header_len, inflated, inflated_len)) < 0)
		goto done;

	out->len = hdr.size;
	out->type = hdr.type;

done:
	git_zstream_free(&zs);
	return error;
}

static int read_header_loose(git_rawobj *out, git_str *loc)
{
	unsigned char obj[1024];
	ssize_t obj_len;
	int fd, error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(loc);

	if (git_str_oom(loc))
		return -1;

	out->data = NULL;

	if ((error = fd = git_futils_open_ro(loc->ptr)) < 0)
		goto done;

	if ((obj_len = p_read(fd, obj, sizeof(obj))) < 0) {
		error = (int)obj_len;
		goto done;
	}

	if (!is_zlib_compressed_data(obj, (size_t)obj_len))
		error = read_header_loose_packlike(out, obj, (size_t)obj_len);
	else
		error = read_header_loose_standard(out, obj, (size_t)obj_len);

	if (!error && !git_object_typeisloose(out->type)) {
		git_error_set(GIT_ERROR_ZLIB, "failed to read loose object header");
		error = -1;
		goto done;
	}

done:
	if (fd >= 0)
		p_close(fd);
	return error;
}

static int locate_object(
	git_str *object_location,
	loose_backend *backend,
	const git_oid *oid)
{
	int error = object_file_name(object_location, backend, oid);

	if (!error && !git_fs_path_exists(object_location->ptr))
		return GIT_ENOTFOUND;

	return error;
}

/* Explore an entry of a directory and see if it matches a short oid */
static int fn_locate_object_short_oid(void *state, git_str *pathbuf) {
	loose_locate_object_state *sstate = (loose_locate_object_state *)state;
	size_t hex_size = sstate->backend->oid_hexsize;

	if (git_str_len(pathbuf) - sstate->dir_len != hex_size - 2) {
		/* Entry cannot be an object. Continue to next entry */
		return 0;
	}

	if (git_fs_path_isdir(pathbuf->ptr) == false) {
		/* We are already in the directory matching the 2 first hex characters,
		 * compare the first ncmp characters of the oids */
		if (!memcmp(sstate->short_oid + 2,
			(unsigned char *)pathbuf->ptr + sstate->dir_len,
			sstate->short_oid_len - 2)) {

			if (!sstate->found) {
				sstate->res_oid[0] = sstate->short_oid[0];
				sstate->res_oid[1] = sstate->short_oid[1];
				memcpy(sstate->res_oid + 2,
				       pathbuf->ptr+sstate->dir_len,
				       hex_size - 2);
			}
			sstate->found++;
		}
	}

	if (sstate->found > 1)
		return GIT_EAMBIGUOUS;

	return 0;
}

/* Locate an object matching a given short oid */
static int locate_object_short_oid(
	git_str *object_location,
	git_oid *res_oid,
	loose_backend *backend,
	const git_oid *short_oid,
	size_t len)
{
	char *objects_dir = backend->objects_dir;
	size_t dir_len = strlen(objects_dir), alloc_len;
	loose_locate_object_state state;
	int error;

	/* prealloc memory for OBJ_DIR/xx/xx..38x..xx */
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, dir_len, backend->oid_hexsize);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 3);
	if (git_str_grow(object_location, alloc_len) < 0)
		return -1;

	git_str_set(object_location, objects_dir, dir_len);
	git_fs_path_to_dir(object_location);

	/* save adjusted position at end of dir so it can be restored later */
	dir_len = git_str_len(object_location);

	/* Convert raw oid to hex formatted oid */
	git_oid_fmt((char *)state.short_oid, short_oid);

	/* Explore OBJ_DIR/xx/ where xx is the beginning of hex formatted short oid */
	if (git_str_put(object_location, (char *)state.short_oid, 3) < 0)
		return -1;
	object_location->ptr[object_location->size - 1] = '/';

	/* Check that directory exists */
	if (git_fs_path_isdir(object_location->ptr) == false)
		return git_odb__error_notfound("no matching loose object for prefix",
			short_oid, len);

	state.backend = backend;
	state.dir_len = git_str_len(object_location);
	state.short_oid_len = len;
	state.found = 0;

	/* Explore directory to find a unique object matching short_oid */
	error = git_fs_path_direach(
		object_location, 0, fn_locate_object_short_oid, &state);
	if (error < 0 && error != GIT_EAMBIGUOUS)
		return error;

	if (!state.found)
		return git_odb__error_notfound("no matching loose object for prefix",
			short_oid, len);

	if (state.found > 1)
		return git_odb__error_ambiguous("multiple matches in loose objects");

	/* Convert obtained hex formatted oid to raw */
	error = git_oid__fromstr(res_oid, (char *)state.res_oid, backend->options.oid_type);
	if (error)
		return error;

	/* Update the location according to the oid obtained */
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, dir_len, backend->oid_hexsize);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 2);

	git_str_truncate(object_location, dir_len);
	if (git_str_grow(object_location, alloc_len) < 0)
		return -1;

	git_oid_pathfmt(object_location->ptr + dir_len, res_oid);

	object_location->size +=  backend->oid_hexsize + 1;
	object_location->ptr[object_location->size] = '\0';

	return 0;
}

/***********************************************************
 *
 * LOOSE BACKEND PUBLIC API
 *
 * Implement the git_odb_backend API calls
 *
 ***********************************************************/

static int loose_backend__read_header(size_t *len_p, git_object_t *type_p, git_odb_backend *backend, const git_oid *oid)
{
	git_str object_path = GIT_STR_INIT;
	git_rawobj raw;
	int error;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(oid);

	raw.len = 0;
	raw.type = GIT_OBJECT_INVALID;

	if (locate_object(&object_path, (loose_backend *)backend, oid) < 0) {
		error = git_odb__error_notfound("no matching loose object",
			oid, ((struct loose_backend *)backend)->oid_hexsize);
	} else if ((error = read_header_loose(&raw, &object_path)) == 0) {
		*len_p = raw.len;
		*type_p = raw.type;
	}

	git_str_dispose(&object_path);

	return error;
}

static int loose_backend__read(void **buffer_p, size_t *len_p, git_object_t *type_p, git_odb_backend *backend, const git_oid *oid)
{
	git_str object_path = GIT_STR_INIT;
	git_rawobj raw;
	int error = 0;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(oid);

	if (locate_object(&object_path, (loose_backend *)backend, oid) < 0) {
		error = git_odb__error_notfound("no matching loose object",
			oid, ((struct loose_backend *)backend)->oid_hexsize);
	} else if ((error = read_loose(&raw, &object_path)) == 0) {
		*buffer_p = raw.data;
		*len_p = raw.len;
		*type_p = raw.type;
	}

	git_str_dispose(&object_path);

	return error;
}

static int loose_backend__read_prefix(
	git_oid *out_oid,
	void **buffer_p,
	size_t *len_p,
	git_object_t *type_p,
	git_odb_backend *_backend,
	const git_oid *short_oid,
	size_t len)
{
	struct loose_backend *backend = (struct loose_backend *)_backend;
	int error = 0;

	GIT_ASSERT_ARG(len >= GIT_OID_MINPREFIXLEN &&
	               len <= backend->oid_hexsize);

	if (len == backend->oid_hexsize) {
		/* We can fall back to regular read method */
		error = loose_backend__read(buffer_p, len_p, type_p, _backend, short_oid);
		if (!error)
			git_oid_cpy(out_oid, short_oid);
	} else {
		git_str object_path = GIT_STR_INIT;
		git_rawobj raw;

		GIT_ASSERT_ARG(backend && short_oid);

		if ((error = locate_object_short_oid(&object_path, out_oid,
				(loose_backend *)backend, short_oid, len)) == 0 &&
			(error = read_loose(&raw, &object_path)) == 0)
		{
			*buffer_p = raw.data;
			*len_p = raw.len;
			*type_p = raw.type;
		}

		git_str_dispose(&object_path);
	}

	return error;
}

static int loose_backend__exists(git_odb_backend *backend, const git_oid *oid)
{
	git_str object_path = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(oid);

	error = locate_object(&object_path, (loose_backend *)backend, oid);

	git_str_dispose(&object_path);

	return !error;
}

static int loose_backend__exists_prefix(
	git_oid *out, git_odb_backend *backend, const git_oid *short_id, size_t len)
{
	git_str object_path = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(short_id);
	GIT_ASSERT_ARG(len >= GIT_OID_MINPREFIXLEN);

	error = locate_object_short_oid(
		&object_path, out, (loose_backend *)backend, short_id, len);

	git_str_dispose(&object_path);

	return error;
}

struct foreach_state {
	struct loose_backend *backend;
	size_t dir_len;
	git_odb_foreach_cb cb;
	void *data;
};

GIT_INLINE(int) filename_to_oid(struct loose_backend *backend, git_oid *oid, const char *ptr)
{
	int v;
	size_t i = 0;

	if (strlen(ptr) != backend->oid_hexsize + 1)
		return -1;

	if (ptr[2] != '/') {
		return -1;
	}

	v = (git__fromhex(ptr[i]) << 4) | git__fromhex(ptr[i+1]);
	if (v < 0)
		return -1;

	oid->id[0] = (unsigned char) v;

	ptr += 3;
	for (i = 0; i < backend->oid_hexsize - 2; i += 2) {
		v = (git__fromhex(ptr[i]) << 4) | git__fromhex(ptr[i + 1]);
		if (v < 0)
			return -1;

		oid->id[1 + i/2] = (unsigned char) v;
	}

#ifdef GIT_EXPERIMENTAL_SHA256
	oid->type = backend->options.oid_type;
#endif

	return 0;
}

static int foreach_object_dir_cb(void *_state, git_str *path)
{
	git_oid oid;
	struct foreach_state *state = (struct foreach_state *) _state;

	if (filename_to_oid(state->backend, &oid, path->ptr + state->dir_len) < 0)
		return 0;

	return git_error_set_after_callback_function(
		state->cb(&oid, state->data), "git_odb_foreach");
}

static int foreach_cb(void *_state, git_str *path)
{
	struct foreach_state *state = (struct foreach_state *) _state;

	/* non-dir is some stray file, ignore it */
	if (!git_fs_path_isdir(git_str_cstr(path)))
		return 0;

	return git_fs_path_direach(path, 0, foreach_object_dir_cb, state);
}

static int loose_backend__foreach(git_odb_backend *_backend, git_odb_foreach_cb cb, void *data)
{
	char *objects_dir;
	int error;
	git_str buf = GIT_STR_INIT;
	struct foreach_state state;
	loose_backend *backend = (loose_backend *) _backend;

	GIT_ASSERT_ARG(backend);
	GIT_ASSERT_ARG(cb);

	objects_dir = backend->objects_dir;

	git_str_sets(&buf, objects_dir);
	git_fs_path_to_dir(&buf);
	if (git_str_oom(&buf))
		return -1;

	memset(&state, 0, sizeof(state));
	state.backend = backend;
	state.cb = cb;
	state.data = data;
	state.dir_len = git_str_len(&buf);

	error = git_fs_path_direach(&buf, 0, foreach_cb, &state);

	git_str_dispose(&buf);

	return error;
}

static int loose_backend__writestream_finalize(git_odb_stream *_stream, const git_oid *oid)
{
	loose_writestream *stream = (loose_writestream *)_stream;
	loose_backend *backend = (loose_backend *)_stream->backend;
	git_str final_path = GIT_STR_INIT;
	int error = 0;

	if (object_file_name(&final_path, backend, oid) < 0 ||
		object_mkdir(&final_path, backend) < 0)
		error = -1;
	else
		error = git_filebuf_commit_at(
			&stream->fbuf, final_path.ptr);

	git_str_dispose(&final_path);

	return error;
}

static int loose_backend__writestream_write(git_odb_stream *_stream, const char *data, size_t len)
{
	loose_writestream *stream = (loose_writestream *)_stream;
	return git_filebuf_write(&stream->fbuf, data, len);
}

static void loose_backend__writestream_free(git_odb_stream *_stream)
{
	loose_writestream *stream = (loose_writestream *)_stream;

	git_filebuf_cleanup(&stream->fbuf);
	git__free(stream);
}

static int filebuf_flags(loose_backend *backend)
{
	int flags = GIT_FILEBUF_TEMPORARY |
		(backend->options.compression_level << GIT_FILEBUF_DEFLATE_SHIFT);

	if ((backend->options.flags & GIT_ODB_BACKEND_LOOSE_FSYNC) ||
	    git_repository__fsync_gitdir)
		flags |= GIT_FILEBUF_FSYNC;

	return flags;
}

static int loose_backend__writestream(git_odb_stream **stream_out, git_odb_backend *_backend, git_object_size_t length, git_object_t type)
{
	loose_backend *backend;
	loose_writestream *stream = NULL;
	char hdr[MAX_HEADER_LEN];
	git_str tmp_path = GIT_STR_INIT;
	size_t hdrlen;
	int error;

	GIT_ASSERT_ARG(_backend);

	backend = (loose_backend *)_backend;
	*stream_out = NULL;

	if ((error = git_odb__format_object_header(&hdrlen,
		hdr, sizeof(hdr), length, type)) < 0)
		return error;

	stream = git__calloc(1, sizeof(loose_writestream));
	GIT_ERROR_CHECK_ALLOC(stream);

	stream->stream.backend = _backend;
	stream->stream.read = NULL; /* read only */
	stream->stream.write = &loose_backend__writestream_write;
	stream->stream.finalize_write = &loose_backend__writestream_finalize;
	stream->stream.free = &loose_backend__writestream_free;
	stream->stream.mode = GIT_STREAM_WRONLY;

	if (git_str_joinpath(&tmp_path, backend->objects_dir, "tmp_object") < 0 ||
		git_filebuf_open(&stream->fbuf, tmp_path.ptr, filebuf_flags(backend),
			backend->options.file_mode) < 0 ||
		stream->stream.write((git_odb_stream *)stream, hdr, hdrlen) < 0)
	{
		git_filebuf_cleanup(&stream->fbuf);
		git__free(stream);
		stream = NULL;
	}
	git_str_dispose(&tmp_path);
	*stream_out = (git_odb_stream *)stream;

	return !stream ? -1 : 0;
}

static int loose_backend__readstream_read(
	git_odb_stream *_stream,
	char *buffer,
	size_t buffer_len)
{
	loose_readstream *stream = (loose_readstream *)_stream;
	size_t start_remain = stream->start_len - stream->start_read;
	int total = 0, error;

	buffer_len = min(buffer_len, INT_MAX);

	/*
	 * if we read more than just the header in the initial read, play
	 * that back for the caller.
	 */
	if (start_remain && buffer_len) {
		size_t chunk = min(start_remain, buffer_len);
		memcpy(buffer, stream->start + stream->start_read, chunk);

		buffer += chunk;
		stream->start_read += chunk;

		total += (int)chunk;
		buffer_len -= chunk;
	}

	if (buffer_len) {
		size_t chunk = buffer_len;

		if ((error = git_zstream_get_output(buffer, &chunk, &stream->zstream)) < 0)
			return error;

		total += (int)chunk;
	}

	return (int)total;
}

static void loose_backend__readstream_free(git_odb_stream *_stream)
{
	loose_readstream *stream = (loose_readstream *)_stream;

	git_futils_mmap_free(&stream->map);
	git_zstream_free(&stream->zstream);
	git__free(stream);
}

static int loose_backend__readstream_packlike(
	obj_hdr *hdr,
	loose_readstream *stream)
{
	const unsigned char *data;
	size_t data_len, head_len;
	int error;

	data = stream->map.data;
	data_len = stream->map.len;

	/*
	 * read the object header, which is an (uncompressed)
	 * binary encoding of the object type and size.
	 */
	if ((error = parse_header_packlike(hdr, &head_len, data, data_len)) < 0)
		return error;

	if (!git_object_typeisloose(hdr->type)) {
		git_error_set(GIT_ERROR_ODB, "failed to inflate loose object");
		return -1;
	}

	return git_zstream_set_input(&stream->zstream,
		data + head_len, data_len - head_len);
}

static int loose_backend__readstream_standard(
	obj_hdr *hdr,
	loose_readstream *stream)
{
	unsigned char head[MAX_HEADER_LEN];
	size_t init, head_len;
	int error;

	if ((error = git_zstream_set_input(&stream->zstream,
			stream->map.data, stream->map.len)) < 0)
		return error;

	init = sizeof(head);

	/*
	 * inflate the initial part of the compressed buffer in order to
	 * parse the header; read the largest header possible, then store
	 * it in the `start` field of the stream object.
	 */
	if ((error = git_zstream_get_output(head, &init, &stream->zstream)) < 0 ||
		(error = parse_header(hdr, &head_len, head, init)) < 0)
		return error;

	if (!git_object_typeisloose(hdr->type)) {
		git_error_set(GIT_ERROR_ODB, "failed to inflate disk object");
		return -1;
	}

	if (init > head_len) {
		stream->start_len = init - head_len;
		memcpy(stream->start, head + head_len, init - head_len);
	}

	return 0;
}

static int loose_backend__readstream(
	git_odb_stream **stream_out,
	size_t *len_out,
	git_object_t *type_out,
	git_odb_backend *_backend,
	const git_oid *oid)
{
	loose_backend *backend;
	loose_readstream *stream = NULL;
	git_hash_ctx *hash_ctx = NULL;
	git_str object_path = GIT_STR_INIT;
	git_hash_algorithm_t algorithm;
	obj_hdr hdr;
	int error = 0;

	GIT_ASSERT_ARG(stream_out);
	GIT_ASSERT_ARG(len_out);
	GIT_ASSERT_ARG(type_out);
	GIT_ASSERT_ARG(_backend);
	GIT_ASSERT_ARG(oid);

	backend = (loose_backend *)_backend;
	*stream_out = NULL;
	*len_out = 0;
	*type_out = GIT_OBJECT_INVALID;

	if (locate_object(&object_path, backend, oid) < 0) {
		error = git_odb__error_notfound("no matching loose object",
			oid, backend->oid_hexsize);
		goto done;
	}

	stream = git__calloc(1, sizeof(loose_readstream));
	GIT_ERROR_CHECK_ALLOC(stream);

	hash_ctx = git__malloc(sizeof(git_hash_ctx));
	GIT_ERROR_CHECK_ALLOC(hash_ctx);

	algorithm = git_oid_algorithm(backend->options.oid_type);

	if ((error = git_hash_ctx_init(hash_ctx, algorithm)) < 0 ||
	    (error = git_futils_mmap_ro_file(&stream->map, object_path.ptr)) < 0 ||
	    (error = git_zstream_init(&stream->zstream, GIT_ZSTREAM_INFLATE)) < 0)
		goto done;

	/* check for a packlike loose object */
	if (!is_zlib_compressed_data(stream->map.data, stream->map.len))
		error = loose_backend__readstream_packlike(&hdr, stream);
	else
		error = loose_backend__readstream_standard(&hdr, stream);

	if (error < 0)
		goto done;

	stream->stream.backend = _backend;
	stream->stream.hash_ctx = hash_ctx;
	stream->stream.read = &loose_backend__readstream_read;
	stream->stream.free = &loose_backend__readstream_free;

	*stream_out = (git_odb_stream *)stream;
	*len_out = hdr.size;
	*type_out = hdr.type;

done:
	if (error < 0) {
		if (stream) {
			git_futils_mmap_free(&stream->map);
			git_zstream_free(&stream->zstream);
			git__free(stream);
		}
		if (hash_ctx) {
			git_hash_ctx_cleanup(hash_ctx);
			git__free(hash_ctx);
		}
	}

	git_str_dispose(&object_path);
	return error;
}

static int loose_backend__write(git_odb_backend *_backend, const git_oid *oid, const void *data, size_t len, git_object_t type)
{
	int error = 0;
	git_str final_path = GIT_STR_INIT;
	char header[MAX_HEADER_LEN];
	size_t header_len;
	git_filebuf fbuf = GIT_FILEBUF_INIT;
	loose_backend *backend;

	backend = (loose_backend *)_backend;

	/* prepare the header for the file */
	if ((error = git_odb__format_object_header(&header_len,
		header, sizeof(header), len, type)) < 0)
		goto cleanup;

	if (git_str_joinpath(&final_path, backend->objects_dir, "tmp_object") < 0 ||
		git_filebuf_open(&fbuf, final_path.ptr, filebuf_flags(backend),
			backend->options.file_mode) < 0)
	{
		error = -1;
		goto cleanup;
	}

	git_filebuf_write(&fbuf, header, header_len);
	git_filebuf_write(&fbuf, data, len);

	if (object_file_name(&final_path, backend, oid) < 0 ||
		object_mkdir(&final_path, backend) < 0 ||
		git_filebuf_commit_at(&fbuf, final_path.ptr) < 0)
		error = -1;

cleanup:
	if (error < 0)
		git_filebuf_cleanup(&fbuf);
	git_str_dispose(&final_path);
	return error;
}

static int loose_backend__freshen(
	git_odb_backend *_backend,
	const git_oid *oid)
{
	loose_backend *backend = (loose_backend *)_backend;
	git_str path = GIT_STR_INIT;
	int error;

	if (object_file_name(&path, backend, oid) < 0)
		return -1;

	error = git_futils_touch(path.ptr, NULL);
	git_str_dispose(&path);

	return error;
}

static void loose_backend__free(git_odb_backend *_backend)
{
	git__free(_backend);
}

static void normalize_options(
	git_odb_backend_loose_options *opts,
	const git_odb_backend_loose_options *given_opts)
{
	git_odb_backend_loose_options init = GIT_ODB_BACKEND_LOOSE_OPTIONS_INIT;

	if (given_opts)
		memcpy(opts, given_opts, sizeof(git_odb_backend_loose_options));
	else
		memcpy(opts, &init, sizeof(git_odb_backend_loose_options));

	if (opts->compression_level < 0)
		opts->compression_level = Z_BEST_SPEED;

	if (opts->dir_mode == 0)
		opts->dir_mode = GIT_OBJECT_DIR_MODE;

	if (opts->file_mode == 0)
		opts->file_mode = GIT_OBJECT_FILE_MODE;

	if (opts->oid_type == 0)
		opts->oid_type = GIT_OID_DEFAULT;
}

int git_odb__backend_loose(
	git_odb_backend **backend_out,
	const char *objects_dir,
	git_odb_backend_loose_options *opts)
{
	loose_backend *backend;
	size_t objects_dirlen, alloclen;

	GIT_ASSERT_ARG(backend_out);
	GIT_ASSERT_ARG(objects_dir);

	objects_dirlen = strlen(objects_dir);

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, sizeof(loose_backend), objects_dirlen);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 2);
	backend = git__calloc(1, alloclen);
	GIT_ERROR_CHECK_ALLOC(backend);

	backend->parent.version = GIT_ODB_BACKEND_VERSION;
	backend->objects_dirlen = objects_dirlen;
	memcpy(backend->objects_dir, objects_dir, objects_dirlen);

	if (backend->objects_dir[backend->objects_dirlen - 1] != '/')
		backend->objects_dir[backend->objects_dirlen++] = '/';

	normalize_options(&backend->options, opts);
	backend->oid_hexsize = git_oid_hexsize(backend->options.oid_type);

	backend->parent.read = &loose_backend__read;
	backend->parent.write = &loose_backend__write;
	backend->parent.read_prefix = &loose_backend__read_prefix;
	backend->parent.read_header = &loose_backend__read_header;
	backend->parent.writestream = &loose_backend__writestream;
	backend->parent.readstream = &loose_backend__readstream;
	backend->parent.exists = &loose_backend__exists;
	backend->parent.exists_prefix = &loose_backend__exists_prefix;
	backend->parent.foreach = &loose_backend__foreach;
	backend->parent.freshen = &loose_backend__freshen;
	backend->parent.free = &loose_backend__free;

	*backend_out = (git_odb_backend *)backend;
	return 0;
}


#ifdef GIT_EXPERIMENTAL_SHA256
int git_odb_backend_loose(
	git_odb_backend **backend_out,
	const char *objects_dir,
	git_odb_backend_loose_options *opts)
{
	return git_odb__backend_loose(backend_out, objects_dir, opts);
}
#else
int git_odb_backend_loose(
	git_odb_backend **backend_out,
	const char *objects_dir,
	int compression_level,
	int do_fsync,
	unsigned int dir_mode,
	unsigned int file_mode)
{
	git_odb_backend_loose_flag_t flags = 0;
	git_odb_backend_loose_options opts = GIT_ODB_BACKEND_LOOSE_OPTIONS_INIT;

	if (do_fsync)
		flags |= GIT_ODB_BACKEND_LOOSE_FSYNC;

	opts.flags = flags;
	opts.compression_level = compression_level;
	opts.dir_mode = dir_mode;
	opts.file_mode = file_mode;
	opts.oid_type = GIT_OID_DEFAULT;

	return git_odb__backend_loose(backend_out, objects_dir, &opts);
}
#endif
