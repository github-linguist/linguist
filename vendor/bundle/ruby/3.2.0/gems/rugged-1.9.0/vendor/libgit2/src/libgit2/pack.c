/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "pack.h"

#include "delta.h"
#include "futils.h"
#include "mwindow.h"
#include "odb.h"
#include "oid.h"
#include "oidarray.h"
#include "hashmap_oid.h"

/* Option to bypass checking existence of '.keep' files */
bool git_disable_pack_keep_file_checks = false;

static int packfile_open_locked(struct git_pack_file *p);
static off64_t nth_packed_object_offset_locked(struct git_pack_file *p, uint32_t n);
static int packfile_unpack_compressed(
		git_rawobj *obj,
		struct git_pack_file *p,
		git_mwindow **w_curs,
		off64_t *curpos,
		size_t size,
		git_object_t type);

/* Can find the offset of an object given
 * a prefix of an identifier.
 * Throws GIT_EAMBIGUOUSOIDPREFIX if short oid
 * is ambiguous within the pack.
 * This method assumes that len is between
 * GIT_OID_MINPREFIXLEN and the oid type's hexsize.
 */
static int pack_entry_find_offset(
		off64_t *offset_out,
		git_oid *found_oid,
		struct git_pack_file *p,
		const git_oid *short_oid,
		size_t len);

#define off64_hash(key) (uint32_t)((key)>>33^(key)^(key)<<11)
#define off64_equal(a, b) ((a) == (b))

GIT_HASHMAP_FUNCTIONS(git_pack_offsetmap, GIT_HASHMAP_INLINE, off64_t, git_pack_cache_entry *, off64_hash, off64_equal);
GIT_HASHMAP_OID_FUNCTIONS(git_pack_oidmap, , struct git_pack_entry *);

static int packfile_error(const char *message)
{
	git_error_set(GIT_ERROR_ODB, "invalid pack file - %s", message);
	return -1;
}

/********************
 * Delta base cache
 ********************/

static git_pack_cache_entry *new_cache_object(git_rawobj *source)
{
	git_pack_cache_entry *e = git__calloc(1, sizeof(git_pack_cache_entry));
	if (!e)
		return NULL;

	git_atomic32_inc(&e->refcount);
	memcpy(&e->raw, source, sizeof(git_rawobj));

	return e;
}

static void free_cache_object(void *o)
{
	git_pack_cache_entry *e = (git_pack_cache_entry *)o;

	if (e != NULL) {
		git__free(e->raw.data);
		git__free(e);
	}
}

static void cache_free(git_pack_cache *cache)
{
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;
	git_pack_cache_entry *entry;

	while (git_pack_offsetmap_iterate(&iter, NULL, &entry, &cache->entries) == 0)
		free_cache_object(entry);

	git_pack_offsetmap_dispose(&cache->entries);
}

static int cache_init(git_pack_cache *cache)
{
	cache->memory_limit = GIT_PACK_CACHE_MEMORY_LIMIT;

	if (git_mutex_init(&cache->lock)) {
		git_error_set(GIT_ERROR_OS, "failed to initialize pack cache mutex");
		return -1;
	}

	return 0;
}

static git_pack_cache_entry *cache_get(git_pack_cache *cache, off64_t offset)
{
	git_pack_cache_entry *entry = NULL;

	if (git_mutex_lock(&cache->lock) < 0)
		return NULL;

	if (git_pack_offsetmap_get(&entry, &cache->entries, offset) == 0) {
		git_atomic32_inc(&entry->refcount);
		entry->last_usage = cache->use_ctr++;
	}

	git_mutex_unlock(&cache->lock);

	return entry;
}

/* Run with the cache lock held */
static void free_lowest_entry(git_pack_cache *cache)
{
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;
	git_pack_cache_entry *entry;
	off64_t offset;

	while (git_pack_offsetmap_iterate(&iter, &offset, &entry, &cache->entries) == 0) {
		if (entry && git_atomic32_get(&entry->refcount) == 0) {
			cache->memory_used -= entry->raw.len;
			git_pack_offsetmap_remove(&cache->entries, offset);
			free_cache_object(entry);
		}
	}
}

static int cache_add(
		git_pack_cache_entry **cached_out,
		git_pack_cache *cache,
		git_rawobj *base,
		off64_t offset)
{
	git_pack_cache_entry *entry;
	int exists;

	if (base->len > GIT_PACK_CACHE_SIZE_LIMIT)
		return -1;

	entry = new_cache_object(base);
	if (entry) {
		if (git_mutex_lock(&cache->lock) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to lock cache");
			git__free(entry);
			return -1;
		}
		/* Add it to the cache if nobody else has */
		exists = git_pack_offsetmap_contains(&cache->entries, offset);
		if (!exists) {
			while (cache->memory_used + base->len > cache->memory_limit)
				free_lowest_entry(cache);

			git_pack_offsetmap_put(&cache->entries, offset, entry);
			cache->memory_used += entry->raw.len;

			*cached_out = entry;
		}
		git_mutex_unlock(&cache->lock);
		/* Somebody beat us to adding it into the cache */
		if (exists) {
			git__free(entry);
			return -1;
		}
	}

	return 0;
}

/***********************************************************
 *
 * PACK INDEX METHODS
 *
 ***********************************************************/

static void pack_index_free(struct git_pack_file *p)
{
	if (p->ids) {
		git__free(p->ids);
		p->ids = NULL;
	}
	if (p->index_map.data) {
		git_futils_mmap_free(&p->index_map);
		p->index_map.data = NULL;
	}
}

/* Run with the packfile lock held */
static int pack_index_check_locked(const char *path, struct git_pack_file *p)
{
	struct git_pack_idx_header *hdr;
	uint32_t version, nr = 0, i, *index;
	void *idx_map;
	size_t idx_size;
	struct stat st;
	int error;

	/* TODO: properly open the file without access time using O_NOATIME */
	git_file fd = git_futils_open_ro(path);
	if (fd < 0)
		return fd;

	if (p_fstat(fd, &st) < 0) {
		p_close(fd);
		git_error_set(GIT_ERROR_OS, "unable to stat pack index '%s'", path);
		return -1;
	}

	if (!S_ISREG(st.st_mode) ||
		!git__is_sizet(st.st_size) ||
		(idx_size = (size_t)st.st_size) < (size_t)((4 * 256) + (p->oid_size * 2))) {
		p_close(fd);
		git_error_set(GIT_ERROR_ODB, "invalid pack index '%s'", path);
		return -1;
	}

	error = git_futils_mmap_ro(&p->index_map, fd, 0, idx_size);

	p_close(fd);

	if (error < 0)
		return error;

	hdr = idx_map = p->index_map.data;

	if (hdr->idx_signature == htonl(PACK_IDX_SIGNATURE)) {
		version = ntohl(hdr->idx_version);

		if (version < 2 || version > 2) {
			git_futils_mmap_free(&p->index_map);
			return packfile_error("unsupported index version");
		}

	} else {
		version = 1;
	}

	index = idx_map;

	if (version > 1)
		index += 2; /* skip index header */

	for (i = 0; i < 256; i++) {
		uint32_t n = ntohl(index[i]);
		if (n < nr) {
			git_futils_mmap_free(&p->index_map);
			return packfile_error("index is non-monotonic");
		}
		nr = n;
	}

	if (version == 1) {
		/*
		 * Total size:
		 * - 256 index entries 4 bytes each
		 * - 24/36-byte entries * nr (20/32 byte SHA + 4-byte offset)
		 * - 20/32-byte SHA of the packfile
		 * - 20/32-byte SHA file checksum
		 */
		if (idx_size != (4 * 256 + ((uint64_t) nr * (p->oid_size + 4)) + (p->oid_size * 2))) {
			git_futils_mmap_free(&p->index_map);
			return packfile_error("index is corrupted");
		}
	} else if (version == 2) {
		/*
		 * Minimum size:
		 * - 8 bytes of header
		 * - 256 index entries 4 bytes each
		 * - 20/32-byte SHA entry * nr
		 * - 4-byte crc entry * nr
		 * - 4-byte offset entry * nr
		 * - 20/32-byte SHA of the packfile
		 * - 20/32-byte SHA file checksum
		 * And after the 4-byte offset table might be a
		 * variable sized table containing 8-byte entries
		 * for offsets larger than 2^31.
		 */
		uint64_t min_size = 8 + (4 * 256) + ((uint64_t)nr * (p->oid_size + 4 + 4)) + (p->oid_size * 2);
		uint64_t max_size = min_size;

		if (nr)
			max_size += (nr - 1)*8;

		if (idx_size < min_size || idx_size > max_size) {
			git_futils_mmap_free(&p->index_map);
			return packfile_error("wrong index size");
		}
	}

	p->num_objects = nr;
	p->index_version = version;
	return 0;
}

/* Run with the packfile lock held */
static int pack_index_open_locked(struct git_pack_file *p)
{
	int error = 0;
	size_t name_len;
	git_str idx_name = GIT_STR_INIT;

	if (p->index_version > -1)
		goto cleanup;

	/* checked by git_pack_file alloc */
	name_len = strlen(p->pack_name);
	GIT_ASSERT(name_len > strlen(".pack"));

	if ((error = git_str_init(&idx_name, name_len)) < 0)
		goto cleanup;

	git_str_put(&idx_name, p->pack_name, name_len - strlen(".pack"));
	git_str_puts(&idx_name, ".idx");
	if (git_str_oom(&idx_name)) {
		error = -1;
		goto cleanup;
	}

	if (p->index_version == -1)
		error = pack_index_check_locked(idx_name.ptr, p);

cleanup:
	git_str_dispose(&idx_name);

	return error;
}

static unsigned char *pack_window_open(
		struct git_pack_file *p,
		git_mwindow **w_cursor,
		off64_t offset,
		unsigned int *left)
{
	unsigned char *pack_data = NULL;

	if (git_mutex_lock(&p->lock) < 0) {
		git_error_set(GIT_ERROR_THREAD, "unable to lock packfile");
		return NULL;
	}
	if (git_mutex_lock(&p->mwf.lock) < 0) {
		git_mutex_unlock(&p->lock);
		git_error_set(GIT_ERROR_THREAD, "unable to lock packfile");
		return NULL;
	}

	if (p->mwf.fd == -1 && packfile_open_locked(p) < 0)
		goto cleanup;

	/* Since packfiles end in a hash of their content and it's
	 * pointless to ask for an offset into the middle of that
	 * hash, and the pack_window_contains function above wouldn't match
	 * don't allow an offset too close to the end of the file.
	 *
	 * Don't allow a negative offset, as that means we've wrapped
	 * around.
	 */
	if (offset > (p->mwf.size - p->oid_size))
		goto cleanup;
	if (offset < 0)
		goto cleanup;

	pack_data = git_mwindow_open(&p->mwf, w_cursor, offset, p->oid_size, left);

cleanup:
	git_mutex_unlock(&p->mwf.lock);
	git_mutex_unlock(&p->lock);
	return pack_data;
 }

/*
 * The per-object header is a pretty dense thing, which is
 *  - first byte: low four bits are "size",
 *    then three bits of "type",
 *    with the high bit being "size continues".
 *  - each byte afterwards: low seven bits are size continuation,
 *    with the high bit being "size continues"
 */
int git_packfile__object_header(size_t *out, unsigned char *hdr, size_t size, git_object_t type)
{
	unsigned char *hdr_base;
	unsigned char c;

	GIT_ASSERT_ARG(type >= GIT_OBJECT_COMMIT && type <= GIT_OBJECT_REF_DELTA);

	/* TODO: add support for chunked objects; see git.git 6c0d19b1 */

	c = (unsigned char)((type << 4) | (size & 15));
	size >>= 4;
	hdr_base = hdr;

	while (size) {
		*hdr++ = c | 0x80;
		c = size & 0x7f;
		size >>= 7;
	}
	*hdr++ = c;

	*out = (hdr - hdr_base);
	return 0;
}


static int packfile_unpack_header1(
		unsigned long *usedp,
		size_t *sizep,
		git_object_t *type,
		const unsigned char *buf,
		unsigned long len)
{
	unsigned shift;
	unsigned long size, c;
	unsigned long used = 0;

	c = buf[used++];
	*type = (c >> 4) & 7;
	size = c & 15;
	shift = 4;
	while (c & 0x80) {
		if (len <= used) {
			git_error_set(GIT_ERROR_ODB, "buffer too small");
			return GIT_EBUFS;
		}

		if (bitsizeof(long) <= shift) {
			*usedp = 0;
			git_error_set(GIT_ERROR_ODB, "packfile corrupted");
			return -1;
		}

		c = buf[used++];
		size += (c & 0x7f) << shift;
		shift += 7;
	}

	*sizep = (size_t)size;
	*usedp = used;
	return 0;
}

int git_packfile_unpack_header(
		size_t *size_p,
		git_object_t *type_p,
		struct git_pack_file *p,
		git_mwindow **w_curs,
		off64_t *curpos)
{
	unsigned char *base;
	unsigned int left;
	unsigned long used;
	int error;

	if ((error = git_mutex_lock(&p->lock)) < 0)
		return error;
	if ((error = git_mutex_lock(&p->mwf.lock)) < 0) {
		git_mutex_unlock(&p->lock);
		return error;
	}

	if (p->mwf.fd == -1 && (error = packfile_open_locked(p)) < 0) {
		git_mutex_unlock(&p->lock);
		git_mutex_unlock(&p->mwf.lock);
		return error;
	}

	/* pack_window_open() assures us we have [base, base + oid_size)
	 * available as a range that we can look at at. (It's actually
	 * the hash size that is assured.) With our object header
	 * encoding the maximum deflated object size is 2^137, which is
	 * just insane, so we know won't exceed what we have been given.
	 */
	base = git_mwindow_open(&p->mwf, w_curs, *curpos, p->oid_size, &left);
	git_mutex_unlock(&p->lock);
	git_mutex_unlock(&p->mwf.lock);
	if (base == NULL)
		return GIT_EBUFS;

	error = packfile_unpack_header1(&used, size_p, type_p, base, left);
	git_mwindow_close(w_curs);
	if (error == GIT_EBUFS)
		return error;
	else if (error < 0)
		return packfile_error("header length is zero");

	*curpos += used;
	return 0;
}

int git_packfile_resolve_header(
		size_t *size_p,
		git_object_t *type_p,
		struct git_pack_file *p,
		off64_t offset)
{
	git_mwindow *w_curs = NULL;
	off64_t curpos = offset;
	size_t size;
	git_object_t type;
	off64_t base_offset;
	int error;

	error = git_mutex_lock(&p->lock);
	if (error < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock packfile reader");
		return error;
	}
	error = git_mutex_lock(&p->mwf.lock);
	if (error < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock packfile reader");
		git_mutex_unlock(&p->lock);
		return error;
	}

	if (p->mwf.fd == -1 && (error = packfile_open_locked(p)) < 0) {
		git_mutex_unlock(&p->mwf.lock);
		git_mutex_unlock(&p->lock);
		return error;
	}
	git_mutex_unlock(&p->mwf.lock);
	git_mutex_unlock(&p->lock);

	error = git_packfile_unpack_header(&size, &type, p, &w_curs, &curpos);
	if (error < 0)
		return error;

	if (type == GIT_OBJECT_OFS_DELTA || type == GIT_OBJECT_REF_DELTA) {
		size_t base_size;
		git_packfile_stream stream;

		error = get_delta_base(&base_offset, p, &w_curs, &curpos, type, offset);
		git_mwindow_close(&w_curs);

		if (error < 0)
			return error;

		if ((error = git_packfile_stream_open(&stream, p, curpos)) < 0)
			return error;
		error = git_delta_read_header_fromstream(&base_size, size_p, &stream);
		git_packfile_stream_dispose(&stream);
		if (error < 0)
			return error;
	} else {
		*size_p = size;
		base_offset = 0;
	}

	while (type == GIT_OBJECT_OFS_DELTA || type == GIT_OBJECT_REF_DELTA) {
		curpos = base_offset;
		error = git_packfile_unpack_header(&size, &type, p, &w_curs, &curpos);
		if (error < 0)
			return error;
		if (type != GIT_OBJECT_OFS_DELTA && type != GIT_OBJECT_REF_DELTA)
			break;

		error = get_delta_base(&base_offset, p, &w_curs, &curpos, type, base_offset);
		git_mwindow_close(&w_curs);

		if (error < 0)
			return error;
	}
	*type_p = type;

	return error;
}

#define SMALL_STACK_SIZE 64

/**
 * Generate the chain of dependencies which we need to get to the
 * object at `off`. `chain` is used a stack, popping gives the right
 * order to apply deltas on. If an object is found in the pack's base
 * cache, we stop calculating there.
 */
static int pack_dependency_chain(git_dependency_chain *chain_out,
				 git_pack_cache_entry **cached_out, off64_t *cached_off,
				 struct pack_chain_elem *small_stack, size_t *stack_sz,
				 struct git_pack_file *p, off64_t obj_offset)
{
	git_dependency_chain chain = GIT_ARRAY_INIT;
	git_mwindow *w_curs = NULL;
	off64_t curpos = obj_offset, base_offset;
	int error = 0, use_heap = 0;
	size_t size, elem_pos;
	git_object_t type;

	elem_pos = 0;
	while (true) {
		struct pack_chain_elem *elem;
		git_pack_cache_entry *cached = NULL;

		/* if we have a base cached, we can stop here instead */
		if ((cached = cache_get(&p->bases, obj_offset)) != NULL) {
			*cached_out = cached;
			*cached_off = obj_offset;
			break;
		}

		/* if we run out of space on the small stack, use the array */
		if (elem_pos == SMALL_STACK_SIZE) {
			git_array_init_to_size(chain, elem_pos);
			GIT_ERROR_CHECK_ARRAY(chain);
			memcpy(chain.ptr, small_stack, elem_pos * sizeof(struct pack_chain_elem));
			chain.size = elem_pos;
			use_heap = 1;
		}

		curpos = obj_offset;
		if (!use_heap) {
			elem = &small_stack[elem_pos];
		} else {
			elem = git_array_alloc(chain);
			if (!elem) {
				error = -1;
				goto on_error;
			}
		}

		elem->base_key = obj_offset;

		error = git_packfile_unpack_header(&size, &type, p, &w_curs, &curpos);
		if (error < 0)
			goto on_error;

		elem->offset = curpos;
		elem->size = size;
		elem->type = type;
		elem->base_key = obj_offset;

		if (type != GIT_OBJECT_OFS_DELTA && type != GIT_OBJECT_REF_DELTA)
			break;

		error = get_delta_base(&base_offset, p, &w_curs, &curpos, type, obj_offset);
		git_mwindow_close(&w_curs);

		if (error < 0)
			goto on_error;

		/* we need to pass the pos *after* the delta-base bit */
		elem->offset = curpos;

		/* go through the loop again, but with the new object */
		obj_offset = base_offset;
		elem_pos++;
	}


	*stack_sz = elem_pos + 1;
	*chain_out = chain;
	return error;

on_error:
	git_array_clear(chain);
	return error;
}

int git_packfile_unpack(
	git_rawobj *obj,
	struct git_pack_file *p,
	off64_t *obj_offset)
{
	git_mwindow *w_curs = NULL;
	off64_t curpos = *obj_offset;
	int error, free_base = 0;
	git_dependency_chain chain = GIT_ARRAY_INIT;
	struct pack_chain_elem *elem = NULL, *stack;
	git_pack_cache_entry *cached = NULL;
	struct pack_chain_elem small_stack[SMALL_STACK_SIZE];
	size_t stack_size = 0, elem_pos, alloclen;
	git_object_t base_type;

	error = git_mutex_lock(&p->lock);
	if (error < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock packfile reader");
		return error;
	}
	error = git_mutex_lock(&p->mwf.lock);
	if (error < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock packfile reader");
		git_mutex_unlock(&p->lock);
		return error;
	}

	if (p->mwf.fd == -1)
		error = packfile_open_locked(p);
	git_mutex_unlock(&p->mwf.lock);
	git_mutex_unlock(&p->lock);
	if (error < 0)
		return error;

	/*
	 * TODO: optionally check the CRC on the packfile
	 */

	error = pack_dependency_chain(&chain, &cached, obj_offset, small_stack, &stack_size, p, *obj_offset);
	if (error < 0)
		return error;

	obj->data = NULL;
	obj->len = 0;
	obj->type = GIT_OBJECT_INVALID;

	/* let's point to the right stack */
	stack = chain.ptr ? chain.ptr : small_stack;

	elem_pos = stack_size;
	if (cached) {
		memcpy(obj, &cached->raw, sizeof(git_rawobj));
		base_type = obj->type;
		elem_pos--;	/* stack_size includes the base, which isn't actually there */
	} else {
		elem = &stack[--elem_pos];
		base_type = elem->type;
	}

	switch (base_type) {
	case GIT_OBJECT_COMMIT:
	case GIT_OBJECT_TREE:
	case GIT_OBJECT_BLOB:
	case GIT_OBJECT_TAG:
		if (!cached) {
			curpos = elem->offset;
			error = packfile_unpack_compressed(obj, p, &w_curs, &curpos, elem->size, elem->type);
			git_mwindow_close(&w_curs);
			base_type = elem->type;
		}
		if (error < 0)
			goto cleanup;
		break;
	case GIT_OBJECT_OFS_DELTA:
	case GIT_OBJECT_REF_DELTA:
		error = packfile_error("dependency chain ends in a delta");
		goto cleanup;
	default:
		error = packfile_error("invalid packfile type in header");
		goto cleanup;
	}

	/*
	 * Finding the object we want a cached base element is
	 * problematic, as we need to make sure we don't accidentally
	 * give the caller the cached object, which it would then feel
	 * free to free, so we need to copy the data.
	 */
	if (cached && stack_size == 1) {
		void *data = obj->data;

		GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, obj->len, 1);
		obj->data = git__malloc(alloclen);
		GIT_ERROR_CHECK_ALLOC(obj->data);

		memcpy(obj->data, data, obj->len + 1);
		git_atomic32_dec(&cached->refcount);
		goto cleanup;
	}

	/* we now apply each consecutive delta until we run out */
	while (elem_pos > 0 && !error) {
		git_rawobj base, delta;

		/*
		 * We can now try to add the base to the cache, as
		 * long as it's not already the cached one.
		 */
		if (!cached)
			free_base = !!cache_add(&cached, &p->bases, obj, elem->base_key);

		elem = &stack[elem_pos - 1];
		curpos = elem->offset;
		error = packfile_unpack_compressed(&delta, p, &w_curs, &curpos, elem->size, elem->type);
		git_mwindow_close(&w_curs);

		if (error < 0) {
			/* We have transferred ownership of the data to the cache. */
			obj->data = NULL;
			break;
		}

		/* the current object becomes the new base, on which we apply the delta */
		base = *obj;
		obj->data = NULL;
		obj->len = 0;
		obj->type = GIT_OBJECT_INVALID;

		error = git_delta_apply(&obj->data, &obj->len, base.data, base.len, delta.data, delta.len);
		obj->type = base_type;

		/*
		 * We usually don't want to free the base at this
		 * point, as we put it into the cache in the previous
		 * iteration. free_base lets us know that we got the
		 * base object directly from the packfile, so we can free it.
		 */
		git__free(delta.data);
		if (free_base) {
			free_base = 0;
			git__free(base.data);
		}

		if (cached) {
			git_atomic32_dec(&cached->refcount);
			cached = NULL;
		}

		if (error < 0)
			break;

		elem_pos--;
	}

cleanup:
	if (error < 0) {
		git__free(obj->data);
		if (cached)
			git_atomic32_dec(&cached->refcount);
	}

	if (elem)
		*obj_offset = curpos;

	git_array_clear(chain);
	return error;
}

int git_packfile_stream_open(git_packfile_stream *obj, struct git_pack_file *p, off64_t curpos)
{
	memset(obj, 0, sizeof(git_packfile_stream));
	obj->curpos = curpos;
	obj->p = p;

	if (git_zstream_init(&obj->zstream, GIT_ZSTREAM_INFLATE) < 0) {
		git_error_set(GIT_ERROR_ZLIB, "failed to init packfile stream");
		return -1;
	}

	return 0;
}

ssize_t git_packfile_stream_read(git_packfile_stream *obj, void *buffer, size_t len)
{
	unsigned int window_len;
	unsigned char *in;
	int error;

	if (obj->done)
		return 0;

	if ((in = pack_window_open(obj->p, &obj->mw, obj->curpos, &window_len)) == NULL)
		return GIT_EBUFS;

	if ((error = git_zstream_set_input(&obj->zstream, in, window_len)) < 0 ||
	    (error = git_zstream_get_output_chunk(buffer, &len, &obj->zstream)) < 0) {
		git_mwindow_close(&obj->mw);
		git_error_set(GIT_ERROR_ZLIB, "error reading from the zlib stream");
		return -1;
	}

	git_mwindow_close(&obj->mw);

	obj->curpos += window_len - obj->zstream.in_len;

	if (git_zstream_eos(&obj->zstream))
		obj->done = 1;

	/* If we didn't write anything out but we're not done, we need more data */
	if (!len && !git_zstream_eos(&obj->zstream))
		return GIT_EBUFS;

	return len;

}

void git_packfile_stream_dispose(git_packfile_stream *obj)
{
	git_zstream_free(&obj->zstream);
}

static int packfile_unpack_compressed(
	git_rawobj *obj,
	struct git_pack_file *p,
	git_mwindow **mwindow,
	off64_t *position,
	size_t size,
	git_object_t type)
{
	git_zstream zstream = GIT_ZSTREAM_INIT;
	size_t buffer_len, total = 0;
	char *data = NULL;
	int error;

	GIT_ERROR_CHECK_ALLOC_ADD(&buffer_len, size, 1);
	data = git__calloc(1, buffer_len);
	GIT_ERROR_CHECK_ALLOC(data);

	if ((error = git_zstream_init(&zstream, GIT_ZSTREAM_INFLATE)) < 0) {
		git_error_set(GIT_ERROR_ZLIB, "failed to init zlib stream on unpack");
		goto out;
	}

	do {
		size_t bytes = buffer_len - total;
		unsigned int window_len, consumed;
		unsigned char *in;

		if ((in = pack_window_open(p, mwindow, *position, &window_len)) == NULL) {
			error = -1;
			goto out;
		}

		if ((error = git_zstream_set_input(&zstream, in, window_len)) < 0 ||
		    (error = git_zstream_get_output_chunk(data + total, &bytes, &zstream)) < 0) {
			git_mwindow_close(mwindow);
			goto out;
		}

		git_mwindow_close(mwindow);

		consumed = window_len - (unsigned int)zstream.in_len;

		if (!bytes && !consumed) {
			git_error_set(GIT_ERROR_ZLIB, "error inflating zlib stream");
			error = -1;
			goto out;
		}

		*position += consumed;
		total += bytes;
	} while (!git_zstream_eos(&zstream));

	if (total != size || !git_zstream_eos(&zstream)) {
		git_error_set(GIT_ERROR_ZLIB, "error inflating zlib stream");
		error = -1;
		goto out;
	}

	obj->type = type;
	obj->len = size;
	obj->data = data;

out:
	git_zstream_free(&zstream);
	if (error)
		git__free(data);

	return error;
}

/*
 * curpos is where the data starts, delta_obj_offset is the where the
 * header starts
 */
int get_delta_base(
		off64_t *delta_base_out,
		struct git_pack_file *p,
		git_mwindow **w_curs,
		off64_t *curpos,
		git_object_t type,
		off64_t delta_obj_offset)
{
	unsigned int left = 0;
	unsigned char *base_info;
	off64_t base_offset;
	git_oid unused;

	GIT_ASSERT_ARG(delta_base_out);

	base_info = pack_window_open(p, w_curs, *curpos, &left);
	/* Assumption: the only reason this would fail is because the file is too small */
	if (base_info == NULL)
		return GIT_EBUFS;
	/* pack_window_open() assured us we have
	 * [base_info, base_info + oid_size) as a range that we can look
	 * at without walking off the end of the mapped window. Its
	 * actually the hash size that is assured. An OFS_DELTA longer
	 * than the hash size is stupid, as then a REF_DELTA would be
	 * smaller to store.
	 */
	if (type == GIT_OBJECT_OFS_DELTA) {
		unsigned used = 0;
		unsigned char c = base_info[used++];
		size_t unsigned_base_offset = c & 127;
		while (c & 128) {
			if (left <= used)
				return GIT_EBUFS;
			unsigned_base_offset += 1;
			if (!unsigned_base_offset || MSB(unsigned_base_offset, 7))
				return packfile_error("overflow");
			c = base_info[used++];
			unsigned_base_offset = (unsigned_base_offset << 7) + (c & 127);
		}
		if (unsigned_base_offset == 0 || (size_t)delta_obj_offset <= unsigned_base_offset)
			return packfile_error("out of bounds");
		base_offset = delta_obj_offset - unsigned_base_offset;
		*curpos += used;
	} else if (type == GIT_OBJECT_REF_DELTA) {
		git_oid base_oid;
		git_oid__fromraw(&base_oid, base_info, p->oid_type);

		/* If we have the cooperative cache, search in it first */
		if (p->has_cache) {
			struct git_pack_entry *entry;

			if (git_pack_oidmap_get(&entry, &p->idx_cache, &base_oid) == 0) {
				if (entry->offset == 0)
					return packfile_error("delta offset is zero");

				*curpos += p->oid_size;
				*delta_base_out = entry->offset;
				return 0;
			} else {
				/* If we're building an index, don't try to find the pack
				 * entry; we just haven't seen it yet.  We'll make
				 * progress again in the next loop.
				 */
				return GIT_PASSTHROUGH;
			}
		}

		/* The base entry _must_ be in the same pack */
		if (pack_entry_find_offset(&base_offset, &unused, p, &base_oid, p->oid_hexsize) < 0)
			return packfile_error("base entry delta is not in the same pack");
		*curpos += p->oid_size;
	} else
		return packfile_error("unknown object type");

	if (base_offset == 0)
		return packfile_error("delta offset is zero");

	*delta_base_out = base_offset;
	return 0;
}

/***********************************************************
 *
 * PACKFILE METHODS
 *
 ***********************************************************/

void git_packfile_free(struct git_pack_file *p, bool unlink_packfile)
{
	bool locked = true;

	if (!p)
		return;

	cache_free(&p->bases);

	if (git_mutex_lock(&p->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock packfile");
		locked = false;
	}
	if (p->mwf.fd >= 0) {
		git_mwindow_free_all(&p->mwf);
		p_close(p->mwf.fd);
		p->mwf.fd = -1;
	}
	if (locked)
		git_mutex_unlock(&p->lock);

	if (unlink_packfile)
		p_unlink(p->pack_name);

	pack_index_free(p);

	git__free(p->bad_object_ids);

	git_mutex_free(&p->bases.lock);
	git_mutex_free(&p->mwf.lock);
	git_mutex_free(&p->lock);
	git__free(p);
}

/* Run with the packfile and mwf locks held */
static int packfile_open_locked(struct git_pack_file *p)
{
	struct stat st;
	struct git_pack_header hdr;
	unsigned char checksum[GIT_OID_MAX_SIZE];
	unsigned char *idx_checksum;

	if (pack_index_open_locked(p) < 0)
		return git_odb__error_notfound("failed to open packfile", NULL, 0);

	if (p->mwf.fd >= 0)
		return 0;

	/* TODO: open with noatime */
	p->mwf.fd = git_futils_open_ro(p->pack_name);
	if (p->mwf.fd < 0)
		goto cleanup;

	if (p_fstat(p->mwf.fd, &st) < 0) {
		git_error_set(GIT_ERROR_OS, "could not stat packfile");
		goto cleanup;
	}

	/* If we created the struct before we had the pack we lack size. */
	if (!p->mwf.size) {
		if (!S_ISREG(st.st_mode))
			goto cleanup;
		p->mwf.size = (off64_t)st.st_size;
	} else if (p->mwf.size != st.st_size)
		goto cleanup;

#if 0
	/* We leave these file descriptors open with sliding mmap;
	 * there is no point keeping them open across exec(), though.
	 */
	fd_flag = fcntl(p->mwf.fd, F_GETFD, 0);
	if (fd_flag < 0)
		goto cleanup;

	fd_flag |= FD_CLOEXEC;
	if (fcntl(p->pack_fd, F_SETFD, fd_flag) == -1)
		goto cleanup;
#endif

	/* Verify we recognize this pack file format. */
	if (p_read(p->mwf.fd, &hdr, sizeof(hdr)) < 0 ||
		hdr.hdr_signature != htonl(PACK_SIGNATURE) ||
		!pack_version_ok(hdr.hdr_version))
		goto cleanup;

	/* Verify the pack matches its index. */
	if (p->num_objects != ntohl(hdr.hdr_entries) ||
	    p_pread(p->mwf.fd, checksum, p->oid_size, p->mwf.size - p->oid_size) < 0)
		goto cleanup;

	idx_checksum = ((unsigned char *)p->index_map.data) +
	               p->index_map.len - (p->oid_size * 2);

	if (git_oid_raw_cmp(checksum, idx_checksum, p->oid_size) != 0)
		goto cleanup;

	if (git_mwindow_file_register(&p->mwf) < 0)
		goto cleanup;

	return 0;

cleanup:
	git_error_set(GIT_ERROR_OS, "invalid packfile '%s'", p->pack_name);

	if (p->mwf.fd >= 0)
		p_close(p->mwf.fd);
	p->mwf.fd = -1;

	return -1;
}

int git_packfile__name(char **out, const char *path)
{
	size_t path_len;
	git_str buf = GIT_STR_INIT;

	path_len = strlen(path);

	if (path_len < strlen(".idx"))
		return git_odb__error_notfound("invalid packfile path", NULL, 0);

	if (git_str_printf(&buf, "%.*s.pack", (int)(path_len - strlen(".idx")), path) < 0)
		return -1;

	*out = git_str_detach(&buf);
	return 0;
}

int git_packfile_alloc(
	struct git_pack_file **pack_out,
	const char *path,
	git_oid_t oid_type)
{
	struct stat st;
	struct git_pack_file *p;
	size_t path_len = path ? strlen(path) : 0, alloc_len;

	*pack_out = NULL;

	if (path_len < strlen(".idx"))
		return git_odb__error_notfound("invalid packfile path", NULL, 0);

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, sizeof(*p), path_len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 2);

	p = git__calloc(1, alloc_len);
	GIT_ERROR_CHECK_ALLOC(p);

	memcpy(p->pack_name, path, path_len + 1);

	/*
	 * Make sure a corresponding .pack file exists and that
	 * the index looks sane.
	 */
	if (git__suffixcmp(path, ".idx") == 0) {
		size_t root_len = path_len - strlen(".idx");

		if (!git_disable_pack_keep_file_checks) {
			memcpy(p->pack_name + root_len, ".keep", sizeof(".keep"));
			if (git_fs_path_exists(p->pack_name) == true)
				p->pack_keep = 1;
		}

		memcpy(p->pack_name + root_len, ".pack", sizeof(".pack"));
	}

	if (p_stat(p->pack_name, &st) < 0 || !S_ISREG(st.st_mode)) {
		git__free(p);
		return git_odb__error_notfound("packfile not found", NULL, 0);
	}

	/* ok, it looks sane as far as we can check without
	 * actually mapping the pack file.
	 */
	p->mwf.fd = -1;
	p->mwf.size = st.st_size;
	p->pack_local = 1;
	p->mtime = (git_time_t)st.st_mtime;
	p->index_version = -1;
	p->oid_type = oid_type ? oid_type : GIT_OID_DEFAULT;
	p->oid_size = (unsigned int)git_oid_size(p->oid_type);
	p->oid_hexsize = (unsigned int)git_oid_hexsize(p->oid_type);

	if (git_mutex_init(&p->lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to initialize packfile mutex");
		git__free(p);
		return -1;
	}

	if (git_mutex_init(&p->mwf.lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to initialize packfile window mutex");
		git_mutex_free(&p->lock);
		git__free(p);
		return -1;
	}

	if (cache_init(&p->bases) < 0) {
		git_mutex_free(&p->mwf.lock);
		git_mutex_free(&p->lock);
		git__free(p);
		return -1;
	}

	*pack_out = p;

	return 0;
}

/***********************************************************
 *
 * PACKFILE ENTRY SEARCH INTERNALS
 *
 ***********************************************************/

static off64_t nth_packed_object_offset_locked(struct git_pack_file *p, uint32_t n)
{
	const unsigned char *index, *end;
	uint32_t off32;

	index = p->index_map.data;
	end = index + p->index_map.len;
	index += 4 * 256;
	if (p->index_version == 1)
		return ntohl(*((uint32_t *)(index + (p->oid_size + 4) * (size_t) n)));

	index += 8 + (size_t) p->num_objects * (p->oid_size + 4);
	off32 = ntohl(*((uint32_t *)(index + 4 * n)));
	if (!(off32 & 0x80000000))
		return off32;
	index += (size_t) p->num_objects * 4 + (off32 & 0x7fffffff) * 8;

	/* Make sure we're not being sent out of bounds */
	if (index >= end - 8)
		return -1;

	return (((uint64_t)ntohl(*((uint32_t *)(index + 0)))) << 32) |
	       ntohl(*((uint32_t *)(index + 4)));
}

static int git__memcmp4(const void *a, const void *b) {
	return memcmp(a, b, 4);
}

int git_pack_foreach_entry(
	struct git_pack_file *p,
	git_odb_foreach_cb cb,
	void *data)
{
	const unsigned char *index, *current;
	uint32_t i;
	int error = 0;
	git_array_oid_t oids = GIT_ARRAY_INIT;
	git_oid *oid;

	if (git_mutex_lock(&p->lock) < 0)
		return packfile_error("failed to get lock for git_pack_foreach_entry");

	if ((error = pack_index_open_locked(p)) < 0) {
		git_mutex_unlock(&p->lock);
		return error;
	}

	if (!p->index_map.data) {
		git_error_set(GIT_ERROR_INTERNAL, "internal error: p->index_map.data == NULL");
		git_mutex_unlock(&p->lock);
		return -1;
	}

	index = p->index_map.data;

	if (p->index_version > 1)
		index += 8;

	index += 4 * 256;

	if (p->ids == NULL) {
		git_vector offsets, oids;

		if ((error = git_vector_init(&oids, p->num_objects, NULL))) {
			git_mutex_unlock(&p->lock);
			return error;
		}

		if ((error = git_vector_init(&offsets, p->num_objects, git__memcmp4))) {
			git_mutex_unlock(&p->lock);
			return error;
		}

		if (p->index_version > 1) {
			const unsigned char *off = index +
				(p->oid_size + 4) * p->num_objects;

			for (i = 0; i < p->num_objects; i++)
				git_vector_insert(&offsets, (void*)&off[4 * i]);

			git_vector_sort(&offsets);
			git_vector_foreach(&offsets, i, current)
				git_vector_insert(&oids, (void*)&index[5 * (current - off)]);
		} else {
			for (i = 0; i < p->num_objects; i++)
				git_vector_insert(&offsets, (void*)&index[(p->oid_size + 4) * i]);
			git_vector_sort(&offsets);
			git_vector_foreach(&offsets, i, current)
				git_vector_insert(&oids, (void*)&current[4]);
		}

		git_vector_dispose(&offsets);
		p->ids = (unsigned char **)git_vector_detach(NULL, NULL, &oids);
	}

	/*
	 * We need to copy the OIDs to another array before we
	 * relinquish the lock to avoid races.  We can also take
	 * this opportunity to put them into normal form.
	 */
	git_array_init_to_size(oids, p->num_objects);
	if (!oids.ptr) {
		git_mutex_unlock(&p->lock);
		git_array_clear(oids);
		GIT_ERROR_CHECK_ARRAY(oids);
	}
	for (i = 0; i < p->num_objects; i++) {
		oid = git_array_alloc(oids);
		if (!oid) {
			git_mutex_unlock(&p->lock);
			git_array_clear(oids);
			GIT_ERROR_CHECK_ALLOC(oid);
		}
		git_oid__fromraw(oid, p->ids[i], p->oid_type);
	}

	git_mutex_unlock(&p->lock);

	git_array_foreach(oids, i, oid) {
		if ((error = cb(oid, data)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	git_array_clear(oids);
	return error;
}

int git_pack_foreach_entry_offset(
	struct git_pack_file *p,
	git_pack_foreach_entry_offset_cb cb,
	void *data)
{
	const unsigned char *index;
	off64_t current_offset;
	git_oid current_oid;
	uint32_t i;
	int error = 0;

	if (git_mutex_lock(&p->lock) < 0)
		return packfile_error("failed to get lock for git_pack_foreach_entry_offset");

	index = p->index_map.data;
	if (index == NULL) {
		if ((error = pack_index_open_locked(p)) < 0)
			goto cleanup;

		if (!p->index_map.data) {
			git_error_set(GIT_ERROR_INTERNAL, "internal error: p->index_map.data == NULL");
			goto cleanup;
		}

		index = p->index_map.data;
	}

	if (p->index_version > 1)
		index += 8;

	index += 4 * 256;

	/* all offsets should have been validated by pack_index_check_locked */
	if (p->index_version > 1) {
		const unsigned char *offsets = index +
			(p->oid_size + 4) * p->num_objects;
		const unsigned char *large_offset_ptr;
		const unsigned char *large_offsets = index +
			(p->oid_size + 8) * p->num_objects;
		const unsigned char *large_offsets_end = ((const unsigned char *)p->index_map.data) + p->index_map.len - p->oid_size;

		for (i = 0; i < p->num_objects; i++) {
			current_offset = ntohl(*(const uint32_t *)(offsets + 4 * i));
			if (current_offset & 0x80000000) {
				large_offset_ptr = large_offsets + (current_offset & 0x7fffffff) * 8;
				if (large_offset_ptr >= large_offsets_end) {
					error = packfile_error("invalid large offset");
					goto cleanup;
				}
				current_offset = (((off64_t)ntohl(*((uint32_t *)(large_offset_ptr + 0)))) << 32) |
						ntohl(*((uint32_t *)(large_offset_ptr + 4)));
			}

			git_oid__fromraw(&current_oid, (index + p->oid_size * i), p->oid_type);
			if ((error = cb(&current_oid, current_offset, data)) != 0) {
				error = git_error_set_after_callback(error);
				goto cleanup;
			}
		}
	} else {
		for (i = 0; i < p->num_objects; i++) {
			current_offset = ntohl(*(const uint32_t *)(index + (p->oid_size + 4) * i));
			git_oid__fromraw(&current_oid, (index + (p->oid_size + 4) * i + 4), p->oid_type);
			if ((error = cb(&current_oid, current_offset, data)) != 0) {
				error = git_error_set_after_callback(error);
				goto cleanup;
			}
		}
	}

cleanup:
	git_mutex_unlock(&p->lock);
	return error;
}

int git_pack__lookup_id(
	const void *oid_lookup_table,
	size_t stride,
	unsigned lo,
	unsigned hi,
	const unsigned char *oid_prefix,
	const git_oid_t oid_type)
{
	const unsigned char *base = oid_lookup_table;
	size_t oid_size = git_oid_size(oid_type);

	while (lo < hi) {
		unsigned mi = (lo + hi) / 2;
		int cmp = git_oid_raw_cmp(base + mi * stride, oid_prefix, oid_size);

		if (!cmp)
			return mi;

		if (cmp > 0)
			hi = mi;
		else
			lo = mi+1;
	}

	return -((int)lo)-1;
}

static int pack_entry_find_offset(
	off64_t *offset_out,
	git_oid *found_oid,
	struct git_pack_file *p,
	const git_oid *short_oid,
	size_t len)
{
	const uint32_t *level1_ofs;
	size_t ofs_delta = 0;
	const unsigned char *index;
	unsigned hi, lo, stride;
	int pos, found = 0;
	off64_t offset;
	const unsigned char *current = 0;
	int error = 0;

	*offset_out = 0;

	if (git_mutex_lock(&p->lock) < 0)
		return packfile_error("failed to get lock for pack_entry_find_offset");

	if ((error = pack_index_open_locked(p)) < 0)
		goto cleanup;

	if (!p->index_map.data) {
		git_error_set(GIT_ERROR_INTERNAL, "internal error: p->index_map.data == NULL");
		goto cleanup;
	}

	index = p->index_map.data;
	level1_ofs = p->index_map.data;

	if (p->index_version > 1) {
		level1_ofs += 2;
		ofs_delta = 2;
		index += 8;
	}

	if ((size_t)short_oid->id[0] + ofs_delta >= p->index_map.len) {
		git_error_set(GIT_ERROR_INTERNAL, "internal error: p->short_oid->[0] out of bounds");
		goto cleanup;
	}

	index += 4 * 256;
	hi = ntohl(level1_ofs[(int)short_oid->id[0]]);
	lo = ((short_oid->id[0] == 0x0) ? 0 : ntohl(level1_ofs[(int)short_oid->id[0] - 1]));

	if (p->index_version > 1) {
		stride = p->oid_size;
	} else {
		stride = p->oid_size + 4;
		index += 4;
	}

#ifdef INDEX_DEBUG_LOOKUP
	printf("%02x%02x%02x... lo %u hi %u nr %d\n",
		short_oid->id[0], short_oid->id[1], short_oid->id[2], lo, hi, p->num_objects);
#endif

	pos = git_pack__lookup_id(index, stride, lo, hi,
		short_oid->id, p->oid_type);

	if (pos >= 0) {
		/* An object matching exactly the oid was found */
		found = 1;
		current = index + pos * stride;
	} else {
		/* No object was found */
		/* pos refers to the object with the "closest" oid to short_oid */
		pos = - 1 - pos;
		if (pos < (int)p->num_objects) {
			current = index + pos * stride;

			if (!git_oid_raw_ncmp(short_oid->id, current, len))
				found = 1;
		}
	}

	if (found &&
	    len != p->oid_hexsize &&
	    pos + 1 < (int)p->num_objects) {
		/* Check for ambiguousity */
		const unsigned char *next = current + stride;

		if (!git_oid_raw_ncmp(short_oid->id, next, len)) {
			found = 2;
		}
	}

	if (!found) {
		error = git_odb__error_notfound("failed to find offset for pack entry", short_oid, len);
		goto cleanup;
	}
	if (found > 1) {
		error = git_odb__error_ambiguous("found multiple offsets for pack entry");
		goto cleanup;
	}

	if ((offset = nth_packed_object_offset_locked(p, pos)) < 0) {
		git_error_set(GIT_ERROR_ODB, "packfile index is corrupt");
		error = -1;
		goto cleanup;
	}

	*offset_out = offset;
	git_oid__fromraw(found_oid, current, p->oid_type);

#ifdef INDEX_DEBUG_LOOKUP
	{
		char hex_sha1[p->oid_hexsize + 1];
		git_oid_fmt(hex_sha1, found_oid);
		hex_sha1[p->oid_hexsize] = '\0';
		printf("found lo=%d %s\n", lo, hex_sha1);
	}
#endif

cleanup:
	git_mutex_unlock(&p->lock);
	return error;
}

int git_pack_entry_find(
		struct git_pack_entry *e,
		struct git_pack_file *p,
		const git_oid *short_oid,
		size_t len)
{
	off64_t offset;
	git_oid found_oid;
	int error;

	GIT_ASSERT_ARG(p);

	if (len == p->oid_hexsize && p->num_bad_objects) {
		unsigned i;
		for (i = 0; i < p->num_bad_objects; i++)
			if (git_oid__cmp(short_oid, &p->bad_object_ids[i]) == 0)
				return packfile_error("bad object found in packfile");
	}

	error = pack_entry_find_offset(&offset, &found_oid, p, short_oid, len);
	if (error < 0)
		return error;

	error = git_mutex_lock(&p->lock);
	if (error < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock packfile reader");
		return error;
	}
	error = git_mutex_lock(&p->mwf.lock);
	if (error < 0) {
		git_mutex_unlock(&p->lock);
		git_error_set(GIT_ERROR_OS, "failed to lock packfile reader");
		return error;
	}

	/* we found a unique entry in the index;
	 * make sure the packfile backing the index
	 * still exists on disk */
	if (p->mwf.fd == -1)
		error = packfile_open_locked(p);
	git_mutex_unlock(&p->mwf.lock);
	git_mutex_unlock(&p->lock);
	if (error < 0)
		return error;

	e->offset = offset;
	e->p = p;

	git_oid_cpy(&e->id, &found_oid);
	return 0;
}
