/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_pack_h__
#define INCLUDE_pack_h__

#include "common.h"

#include "git2/oid.h"

#include "array.h"
#include "map.h"
#include "mwindow.h"
#include "odb.h"
#include "zstream.h"
#include "oid.h"
#include "hashmap_oid.h"

/**
 * Function type for callbacks from git_pack_foreach_entry_offset.
 */
typedef int git_pack_foreach_entry_offset_cb(
		const git_oid *id,
		off64_t offset,
		void *payload);

#define GIT_PACK_FILE_MODE 0444

#define PACK_SIGNATURE 0x5041434b	/* "PACK" */
#define PACK_VERSION 2
#define pack_version_ok(v) ((v) == htonl(2))
struct git_pack_header {
	uint32_t hdr_signature;
	uint32_t hdr_version;
	uint32_t hdr_entries;
};

/*
 * The first four bytes of index formats later than version 1 should
 * start with this signature, as all older git binaries would find this
 * value illegal and abort reading the file.
 *
 * This is the case because the number of objects in a packfile
 * cannot exceed 1,431,660,000 as every object would need at least
 * 3 bytes of data and the overall packfile cannot exceed 4 GiB with
 * version 1 of the index file due to the offsets limited to 32 bits.
 * Clearly the signature exceeds this maximum.
 *
 * Very old git binaries will also compare the first 4 bytes to the
 * next 4 bytes in the index and abort with a "non-monotonic index"
 * error if the second 4 byte word is smaller than the first 4
 * byte word. This would be true in the proposed future index
 * format as idx_signature would be greater than idx_version.
 */

#define PACK_IDX_SIGNATURE 0xff744f63	/* "\377tOc" */

struct git_pack_idx_header {
	uint32_t idx_signature;
	uint32_t idx_version;
};

typedef struct git_pack_cache_entry {
	size_t last_usage; /* enough? */
	git_atomic32 refcount;
	git_rawobj raw;
} git_pack_cache_entry;

struct pack_chain_elem {
	off64_t base_key;
	off64_t offset;
	size_t size;
	git_object_t type;
};

typedef git_array_t(struct pack_chain_elem) git_dependency_chain;

#define GIT_PACK_CACHE_MEMORY_LIMIT 16 * 1024 * 1024
#define GIT_PACK_CACHE_SIZE_LIMIT 1024 * 1024 /* don't bother caching anything over 1MB */

struct git_pack_entry {
	off64_t offset;
	git_oid id;
	struct git_pack_file *p;
};

GIT_HASHMAP_STRUCT(git_pack_offsetmap, off64_t, git_pack_cache_entry *);

GIT_HASHMAP_OID_STRUCT(git_pack_oidmap, struct git_pack_entry *);
GIT_HASHMAP_OID_PROTOTYPES(git_pack_oidmap, struct git_pack_entry *);

typedef struct {
	size_t memory_used;
	size_t memory_limit;
	size_t use_ctr;
	git_mutex lock;
	git_pack_offsetmap entries;
} git_pack_cache;

struct git_pack_file {
	git_mwindow_file mwf;
	git_map index_map;
	git_mutex lock; /* protect updates to index_map */
	git_atomic32 refcount;

	uint32_t num_objects;
	uint32_t num_bad_objects;
	git_oid *bad_object_ids; /* array of git_oid */

	git_oid_t oid_type;
	unsigned oid_hexsize:7,
	         oid_size:6,
	         pack_local:1,
	         pack_keep:1,
		 has_cache:1;

	int index_version;
	git_time_t mtime;

	git_pack_oidmap idx_cache;
	unsigned char **ids;

	git_pack_cache bases; /* delta base cache */

	time_t last_freshen; /* last time the packfile was freshened */

	/* something like ".git/objects/pack/xxxxx.pack" */
	char pack_name[GIT_FLEX_ARRAY]; /* more */
};

/**
 * Return the position where an OID (or a prefix) would be inserted within
 * the OID Lookup Table of an .idx file. This performs binary search
 * between the lo and hi indices.
 *
 * The stride parameter is provided because .idx files version 1 store the
 * OIDs interleaved with the 4-byte file offsets of the objects within the
 * .pack file (stride = oid_size + 4), whereas files with version 2 store
 * them in a contiguous flat array (stride = oid_size).
 */
int git_pack__lookup_id(
	const void *id_lookup_table,
	size_t stride,
	unsigned lo,
	unsigned hi,
	const unsigned char *id_prefix,
	const git_oid_t oid_type);

typedef struct git_packfile_stream {
	off64_t curpos;
	int done;
	git_zstream zstream;
	struct git_pack_file *p;
	git_mwindow *mw;
} git_packfile_stream;

int git_packfile__object_header(size_t *out, unsigned char *hdr, size_t size, git_object_t type);

int git_packfile__name(char **out, const char *path);

int git_packfile_unpack_header(
		size_t *size_p,
		git_object_t *type_p,
		struct git_pack_file *p,
		git_mwindow **w_curs,
		off64_t *curpos);

int git_packfile_resolve_header(
		size_t *size_p,
		git_object_t *type_p,
		struct git_pack_file *p,
		off64_t offset);

int git_packfile_unpack(git_rawobj *obj, struct git_pack_file *p, off64_t *obj_offset);

int git_packfile_stream_open(git_packfile_stream *obj, struct git_pack_file *p, off64_t curpos);
ssize_t git_packfile_stream_read(git_packfile_stream *obj, void *buffer, size_t len);
void git_packfile_stream_dispose(git_packfile_stream *obj);

int get_delta_base(
		off64_t *delta_base_out,
		struct git_pack_file *p,
		git_mwindow **w_curs,
		off64_t *curpos,
		git_object_t type,
		off64_t delta_obj_offset);

void git_packfile_free(struct git_pack_file *p, bool unlink_packfile);
int git_packfile_alloc(
	struct git_pack_file **pack_out,
	const char *path,
	git_oid_t oid_type);

int git_pack_entry_find(
		struct git_pack_entry *e,
		struct git_pack_file *p,
		const git_oid *short_id,
		size_t len);
int git_pack_foreach_entry(
		struct git_pack_file *p,
		git_odb_foreach_cb cb,
		void *data);
/**
 * Similar to git_pack_foreach_entry, but:
 * - It also provides the offset of the object within the
 *   packfile.
 * - It does not sort the objects in any order.
 * - It retains the lock while invoking the callback.
 */
int git_pack_foreach_entry_offset(
		struct git_pack_file *p,
		git_pack_foreach_entry_offset_cb cb,
		void *data);

#endif
