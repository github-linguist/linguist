/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_midx_h__
#define INCLUDE_midx_h__

#include "common.h"

#include <ctype.h>

#include "git2/sys/midx.h"

#include "map.h"
#include "mwindow.h"
#include "odb.h"
#include "oid.h"

/*
 * A multi-pack-index file.
 *
 * This file contains a merged index for multiple independent .pack files. This
 * can help speed up locating objects without requiring a garbage collection
 * cycle to create a single .pack file.
 *
 * Support for this feature was added in git 2.21, and requires the
 * `core.multiPackIndex` config option to be set.
 */
typedef struct git_midx_file {
	git_map index_map;

	/* The table of Packfile Names. */
	git_vector packfile_names;

	/* The OID Fanout table. */
	const uint32_t *oid_fanout;
	/* The total number of objects in the index. */
	uint32_t num_objects;

	/* The OID Lookup table. */
	unsigned char *oid_lookup;

	/* The Object Offsets table. Each entry has two 4-byte fields with the pack index and the offset. */
	const unsigned char *object_offsets;

	/* The Object Large Offsets table. */
	const unsigned char *object_large_offsets;
	/* The number of entries in the Object Large Offsets table. Each entry has an 8-byte with an offset */
	size_t num_object_large_offsets;

	/*
	 * The trailer of the file. Contains the checksum of the whole
	 * file, in the repository's object format hash.
	 */
	unsigned char checksum[GIT_HASH_MAX_SIZE];

	/* The type of object IDs in the midx. */
	git_oid_t oid_type;

	/* something like ".git/objects/pack/multi-pack-index". */
	git_str filename;
} git_midx_file;

/*
 * An entry in the multi-pack-index file. Similar in purpose to git_pack_entry.
 */
typedef struct git_midx_entry {
	/* The index within idx->packfile_names where the packfile name can be found. */
	size_t pack_index;
	/* The offset within the .pack file where the requested object is found. */
	off64_t offset;
	/* The SHA-1 hash of the requested object. */
	git_oid sha1;
} git_midx_entry;

/*
 * A writer for `multi-pack-index` files.
 */
struct git_midx_writer {
	/*
	 * The path of the directory where the .pack/.idx files are stored. The
	 * `multi-pack-index` file will be written to the same directory.
	 */
	git_str pack_dir;

	/* The list of `git_pack_file`s. */
	git_vector packs;

	/* The object ID type of the writer. */
	git_oid_t oid_type;
};

int git_midx_open(
		git_midx_file **idx_out,
		const char *path,
		git_oid_t oid_type);
bool git_midx_needs_refresh(
		const git_midx_file *idx,
		const char *path);
int git_midx_entry_find(
		git_midx_entry *e,
		git_midx_file *idx,
		const git_oid *short_oid,
		size_t len);
int git_midx_foreach_entry(
		git_midx_file *idx,
		git_odb_foreach_cb cb,
		void *data);
int git_midx_close(git_midx_file *idx);
void git_midx_free(git_midx_file *idx);

/* This is exposed for use in the fuzzers. */
int git_midx_parse(
		git_midx_file *idx,
		const unsigned char *data,
		size_t size);

#endif
