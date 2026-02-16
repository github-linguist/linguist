/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_odb_h__
#define INCLUDE_odb_h__

#include "common.h"

#include "git2/odb.h"
#include "git2/odb_backend.h"
#include "git2/oid.h"
#include "git2/types.h"
#include "git2/sys/commit_graph.h"

#include "cache.h"
#include "commit_graph.h"
#include "filter.h"
#include "posix.h"
#include "vector.h"

#define GIT_OBJECTS_DIR "objects/"
#define GIT_OBJECT_DIR_MODE 0777
#define GIT_OBJECT_FILE_MODE 0444

#define GIT_ODB_DEFAULT_LOOSE_PRIORITY 1
#define GIT_ODB_DEFAULT_PACKED_PRIORITY 2

extern bool git_odb__strict_hash_verification;

/* DO NOT EXPORT */
typedef struct {
	void *data;			/**< Raw, decompressed object data. */
	size_t len;			/**< Total number of bytes in data. */
	git_object_t type;		/**< Type of this object. */
} git_rawobj;

/* EXPORT */
struct git_odb_object {
	git_cached_obj cached;
	void *buffer;
};

/* EXPORT */
struct git_odb {
	git_refcount rc;
	git_mutex lock;  /* protects backends */
	git_odb_options options;
	git_vector backends;
	git_cache own_cache;
	git_commit_graph *cgraph;
	unsigned int do_fsync :1;
};

typedef enum {
	GIT_ODB_CAP_FROM_OWNER = -1
} git_odb_cap_t;

/*
 * Set the capabilities for the object database.
 */
int git_odb__set_caps(git_odb *odb, int caps);

/*
 * Add the default loose and packed backends for a database.
 */
int git_odb__add_default_backends(
	git_odb *db, const char *objects_dir,
	bool as_alternates, int alternate_depth);

/*
 * Hash a git_rawobj internally.
 * The `git_rawobj` is supposed to be previously initialized
 */
int git_odb__hashobj(git_oid *id, git_rawobj *obj, git_oid_t oid_type);

/*
 * Format the object header such as it would appear in the on-disk object
 */
int git_odb__format_object_header(size_t *out_len, char *hdr, size_t hdr_size, git_object_size_t obj_len, git_object_t obj_type);

/*
 * Hash an open file descriptor.
 * This is a performance call when the contents of a fd need to be hashed,
 * but the fd is already open and we have the size of the contents.
 *
 * Saves us some `stat` calls.
 *
 * The fd is never closed, not even on error. It must be opened and closed
 * by the caller
 */
int git_odb__hashfd(
	git_oid *out,
	git_file fd,
	size_t size,
	git_object_t object_type,
	git_oid_t oid_type);

/*
 * Hash an open file descriptor applying an array of filters
 * Acts just like git_odb__hashfd with the addition of filters...
 */
int git_odb__hashfd_filtered(
	git_oid *out,
	git_file fd,
	size_t len,
	git_object_t object_type,
	git_oid_t oid_type,
	git_filter_list *fl);

/*
 * Hash a `path`, assuming it could be a POSIX symlink: if the path is a
 * symlink, then the raw contents of the symlink will be hashed. Otherwise,
 * this will fallback to `git_odb__hashfd`.
 *
 * The hash type for this call is always `GIT_OBJECT_BLOB` because
 * symlinks may only point to blobs.
 */
int git_odb__hashlink(git_oid *out, const char *path, git_oid_t oid_type);

/**
 * Generate a GIT_EMISMATCH error for the ODB.
 */
int git_odb__error_mismatch(
	const git_oid *expected, const git_oid *actual);

/*
 * Generate a GIT_ENOTFOUND error for the ODB.
 */
int git_odb__error_notfound(
	const char *message, const git_oid *oid, size_t oid_len);

/*
 * Generate a GIT_EAMBIGUOUS error for the ODB.
 */
int git_odb__error_ambiguous(const char *message);

/*
 * Attempt to read object header or just return whole object if it could
 * not be read.
 */
int git_odb__read_header_or_object(
	git_odb_object **out, size_t *len_p, git_object_t *type_p,
	git_odb *db, const git_oid *id);

/*
 * Attempt to get the ODB's commit-graph file. This object is still owned by
 * the ODB. If the repository does not contain a commit-graph, it will return
 * GIT_ENOTFOUND.
 */
int git_odb__get_commit_graph_file(git_commit_graph_file **out, git_odb *odb);

/* freshen an entry in the object database */
int git_odb__freshen(git_odb *db, const git_oid *id);

/* fully free the object; internal method, DO NOT EXPORT */
void git_odb_object__free(void *object);

/* SHA256 support */

int git_odb__new(git_odb **out, const git_odb_options *opts);

int git_odb__open(
	git_odb **out,
	const char *objects_dir,
	const git_odb_options *opts);

int git_odb__hash(
	git_oid *out,
	const void *data,
	size_t len,
	git_object_t object_type,
	git_oid_t oid_type);

int git_odb__hashfile(
	git_oid *out,
	const char *path,
	git_object_t object_type,
	git_oid_t oid_type);

GIT_EXTERN(int) git_odb__backend_loose(
	git_odb_backend **out,
	const char *objects_dir,
	git_odb_backend_loose_options *opts);

#endif
