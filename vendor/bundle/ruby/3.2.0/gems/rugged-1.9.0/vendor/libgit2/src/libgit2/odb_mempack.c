/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "buf.h"
#include "futils.h"
#include "hash.h"
#include "odb.h"
#include "array.h"
#include "pack-objects.h"

#include "git2/odb_backend.h"
#include "git2/object.h"
#include "git2/types.h"
#include "git2/pack.h"
#include "git2/sys/odb_backend.h"
#include "git2/sys/mempack.h"

struct memobject {
	git_oid oid;
	size_t len;
	git_object_t type;
	char data[GIT_FLEX_ARRAY];
};

GIT_HASHMAP_OID_SETUP(git_odb_mempack_oidmap, struct memobject *);

struct memory_packer_db {
	git_odb_backend parent;
	git_odb_mempack_oidmap objects;
	git_array_t(struct memobject *) commits;
};

static int impl__write(git_odb_backend *_backend, const git_oid *oid, const void *data, size_t len, git_object_t type)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;
	struct memobject *obj = NULL;
	size_t alloc_len;

	if (git_odb_mempack_oidmap_contains(&db->objects, oid))
		return 0;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, sizeof(struct memobject), len);
	obj = git__malloc(alloc_len);
	GIT_ERROR_CHECK_ALLOC(obj);

	memcpy(obj->data, data, len);
	git_oid_cpy(&obj->oid, oid);
	obj->len = len;
	obj->type = type;

	if (git_odb_mempack_oidmap_put(&db->objects, &obj->oid, obj) < 0)
		return -1;

	if (type == GIT_OBJECT_COMMIT) {
		struct memobject **store = git_array_alloc(db->commits);
		GIT_ERROR_CHECK_ALLOC(store);
		*store = obj;
	}

	return 0;
}

static int impl__exists(git_odb_backend *backend, const git_oid *oid)
{
	struct memory_packer_db *db = (struct memory_packer_db *)backend;

	return git_odb_mempack_oidmap_contains(&db->objects, oid);
}

static int impl__read(void **buffer_p, size_t *len_p, git_object_t *type_p, git_odb_backend *backend, const git_oid *oid)
{
	struct memory_packer_db *db = (struct memory_packer_db *)backend;
	struct memobject *obj;
	int error;

	if ((error = git_odb_mempack_oidmap_get(&obj, &db->objects, oid)) != 0)
		return error;

	*len_p = obj->len;
	*type_p = obj->type;
	*buffer_p = git__malloc(obj->len);
	GIT_ERROR_CHECK_ALLOC(*buffer_p);

	memcpy(*buffer_p, obj->data, obj->len);
	return 0;
}

static int impl__read_header(size_t *len_p, git_object_t *type_p, git_odb_backend *backend, const git_oid *oid)
{
	struct memory_packer_db *db = (struct memory_packer_db *)backend;
	struct memobject *obj;
	int error;

	if ((error = git_odb_mempack_oidmap_get(&obj, &db->objects, oid)) != 0)
		return error;

	*len_p = obj->len;
	*type_p = obj->type;
	return 0;
}

static int git_mempack__dump(
	git_str *pack,
	git_repository *repo,
	git_odb_backend *_backend)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;
	git_packbuilder *packbuilder;
	uint32_t i;
	int err = -1;

	if (git_packbuilder_new(&packbuilder, repo) < 0)
		return -1;

	git_packbuilder_set_threads(packbuilder, 0);

	for (i = 0; i < db->commits.size; ++i) {
		struct memobject *commit = db->commits.ptr[i];

		err = git_packbuilder_insert_commit(packbuilder, &commit->oid);
		if (err < 0)
			goto cleanup;
	}

	err = git_packbuilder__write_buf(pack, packbuilder);

cleanup:
	git_packbuilder_free(packbuilder);
	return err;
}

int git_mempack_write_thin_pack(git_odb_backend *backend, git_packbuilder *pb)
{
	struct memory_packer_db *db = (struct memory_packer_db *)backend;
	const git_oid *oid;
	git_hashmap_iter_t iter = GIT_HASHMAP_INIT;
	int err;

	while (true) {
		err = git_odb_mempack_oidmap_iterate(&iter, &oid, NULL, &db->objects);

		if (err == GIT_ITEROVER)
			break;
		else if (err != 0)
			return err;

		err = git_packbuilder_insert(pb, oid, NULL);
		if (err != 0)
			return err;
	}

	return 0;
}

int git_mempack_dump(
	git_buf *pack,
	git_repository *repo,
	git_odb_backend *_backend)
{
	GIT_BUF_WRAP_PRIVATE(pack, git_mempack__dump, repo, _backend);
}

int git_mempack_reset(git_odb_backend *_backend)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;
	struct memobject *object = NULL;
	git_hashmap_iter_t iter = GIT_HASHMAP_ITER_INIT;

	while (git_odb_mempack_oidmap_iterate(&iter, NULL, &object, &db->objects) == 0)
		git__free(object);

	git_array_clear(db->commits);
	git_odb_mempack_oidmap_clear(&db->objects);

	return 0;
}

static void impl__free(git_odb_backend *_backend)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;

	git_mempack_reset(_backend);
	git_odb_mempack_oidmap_dispose(&db->objects);
	git__free(db);
}

int git_mempack_new(git_odb_backend **out)
{
	struct memory_packer_db *db;

	GIT_ASSERT_ARG(out);

	db = git__calloc(1, sizeof(struct memory_packer_db));
	GIT_ERROR_CHECK_ALLOC(db);

	db->parent.version = GIT_ODB_BACKEND_VERSION;
	db->parent.read = &impl__read;
	db->parent.write = &impl__write;
	db->parent.read_header = &impl__read_header;
	db->parent.exists = &impl__exists;
	db->parent.free = &impl__free;

	*out = (git_odb_backend *)db;
	return 0;
}

int git_mempack_object_count(size_t *out, git_odb_backend *_backend)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;

	GIT_ASSERT_ARG(_backend);

	*out = (size_t)git_odb_mempack_oidmap_size(&db->objects);
	return 0;
}
