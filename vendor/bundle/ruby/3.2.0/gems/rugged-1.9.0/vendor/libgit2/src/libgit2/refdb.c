/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "refdb.h"

#include "git2/object.h"
#include "git2/refs.h"
#include "git2/refdb.h"
#include "git2/sys/refdb_backend.h"

#include "hash.h"
#include "refs.h"
#include "reflog.h"
#include "posix.h"

#define DEFAULT_NESTING_LEVEL	5
#define MAX_NESTING_LEVEL		10

int git_refdb_new(git_refdb **out, git_repository *repo)
{
	git_refdb *db;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);

	db = git__calloc(1, sizeof(*db));
	GIT_ERROR_CHECK_ALLOC(db);

	db->repo = repo;

	*out = db;
	GIT_REFCOUNT_INC(db);
	return 0;
}

int git_refdb_open(git_refdb **out, git_repository *repo)
{
	git_refdb *db;
	git_refdb_backend *dir;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);

	*out = NULL;

	if (git_refdb_new(&db, repo) < 0)
		return -1;

	/* Add the default (filesystem) backend */
	if (git_refdb_backend_fs(&dir, repo) < 0) {
		git_refdb_free(db);
		return -1;
	}

	db->repo = repo;
	db->backend = dir;

	*out = db;
	return 0;
}

static void refdb_free_backend(git_refdb *db)
{
	if (db->backend)
		db->backend->free(db->backend);
}

int git_refdb_set_backend(git_refdb *db, git_refdb_backend *backend)
{
	GIT_ERROR_CHECK_VERSION(backend, GIT_REFDB_BACKEND_VERSION, "git_refdb_backend");

	if (!backend->exists || !backend->lookup || !backend->iterator ||
	    !backend->write || !backend->rename || !backend->del ||
	    !backend->has_log || !backend->ensure_log || !backend->free ||
	    !backend->reflog_read || !backend->reflog_write ||
	    !backend->reflog_rename || !backend->reflog_delete ||
	    (backend->lock && !backend->unlock)) {
		git_error_set(GIT_ERROR_REFERENCE, "incomplete refdb backend implementation");
		return GIT_EINVALID;
	}

	refdb_free_backend(db);
	db->backend = backend;

	return 0;
}

int git_refdb_compress(git_refdb *db)
{
	GIT_ASSERT_ARG(db);

	if (db->backend->compress)
		return db->backend->compress(db->backend);

	return 0;
}

void git_refdb__free(git_refdb *db)
{
	refdb_free_backend(db);
	git__memzero(db, sizeof(*db));
	git__free(db);
}

void git_refdb_free(git_refdb *db)
{
	if (db == NULL)
		return;

	GIT_REFCOUNT_DEC(db, git_refdb__free);
}

int git_refdb_exists(int *exists, git_refdb *refdb, const char *ref_name)
{
	GIT_ASSERT_ARG(exists);
	GIT_ASSERT_ARG(refdb);
	GIT_ASSERT_ARG(refdb->backend);

	return refdb->backend->exists(exists, refdb->backend, ref_name);
}

int git_refdb_lookup(git_reference **out, git_refdb *db, const char *ref_name)
{
	git_reference *ref;
	int error;

	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(db->backend);
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(ref_name);

	error = db->backend->lookup(&ref, db->backend, ref_name);
	if (error < 0)
		return error;

	GIT_REFCOUNT_INC(db);
	ref->db = db;

	*out = ref;
	return 0;
}

int git_refdb_resolve(
	git_reference **out,
	git_refdb *db,
	const char *ref_name,
	int max_nesting)
{
	git_reference *ref = NULL;
	int error = 0, nesting;

	*out = NULL;

	if (max_nesting > MAX_NESTING_LEVEL)
		max_nesting = MAX_NESTING_LEVEL;
	else if (max_nesting < 0)
		max_nesting = DEFAULT_NESTING_LEVEL;

	if ((error = git_refdb_lookup(&ref, db, ref_name)) < 0)
		goto out;

	for (nesting = 0; nesting < max_nesting; nesting++) {
		git_reference *resolved;

		if (ref->type == GIT_REFERENCE_DIRECT)
			break;

		if ((error = git_refdb_lookup(&resolved, db, git_reference_symbolic_target(ref))) < 0) {
			/* If we found a symbolic reference with a nonexistent target, return it. */
			if (error == GIT_ENOTFOUND) {
				error = 0;
				*out = ref;
				ref = NULL;
			}
			goto out;
		}

		git_reference_free(ref);
		ref = resolved;
	}

	if (ref->type != GIT_REFERENCE_DIRECT && max_nesting != 0) {
		git_error_set(GIT_ERROR_REFERENCE,
			"cannot resolve reference (>%u levels deep)", max_nesting);
		error = -1;
		goto out;
	}

	*out = ref;
	ref = NULL;
out:
	git_reference_free(ref);
	return error;
}

int git_refdb_iterator(git_reference_iterator **out, git_refdb *db, const char *glob)
{
	int error;

	if (!db->backend || !db->backend->iterator) {
		git_error_set(GIT_ERROR_REFERENCE, "this backend doesn't support iterators");
		return -1;
	}

	if ((error = db->backend->iterator(out, db->backend, glob)) < 0)
		return error;

	GIT_REFCOUNT_INC(db);
	(*out)->db = db;

	return 0;
}

int git_refdb_iterator_next(git_reference **out, git_reference_iterator *iter)
{
	int error;

	if ((error = iter->next(out, iter)) < 0)
		return error;

	GIT_REFCOUNT_INC(iter->db);
	(*out)->db = iter->db;

	return 0;
}

int git_refdb_iterator_next_name(const char **out, git_reference_iterator *iter)
{
	return iter->next_name(out, iter);
}

void git_refdb_iterator_free(git_reference_iterator *iter)
{
	GIT_REFCOUNT_DEC(iter->db, git_refdb__free);
	iter->free(iter);
}

int git_refdb_write(git_refdb *db, git_reference *ref, int force, const git_signature *who, const char *message, const git_oid *old_id, const char *old_target)
{
	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(db->backend);

	GIT_REFCOUNT_INC(db);
	ref->db = db;

	return db->backend->write(db->backend, ref, force, who, message, old_id, old_target);
}

int git_refdb_rename(
	git_reference **out,
	git_refdb *db,
	const char *old_name,
	const char *new_name,
	int force,
	const git_signature *who,
	const char *message)
{
	int error;

	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(db->backend);

	error = db->backend->rename(out, db->backend, old_name, new_name, force, who, message);
	if (error < 0)
		return error;

	if (out) {
		GIT_REFCOUNT_INC(db);
		(*out)->db = db;
	}

	return 0;
}

int git_refdb_delete(struct git_refdb *db, const char *ref_name, const git_oid *old_id, const char *old_target)
{
	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(db->backend);

	return db->backend->del(db->backend, ref_name, old_id, old_target);
}

int git_refdb_reflog_read(git_reflog **out, git_refdb *db,  const char *name)
{
	int error;

	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(db->backend);

	if ((error = db->backend->reflog_read(out, db->backend, name)) < 0)
		return error;

	GIT_REFCOUNT_INC(db);
	(*out)->db = db;

	return 0;
}

int git_refdb_should_write_reflog(int *out, git_refdb *db, const git_reference *ref)
{
	int error, logall;

	error = git_repository__configmap_lookup(&logall, db->repo, GIT_CONFIGMAP_LOGALLREFUPDATES);
	if (error < 0)
		return error;

	/* Defaults to the opposite of the repo being bare */
	if (logall == GIT_LOGALLREFUPDATES_UNSET)
		logall = !git_repository_is_bare(db->repo);

	*out = 0;
	switch (logall) {
	case GIT_LOGALLREFUPDATES_FALSE:
		*out = 0;
		break;

	case GIT_LOGALLREFUPDATES_TRUE:
		/* Only write if it already has a log,
		 * or if it's under heads/, remotes/ or notes/
		 */
		*out = git_refdb_has_log(db, ref->name) ||
			!git__prefixcmp(ref->name, GIT_REFS_HEADS_DIR) ||
			!git__strcmp(ref->name, GIT_HEAD_FILE) ||
			!git__prefixcmp(ref->name, GIT_REFS_REMOTES_DIR) ||
			!git__prefixcmp(ref->name, GIT_REFS_NOTES_DIR);
		break;

	case GIT_LOGALLREFUPDATES_ALWAYS:
		*out = 1;
		break;
	}

	return 0;
}

int git_refdb_should_write_head_reflog(int *out, git_refdb *db, const git_reference *ref)
{
	git_reference *head = NULL, *resolved = NULL;
	const char *name;
	int error;

	*out = 0;

	if (ref->type == GIT_REFERENCE_SYMBOLIC) {
		error = 0;
		goto out;
	}

	if ((error = git_refdb_lookup(&head, db, GIT_HEAD_FILE)) < 0)
		goto out;

	if (git_reference_type(head) == GIT_REFERENCE_DIRECT)
		goto out;

	/* Go down the symref chain until we find the branch */
	if ((error = git_refdb_resolve(&resolved, db, git_reference_symbolic_target(head), -1)) < 0) {
		if (error != GIT_ENOTFOUND)
			goto out;
		error = 0;
		name = git_reference_symbolic_target(head);
	} else if (git_reference_type(resolved) == GIT_REFERENCE_SYMBOLIC) {
		name = git_reference_symbolic_target(resolved);
	} else {
		name = git_reference_name(resolved);
	}

	if (strcmp(name, ref->name))
		goto out;

	*out = 1;

out:
	git_reference_free(resolved);
	git_reference_free(head);
	return error;
}

int git_refdb_has_log(git_refdb *db, const char *refname)
{
	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(refname);

	return db->backend->has_log(db->backend, refname);
}

int git_refdb_ensure_log(git_refdb *db, const char *refname)
{
	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(refname);

	return db->backend->ensure_log(db->backend, refname);
}

int git_refdb_init_backend(git_refdb_backend *backend, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		backend, version, git_refdb_backend, GIT_REFDB_BACKEND_INIT);
	return 0;
}

int git_refdb_lock(void **payload, git_refdb *db, const char *refname)
{
	GIT_ASSERT_ARG(payload);
	GIT_ASSERT_ARG(db);
	GIT_ASSERT_ARG(refname);

	if (!db->backend->lock) {
		git_error_set(GIT_ERROR_REFERENCE, "backend does not support locking");
		return -1;
	}

	return db->backend->lock(payload, db->backend, refname);
}

int git_refdb_unlock(git_refdb *db, void *payload, int success, int update_reflog, const git_reference *ref, const git_signature *sig, const char *message)
{
	GIT_ASSERT_ARG(db);

	return db->backend->unlock(db->backend, payload, success, update_reflog, ref, sig, message);
}
