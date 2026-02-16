/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "reflog.h"

#include "repository.h"
#include "filebuf.h"
#include "signature.h"
#include "refdb.h"

#include "git2/sys/refdb_backend.h"

void git_reflog_entry__free(git_reflog_entry *entry)
{
	git_signature_free(entry->committer);

	git__free(entry->msg);
	git__free(entry);
}

void git_reflog_free(git_reflog *reflog)
{
	size_t i;
	git_reflog_entry *entry;

	if (reflog == NULL)
		return;

	if (reflog->db)
		GIT_REFCOUNT_DEC(reflog->db, git_refdb__free);

	for (i=0; i < reflog->entries.length; i++) {
		entry = git_vector_get(&reflog->entries, i);

		git_reflog_entry__free(entry);
	}

	git_vector_dispose(&reflog->entries);
	git__free(reflog->ref_name);
	git__free(reflog);
}

int git_reflog_read(git_reflog **reflog, git_repository *repo,  const char *name)
{
	git_refdb *refdb;
	int error;

	GIT_ASSERT_ARG(reflog);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(name);

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return error;

	return git_refdb_reflog_read(reflog, refdb, name);
}

int git_reflog_write(git_reflog *reflog)
{
	git_refdb *db;

	GIT_ASSERT_ARG(reflog);
	GIT_ASSERT_ARG(reflog->db);

	db = reflog->db;
	return db->backend->reflog_write(db->backend, reflog);
}

int git_reflog_append(
	git_reflog *reflog,
	const git_oid *new_oid,
	const git_signature *committer,
	const char *msg)
{
	const git_reflog_entry *previous;
	git_reflog_entry *entry;

	GIT_ASSERT_ARG(reflog);
	GIT_ASSERT_ARG(new_oid);
	GIT_ASSERT_ARG(committer);

	entry = git__calloc(1, sizeof(git_reflog_entry));
	GIT_ERROR_CHECK_ALLOC(entry);

	if ((git_signature_dup(&entry->committer, committer)) < 0)
		goto cleanup;

	if (msg != NULL) {
		size_t i, msglen = strlen(msg);

		if ((entry->msg = git__strndup(msg, msglen)) == NULL)
			goto cleanup;

		/*
		 * Replace all newlines with spaces, except for
		 * the final trailing newline.
		 */
		for (i = 0; i < msglen; i++)
			if (entry->msg[i] == '\n')
				entry->msg[i] = ' ';
	}

	previous = git_reflog_entry_byindex(reflog, 0);

	if (previous == NULL)
		git_oid_clear(&entry->oid_old, reflog->oid_type);
	else
		git_oid_cpy(&entry->oid_old, &previous->oid_cur);

	git_oid_cpy(&entry->oid_cur, new_oid);

	if (git_vector_insert(&reflog->entries, entry) < 0)
		goto cleanup;

	return 0;

cleanup:
	git_reflog_entry__free(entry);
	return -1;
}

int git_reflog_rename(git_repository *repo, const char *old_name, const char *new_name)
{
	git_refdb *refdb;
	int error;

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return -1;

	return refdb->backend->reflog_rename(refdb->backend, old_name, new_name);
}

int git_reflog_delete(git_repository *repo, const char *name)
{
	git_refdb *refdb;
	int error;

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return -1;

	return refdb->backend->reflog_delete(refdb->backend, name);
}

size_t git_reflog_entrycount(git_reflog *reflog)
{
	GIT_ASSERT_ARG_WITH_RETVAL(reflog, 0);
	return reflog->entries.length;
}

const git_reflog_entry *git_reflog_entry_byindex(const git_reflog *reflog, size_t idx)
{
	GIT_ASSERT_ARG_WITH_RETVAL(reflog, NULL);

	if (idx >= reflog->entries.length)
		return NULL;

	return git_vector_get(
		&reflog->entries, reflog_inverse_index(idx, reflog->entries.length));
}

const git_oid *git_reflog_entry_id_old(const git_reflog_entry *entry)
{
	GIT_ASSERT_ARG_WITH_RETVAL(entry, NULL);
	return &entry->oid_old;
}

const git_oid *git_reflog_entry_id_new(const git_reflog_entry *entry)
{
	GIT_ASSERT_ARG_WITH_RETVAL(entry, NULL);
	return &entry->oid_cur;
}

const git_signature *git_reflog_entry_committer(const git_reflog_entry *entry)
{
	GIT_ASSERT_ARG_WITH_RETVAL(entry, NULL);
	return entry->committer;
}

const char *git_reflog_entry_message(const git_reflog_entry *entry)
{
	GIT_ASSERT_ARG_WITH_RETVAL(entry, NULL);
	return entry->msg;
}

int git_reflog_drop(git_reflog *reflog, size_t idx, int rewrite_previous_entry)
{
	size_t entrycount;
	git_reflog_entry *entry, *previous;

	entrycount = git_reflog_entrycount(reflog);

	entry = (git_reflog_entry *)git_reflog_entry_byindex(reflog, idx);

	if (entry == NULL) {
		git_error_set(GIT_ERROR_REFERENCE, "no reflog entry at index %"PRIuZ, idx);
		return GIT_ENOTFOUND;
	}

	git_reflog_entry__free(entry);

	if (git_vector_remove(
			&reflog->entries, reflog_inverse_index(idx, entrycount)) < 0)
		return -1;

	if (!rewrite_previous_entry)
		return 0;

	/* No need to rewrite anything when removing the most recent entry */
	if (idx == 0)
		return 0;

	/* Have the latest entry just been dropped? */
	if (entrycount == 1)
		return 0;

	entry = (git_reflog_entry *)git_reflog_entry_byindex(reflog, idx - 1);

	/* If the oldest entry has just been removed... */
	if (idx == entrycount - 1) {
		/* ...clear the oid_old member of the "new" oldest entry */
		git_oid_clear(&entry->oid_old, reflog->oid_type);
		return 0;
	}

	previous = (git_reflog_entry *)git_reflog_entry_byindex(reflog, idx);
	git_oid_cpy(&entry->oid_old, &previous->oid_cur);

	return 0;
}
