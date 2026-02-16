/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "refs.h"

#include "hash.h"
#include "repository.h"
#include "futils.h"
#include "filebuf.h"
#include "pack.h"
#include "reflog.h"
#include "refdb.h"

#include <git2/tag.h>
#include <git2/object.h>
#include <git2/oid.h>
#include <git2/branch.h>
#include <git2/refs.h>
#include <git2/refdb.h>
#include <git2/sys/refs.h>
#include <git2/signature.h>
#include <git2/commit.h>

bool git_reference__enable_symbolic_ref_target_validation = true;

enum {
	GIT_PACKREF_HAS_PEEL = 1,
	GIT_PACKREF_WAS_LOOSE = 2
};

static git_reference *alloc_ref(const char *name)
{
	git_reference *ref = NULL;
	size_t namelen = strlen(name), reflen;

	if (!GIT_ADD_SIZET_OVERFLOW(&reflen, sizeof(git_reference), namelen) &&
		!GIT_ADD_SIZET_OVERFLOW(&reflen, reflen, 1) &&
		(ref = git__calloc(1, reflen)) != NULL)
		memcpy(ref->name, name, namelen + 1);

	return ref;
}

git_reference *git_reference__alloc_symbolic(
	const char *name, const char *target)
{
	git_reference *ref;

	GIT_ASSERT_ARG_WITH_RETVAL(name, NULL);
	GIT_ASSERT_ARG_WITH_RETVAL(target, NULL);

	ref = alloc_ref(name);
	if (!ref)
		return NULL;

	ref->type = GIT_REFERENCE_SYMBOLIC;

	if ((ref->target.symbolic = git__strdup(target)) == NULL) {
		git__free(ref);
		return NULL;
	}

	return ref;
}

git_reference *git_reference__alloc(
	const char *name,
	const git_oid *oid,
	const git_oid *peel)
{
	git_oid_t oid_type;
	git_reference *ref;

	GIT_ASSERT_ARG_WITH_RETVAL(name, NULL);
	GIT_ASSERT_ARG_WITH_RETVAL(oid, NULL);

	ref = alloc_ref(name);
	if (!ref)
		return NULL;

	ref->type = GIT_REFERENCE_DIRECT;
	git_oid_cpy(&ref->target.oid, oid);

#ifdef GIT_EXPERIMENTAL_SHA256
	oid_type = oid->type;
#else
	oid_type = GIT_OID_SHA1;
#endif

	if (peel != NULL)
		git_oid_cpy(&ref->peel, peel);
	else
		git_oid_clear(&ref->peel, oid_type);

	return ref;
}

git_reference *git_reference__realloc(
	git_reference **ptr_to_ref, const char *name)
{
	size_t namelen, reflen;
	git_reference *rewrite = NULL;

	GIT_ASSERT_ARG_WITH_RETVAL(ptr_to_ref, NULL);
	GIT_ASSERT_ARG_WITH_RETVAL(name, NULL);

	namelen = strlen(name);

	if (!GIT_ADD_SIZET_OVERFLOW(&reflen, sizeof(git_reference), namelen) &&
		!GIT_ADD_SIZET_OVERFLOW(&reflen, reflen, 1) &&
		(rewrite = git__realloc(*ptr_to_ref, reflen)) != NULL)
		memcpy(rewrite->name, name, namelen + 1);

	*ptr_to_ref = NULL;

	return rewrite;
}

int git_reference_dup(git_reference **dest, git_reference *source)
{
	if (source->type == GIT_REFERENCE_SYMBOLIC)
		*dest = git_reference__alloc_symbolic(source->name, source->target.symbolic);
	else
		*dest = git_reference__alloc(source->name, &source->target.oid, &source->peel);

	GIT_ERROR_CHECK_ALLOC(*dest);

	(*dest)->db = source->db;
	GIT_REFCOUNT_INC((*dest)->db);

	return 0;
}

void git_reference_free(git_reference *reference)
{
	if (reference == NULL)
		return;

	if (reference->type == GIT_REFERENCE_SYMBOLIC)
		git__free(reference->target.symbolic);

	if (reference->db)
		GIT_REFCOUNT_DEC(reference->db, git_refdb__free);

	git__free(reference);
}

int git_reference_delete(git_reference *ref)
{
	const git_oid *old_id = NULL;
	const char *old_target = NULL;

	if (!strcmp(ref->name, "HEAD")) {
		git_error_set(GIT_ERROR_REFERENCE, "cannot delete HEAD");
		return GIT_ERROR;
	}

	if (ref->type == GIT_REFERENCE_DIRECT)
		old_id = &ref->target.oid;
	else
		old_target = ref->target.symbolic;

	return git_refdb_delete(ref->db, ref->name, old_id, old_target);
}

int git_reference_remove(git_repository *repo, const char *name)
{
	git_refdb *db;
	int error;

	if ((error = git_repository_refdb__weakptr(&db, repo)) < 0)
		return error;

	return git_refdb_delete(db, name, NULL, NULL);
}

int git_reference_lookup(git_reference **ref_out,
	git_repository *repo, const char *name)
{
	return git_reference_lookup_resolved(ref_out, repo, name, 0);
}

int git_reference_name_to_id(
	git_oid *out, git_repository *repo, const char *name)
{
	int error;
	git_reference *ref;

	if ((error = git_reference_lookup_resolved(&ref, repo, name, -1)) < 0)
		return error;

	git_oid_cpy(out, git_reference_target(ref));
	git_reference_free(ref);
	return 0;
}

static int reference_normalize_for_repo(
	git_refname_t out,
	git_repository *repo,
	const char *name,
	bool validate)
{
	int precompose;
	unsigned int flags = GIT_REFERENCE_FORMAT_ALLOW_ONELEVEL;

	if (!git_repository__configmap_lookup(&precompose, repo, GIT_CONFIGMAP_PRECOMPOSE) &&
		precompose)
		flags |= GIT_REFERENCE_FORMAT__PRECOMPOSE_UNICODE;

	if (!validate)
		flags |= GIT_REFERENCE_FORMAT__VALIDATION_DISABLE;

	return git_reference_normalize_name(out, GIT_REFNAME_MAX, name, flags);
}

int git_reference_lookup_resolved(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	int max_nesting)
{
	git_refname_t normalized;
	git_refdb *refdb;
	int error = 0;

	GIT_ASSERT_ARG(ref_out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(name);

	if ((error = reference_normalize_for_repo(normalized, repo, name, true)) < 0 ||
	    (error = git_repository_refdb__weakptr(&refdb, repo)) < 0 ||
	    (error = git_refdb_resolve(ref_out, refdb, normalized, max_nesting)) < 0)
		return error;

	/*
	 * The resolved reference may be a symbolic reference in case its
	 * target doesn't exist. If the user asked us to resolve (e.g.
	 * `max_nesting != 0`), then we need to return an error in case we got
	 * a symbolic reference back.
	 */
	if (max_nesting && git_reference_type(*ref_out) == GIT_REFERENCE_SYMBOLIC) {
		git_reference_free(*ref_out);
		*ref_out = NULL;
		return GIT_ENOTFOUND;
	}

	return 0;
}

int git_reference_dwim(git_reference **out, git_repository *repo, const char *refname)
{
	int error = 0, i, valid;
	bool fallbackmode = true, foundvalid = false;
	git_reference *ref;
	git_str refnamebuf = GIT_STR_INIT, name = GIT_STR_INIT;

	static const char *formatters[] = {
		"%s",
		GIT_REFS_DIR "%s",
		GIT_REFS_TAGS_DIR "%s",
		GIT_REFS_HEADS_DIR "%s",
		GIT_REFS_REMOTES_DIR "%s",
		GIT_REFS_REMOTES_DIR "%s/" GIT_HEAD_FILE,
		NULL
	};

	if (*refname)
		git_str_puts(&name, refname);
	else {
		git_str_puts(&name, GIT_HEAD_FILE);
		fallbackmode = false;
	}

	for (i = 0; formatters[i] && (fallbackmode || i == 0); i++) {

		git_str_clear(&refnamebuf);

		if ((error = git_str_printf(&refnamebuf, formatters[i], git_str_cstr(&name))) < 0 ||
		    (error = git_reference_name_is_valid(&valid, git_str_cstr(&refnamebuf))) < 0)
			goto cleanup;

		if (!valid) {
			error = GIT_EINVALIDSPEC;
			continue;
		}
		foundvalid = true;

		error = git_reference_lookup_resolved(&ref, repo, git_str_cstr(&refnamebuf), -1);

		if (!error) {
			*out = ref;
			error = 0;
			goto cleanup;
		}

		if (error != GIT_ENOTFOUND)
			goto cleanup;
	}

cleanup:
	if (error && !foundvalid) {
		/* never found a valid reference name */
		git_error_set(GIT_ERROR_REFERENCE,
			"could not use '%s' as valid reference name", git_str_cstr(&name));
	}

	if (error == GIT_ENOTFOUND)
		git_error_set(GIT_ERROR_REFERENCE, "no reference found for shorthand '%s'", refname);

	git_str_dispose(&name);
	git_str_dispose(&refnamebuf);
	return error;
}

/**
 * Getters
 */
git_reference_t git_reference_type(const git_reference *ref)
{
	GIT_ASSERT_ARG(ref);
	return ref->type;
}

const char *git_reference_name(const git_reference *ref)
{
	GIT_ASSERT_ARG_WITH_RETVAL(ref, NULL);
	return ref->name;
}

git_repository *git_reference_owner(const git_reference *ref)
{
	GIT_ASSERT_ARG_WITH_RETVAL(ref, NULL);
	return ref->db->repo;
}

const git_oid *git_reference_target(const git_reference *ref)
{
	GIT_ASSERT_ARG_WITH_RETVAL(ref, NULL);

	if (ref->type != GIT_REFERENCE_DIRECT)
		return NULL;

	return &ref->target.oid;
}

const git_oid *git_reference_target_peel(const git_reference *ref)
{
	GIT_ASSERT_ARG_WITH_RETVAL(ref, NULL);

	if (ref->type != GIT_REFERENCE_DIRECT || git_oid_is_zero(&ref->peel))
		return NULL;

	return &ref->peel;
}

const char *git_reference_symbolic_target(const git_reference *ref)
{
	GIT_ASSERT_ARG_WITH_RETVAL(ref, NULL);

	if (ref->type != GIT_REFERENCE_SYMBOLIC)
		return NULL;

	return ref->target.symbolic;
}

static int reference__create(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const git_oid *oid,
	const char *symbolic,
	int force,
	const git_signature *signature,
	const char *log_message,
	const git_oid *old_id,
	const char *old_target)
{
	git_refname_t normalized;
	git_refdb *refdb;
	git_reference *ref = NULL;
	int error = 0;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(name);
	GIT_ASSERT_ARG(symbolic || signature);

	if (ref_out)
		*ref_out = NULL;

	error = reference_normalize_for_repo(normalized, repo, name, true);
	if (error < 0)
		return error;

	error = git_repository_refdb__weakptr(&refdb, repo);
	if (error < 0)
		return error;

	if (oid != NULL) {
		GIT_ASSERT(symbolic == NULL);

		if (!git_object__is_valid(repo, oid, GIT_OBJECT_ANY)) {
			git_error_set(GIT_ERROR_REFERENCE,
				"target OID for the reference doesn't exist on the repository");
			return -1;
		}

		ref = git_reference__alloc(normalized, oid, NULL);
	} else {
		git_refname_t normalized_target;

		error = reference_normalize_for_repo(normalized_target, repo,
			symbolic, git_reference__enable_symbolic_ref_target_validation);

		if (error < 0)
			return error;

		ref = git_reference__alloc_symbolic(normalized, normalized_target);
	}

	GIT_ERROR_CHECK_ALLOC(ref);

	if ((error = git_refdb_write(refdb, ref, force, signature, log_message, old_id, old_target)) < 0) {
		git_reference_free(ref);
		return error;
	}

	if (ref_out == NULL)
		git_reference_free(ref);
	else
		*ref_out = ref;

	return 0;
}

static int refs_configured_ident(git_signature **out, const git_repository *repo)
{
	if (repo->ident_name && repo->ident_email)
		return git_signature_now(out, repo->ident_name, repo->ident_email);

	/* if not configured let us fall-through to the next method  */
	return -1;
}

int git_reference__log_signature(git_signature **out, git_repository *repo)
{
	int error;
	git_signature *who;

	if(((error = refs_configured_ident(&who, repo)) < 0) &&
	   ((error = git_signature_default(&who, repo)) < 0) &&
	   ((error = git_signature_now(&who, "unknown", "unknown")) < 0))
		return error;

	*out = who;
	return 0;
}

int git_reference_create_matching(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const git_oid *id,
	int force,
	const git_oid *old_id,
	const char *log_message)

{
	int error;
	git_signature *who = NULL;

	GIT_ASSERT_ARG(id);

	if ((error = git_reference__log_signature(&who, repo)) < 0)
		return error;

	error = reference__create(
		ref_out, repo, name, id, NULL, force, who, log_message, old_id, NULL);

	git_signature_free(who);
	return error;
}

int git_reference_create(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const git_oid *id,
	int force,
	const char *log_message)
{
        return git_reference_create_matching(ref_out, repo, name, id, force, NULL, log_message);
}

int git_reference_symbolic_create_matching(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const char *target,
	int force,
	const char *old_target,
	const char *log_message)
{
	int error;
	git_signature *who = NULL;

	GIT_ASSERT_ARG(target);

	if ((error = git_reference__log_signature(&who, repo)) < 0)
		return error;

	error = reference__create(
		ref_out, repo, name, NULL, target, force, who, log_message, NULL, old_target);

	git_signature_free(who);
	return error;
}

int git_reference_symbolic_create(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const char *target,
	int force,
	const char *log_message)
{
	return git_reference_symbolic_create_matching(ref_out, repo, name, target, force, NULL, log_message);
}

static int ensure_is_an_updatable_direct_reference(git_reference *ref)
{
	if (ref->type == GIT_REFERENCE_DIRECT)
		return 0;

	git_error_set(GIT_ERROR_REFERENCE, "cannot set OID on symbolic reference");
	return -1;
}

int git_reference_set_target(
	git_reference **out,
	git_reference *ref,
	const git_oid *id,
	const char *log_message)
{
	int error;
	git_repository *repo;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(ref);
	GIT_ASSERT_ARG(id);

	repo = ref->db->repo;

	if ((error = ensure_is_an_updatable_direct_reference(ref)) < 0)
		return error;

	return git_reference_create_matching(out, repo, ref->name, id, 1, &ref->target.oid, log_message);
}

static int ensure_is_an_updatable_symbolic_reference(git_reference *ref)
{
	if (ref->type == GIT_REFERENCE_SYMBOLIC)
		return 0;

	git_error_set(GIT_ERROR_REFERENCE, "cannot set symbolic target on a direct reference");
	return -1;
}

int git_reference_symbolic_set_target(
	git_reference **out,
	git_reference *ref,
	const char *target,
	const char *log_message)
{
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(ref);
	GIT_ASSERT_ARG(target);

	if ((error = ensure_is_an_updatable_symbolic_reference(ref)) < 0)
		return error;

	return git_reference_symbolic_create_matching(
		out, ref->db->repo, ref->name, target, 1, ref->target.symbolic, log_message);
}

typedef struct {
    const char *old_name;
    git_refname_t new_name;
} refs_update_head_payload;

static int refs_update_head(git_repository *worktree, void *_payload)
{
	refs_update_head_payload *payload = (refs_update_head_payload *)_payload;
	git_reference *head = NULL, *updated = NULL;
	int error;

	if ((error = git_reference_lookup(&head, worktree, GIT_HEAD_FILE)) < 0)
		goto out;

	if (git_reference_type(head) != GIT_REFERENCE_SYMBOLIC ||
	    git__strcmp(git_reference_symbolic_target(head), payload->old_name) != 0)
		goto out;

	/* Update HEAD if it was pointing to the reference being renamed */
	if ((error = git_reference_symbolic_set_target(&updated, head, payload->new_name, NULL)) < 0) {
		git_error_set(GIT_ERROR_REFERENCE, "failed to update HEAD after renaming reference");
		goto out;
	}

out:
	git_reference_free(updated);
	git_reference_free(head);
	return error;
}

int git_reference_rename(
	git_reference **out,
	git_reference *ref,
	const char *new_name,
	int force,
	const char *log_message)
{
	refs_update_head_payload payload;
	git_signature *signature = NULL;
	git_repository *repo;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(ref);

	repo = git_reference_owner(ref);

	if ((error = git_reference__log_signature(&signature, repo)) < 0 ||
	    (error = reference_normalize_for_repo(payload.new_name, repo, new_name, true)) < 0 ||
	    (error = git_refdb_rename(out, ref->db, ref->name, payload.new_name, force, signature, log_message)) < 0)
		goto out;

	payload.old_name = ref->name;

	/* We may have to update any HEAD that was pointing to the renamed reference. */
	if ((error = git_repository_foreach_worktree(repo, refs_update_head, &payload)) < 0)
		goto out;

out:
	git_signature_free(signature);
	return error;
}

int git_reference_resolve(git_reference **ref_out, const git_reference *ref)
{
	switch (git_reference_type(ref)) {
	case GIT_REFERENCE_DIRECT:
		return git_reference_lookup(ref_out, ref->db->repo, ref->name);

	case GIT_REFERENCE_SYMBOLIC:
		return git_reference_lookup_resolved(ref_out, ref->db->repo, ref->target.symbolic, -1);

	default:
		git_error_set(GIT_ERROR_REFERENCE, "invalid reference");
		return -1;
	}
}

int git_reference_foreach(
	git_repository *repo,
	git_reference_foreach_cb callback,
	void *payload)
{
	git_reference_iterator *iter;
	git_reference *ref;
	int error;

	if ((error = git_reference_iterator_new(&iter, repo)) < 0)
		return error;

	while (!(error = git_reference_next(&ref, iter))) {
		if ((error = callback(ref, payload)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_reference_iterator_free(iter);
	return error;
}

int git_reference_foreach_name(
	git_repository *repo,
	git_reference_foreach_name_cb callback,
	void *payload)
{
	git_reference_iterator *iter;
	const char *refname;
	int error;

	if ((error = git_reference_iterator_new(&iter, repo)) < 0)
		return error;

	while (!(error = git_reference_next_name(&refname, iter))) {
		if ((error = callback(refname, payload)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_reference_iterator_free(iter);
	return error;
}

int git_reference_foreach_glob(
	git_repository *repo,
	const char *glob,
	git_reference_foreach_name_cb callback,
	void *payload)
{
	git_reference_iterator *iter;
	const char *refname;
	int error;

	if ((error = git_reference_iterator_glob_new(&iter, repo, glob)) < 0)
		return error;

	while (!(error = git_reference_next_name(&refname, iter))) {
		if ((error = callback(refname, payload)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_reference_iterator_free(iter);
	return error;
}

int git_reference_iterator_new(git_reference_iterator **out, git_repository *repo)
{
	git_refdb *refdb;

	if (git_repository_refdb__weakptr(&refdb, repo) < 0)
		return -1;

	return git_refdb_iterator(out, refdb, NULL);
}

int git_reference_iterator_glob_new(
	git_reference_iterator **out, git_repository *repo, const char *glob)
{
	git_refdb *refdb;

	if (git_repository_refdb__weakptr(&refdb, repo) < 0)
		return -1;

	return git_refdb_iterator(out, refdb, glob);
}

int git_reference_next(git_reference **out, git_reference_iterator *iter)
{
	return git_refdb_iterator_next(out, iter);
}

int git_reference_next_name(const char **out, git_reference_iterator *iter)
{
	return git_refdb_iterator_next_name(out, iter);
}

void git_reference_iterator_free(git_reference_iterator *iter)
{
	if (iter == NULL)
		return;

	git_refdb_iterator_free(iter);
}

static int cb__reflist_add(const char *ref, void *data)
{
	char *name = git__strdup(ref);
	GIT_ERROR_CHECK_ALLOC(name);
	return git_vector_insert((git_vector *)data, name);
}

int git_reference_list(
	git_strarray *array,
	git_repository *repo)
{
	git_vector ref_list;

	GIT_ASSERT_ARG(array);
	GIT_ASSERT_ARG(repo);

	array->strings = NULL;
	array->count = 0;

	if (git_vector_init(&ref_list, 8, NULL) < 0)
		return -1;

	if (git_reference_foreach_name(
			repo, &cb__reflist_add, (void *)&ref_list) < 0) {
		git_vector_dispose(&ref_list);
		return -1;
	}

	array->strings = (char **)git_vector_detach(&array->count, NULL, &ref_list);

	return 0;
}

static int is_valid_ref_char(char ch)
{
	if ((unsigned) ch <= ' ')
		return 0;

	switch (ch) {
	case '~':
	case '^':
	case ':':
	case '\\':
	case '?':
	case '[':
		return 0;
	default:
		return 1;
	}
}

static int ensure_segment_validity(const char *name, char may_contain_glob, bool allow_caret_prefix)
{
	const char *current = name;
	const char *start = current;
	char prev = '\0';
	const int lock_len = (int)strlen(GIT_FILELOCK_EXTENSION);
	int segment_len;

	if (*current == '.')
		return -1; /* Refname starts with "." */
	if (allow_caret_prefix && *current == '^')
		start++;

	for (current = start; ; current++) {
		if (*current == '\0' || *current == '/')
			break;

		if (!is_valid_ref_char(*current))
			return -1; /* Illegal character in refname */

		if (prev == '.' && *current == '.')
			return -1; /* Refname contains ".." */

		if (prev == '@' && *current == '{')
			return -1; /* Refname contains "@{" */

		if (*current == '*') {
			if (!may_contain_glob)
				return -1;
			may_contain_glob = 0;
		}

		prev = *current;
	}

	segment_len = (int)(current - name);

	/* A refname component can not end with ".lock" */
	if (segment_len >= lock_len &&
		!memcmp(current - lock_len, GIT_FILELOCK_EXTENSION, lock_len))
			return -1;

	return segment_len;
}

static bool is_valid_normalized_name(const char *name, size_t len)
{
	size_t i;
	char c;

	GIT_ASSERT_ARG(name);
	GIT_ASSERT_ARG(len > 0);

	for (i = 0; i < len; i++)
	{
		c = name[i];
		if (i == 0 && c == '^')
			continue; /* The first character is allowed to be "^" for negative refspecs */

		if ((c < 'A' || c > 'Z') && c != '_')
			return false;
	}

	if (*name == '_' || name[len - 1] == '_')
		return false;

	return true;
}

/* Inspired from https://github.com/git/git/blob/f06d47e7e0d9db709ee204ed13a8a7486149f494/refs.c#L36-100 */
int git_reference__normalize_name(
	git_str *buf,
	const char *name,
	unsigned int flags)
{
	const char *current;
	int segment_len, segments_count = 0, error = GIT_EINVALIDSPEC;
	unsigned int process_flags;
	bool normalize = (buf != NULL);
	bool allow_caret_prefix = true;
	bool validate = (flags & GIT_REFERENCE_FORMAT__VALIDATION_DISABLE) == 0;

#ifdef GIT_USE_ICONV
	git_fs_path_iconv_t ic = GIT_PATH_ICONV_INIT;
#endif

	GIT_ASSERT_ARG(name);

	process_flags = flags;
	current = (char *)name;

	if (validate && *current == '/')
		goto cleanup;

	if (normalize)
		git_str_clear(buf);

#ifdef GIT_USE_ICONV
	if ((flags & GIT_REFERENCE_FORMAT__PRECOMPOSE_UNICODE) != 0) {
		size_t namelen = strlen(current);
		if ((error = git_fs_path_iconv_init_precompose(&ic)) < 0 ||
			(error = git_fs_path_iconv(&ic, &current, &namelen)) < 0)
			goto cleanup;
		error = GIT_EINVALIDSPEC;
	}
#endif

	if (!validate) {
		git_str_sets(buf, current);

		error = git_str_oom(buf) ? -1 : 0;
		goto cleanup;
	}

	while (true) {
		char may_contain_glob = process_flags & GIT_REFERENCE_FORMAT_REFSPEC_PATTERN;

		segment_len = ensure_segment_validity(current, may_contain_glob, allow_caret_prefix);
		if (segment_len < 0)
			goto cleanup;

		if (segment_len > 0) {
			/*
			 * There may only be one glob in a pattern, thus we reset
			 * the pattern-flag in case the current segment has one.
			 */
			if (memchr(current, '*', segment_len))
				process_flags &= ~GIT_REFERENCE_FORMAT_REFSPEC_PATTERN;

			if (normalize) {
				size_t cur_len = git_str_len(buf);

				git_str_joinpath(buf, git_str_cstr(buf), current);
				git_str_truncate(buf,
					cur_len + segment_len + (segments_count ? 1 : 0));

				if (git_str_oom(buf)) {
					error = -1;
					goto cleanup;
				}
			}

			segments_count++;
		}

		/* No empty segment is allowed when not normalizing */
		if (segment_len == 0 && !normalize)
			goto cleanup;

		if (current[segment_len] == '\0')
			break;

		current += segment_len + 1;

		/*
		 * A caret prefix is only allowed in the first segment to signify a
		 * negative refspec.
		 */
		allow_caret_prefix = false;
	}

	/* A refname can not be empty */
	if (segment_len == 0 && segments_count == 0)
		goto cleanup;

	/* A refname can not end with "." */
	if (current[segment_len - 1] == '.')
		goto cleanup;

	/* A refname can not end with "/" */
	if (current[segment_len - 1] == '/')
		goto cleanup;

	if ((segments_count == 1 ) && !(flags & GIT_REFERENCE_FORMAT_ALLOW_ONELEVEL))
		goto cleanup;

	if ((segments_count == 1 ) &&
	    !(flags & GIT_REFERENCE_FORMAT_REFSPEC_SHORTHAND) &&
		!(is_valid_normalized_name(name, (size_t)segment_len) ||
			((flags & GIT_REFERENCE_FORMAT_REFSPEC_PATTERN) && !strcmp("*", name))))
			goto cleanup;

	if ((segments_count > 1)
		&& (is_valid_normalized_name(name, strchr(name, '/') - name)))
			goto cleanup;

	error = 0;

cleanup:
	if (error == GIT_EINVALIDSPEC)
		git_error_set(
			GIT_ERROR_REFERENCE,
			"the given reference name '%s' is not valid", name);

	if (error && normalize)
		git_str_dispose(buf);

#ifdef GIT_USE_ICONV
	git_fs_path_iconv_clear(&ic);
#endif

	return error;
}

int git_reference_normalize_name(
	char *buffer_out,
	size_t buffer_size,
	const char *name,
	unsigned int flags)
{
	git_str buf = GIT_STR_INIT;
	int error;

	if ((error = git_reference__normalize_name(&buf, name, flags)) < 0)
		goto cleanup;

	if (git_str_len(&buf) > buffer_size - 1) {
		git_error_set(
		GIT_ERROR_REFERENCE,
		"the provided buffer is too short to hold the normalization of '%s'", name);
		error = GIT_EBUFS;
		goto cleanup;
	}

	if ((error = git_str_copy_cstr(buffer_out, buffer_size, &buf)) < 0)
		goto cleanup;

	error = 0;

cleanup:
	git_str_dispose(&buf);
	return error;
}

#define GIT_REFERENCE_TYPEMASK (GIT_REFERENCE_DIRECT | GIT_REFERENCE_SYMBOLIC)

int git_reference_cmp(
	const git_reference *ref1,
	const git_reference *ref2)
{
	git_reference_t type1, type2;

	GIT_ASSERT_ARG(ref1);
	GIT_ASSERT_ARG(ref2);

	type1 = git_reference_type(ref1);
	type2 = git_reference_type(ref2);

	/* let's put symbolic refs before OIDs */
	if (type1 != type2)
		return (type1 == GIT_REFERENCE_SYMBOLIC) ? -1 : 1;

	if (type1 == GIT_REFERENCE_SYMBOLIC)
		return strcmp(ref1->target.symbolic, ref2->target.symbolic);

	return git_oid__cmp(&ref1->target.oid, &ref2->target.oid);
}

int git_reference__cmp_cb(const void *a, const void *b)
{
	return git_reference_cmp(
		(const git_reference *)a, (const git_reference *)b);
}

/*
 * Starting with the reference given by `ref_name`, follows symbolic
 * references until a direct reference is found and updated the OID
 * on that direct reference to `oid`.
 */
int git_reference__update_terminal(
	git_repository *repo,
	const char *ref_name,
	const git_oid *oid,
	const git_signature *sig,
	const char *log_message)
{
	git_reference *ref = NULL, *ref2 = NULL;
	git_signature *who = NULL;
	git_refdb *refdb = NULL;
	const git_signature *to_use;
	int error = 0;

	if (!sig && (error = git_reference__log_signature(&who, repo)) < 0)
		goto out;

	to_use = sig ? sig : who;

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		goto out;

	if ((error = git_refdb_resolve(&ref, refdb, ref_name, -1)) < 0) {
		if (error == GIT_ENOTFOUND) {
			git_error_clear();
			error = reference__create(&ref2, repo, ref_name, oid, NULL, 0, to_use,
						  log_message, NULL, NULL);
		}
		goto out;
	}

	/* In case the resolved reference is symbolic, then it's a dangling symref. */
	if (git_reference_type(ref) == GIT_REFERENCE_SYMBOLIC) {
		error = reference__create(&ref2, repo, ref->target.symbolic, oid, NULL, 0, to_use,
					  log_message, NULL, NULL);
	} else {
		error = reference__create(&ref2, repo, ref->name, oid, NULL, 1, to_use,
					  log_message, &ref->target.oid, NULL);
	}

out:
	git_reference_free(ref2);
	git_reference_free(ref);
	git_signature_free(who);
	return error;
}

static const char *commit_type(const git_commit *commit)
{
	unsigned int count = git_commit_parentcount(commit);

	if (count >= 2)
		return " (merge)";
	else if (count == 0)
		return " (initial)";
	else
		return "";
}

int git_reference__update_for_commit(
	git_repository *repo,
	git_reference *ref,
	const char *ref_name,
	const git_oid *id,
	const char *operation)
{
	git_reference *ref_new = NULL;
	git_commit *commit = NULL;
	git_str reflog_msg = GIT_STR_INIT;
	const git_signature *who;
	int error;

	if ((error = git_commit_lookup(&commit, repo, id)) < 0 ||
		(error = git_str_printf(&reflog_msg, "%s%s: %s",
			operation ? operation : "commit",
			commit_type(commit),
			git_commit_summary(commit))) < 0)
		goto done;

	who = git_commit_committer(commit);

	if (ref) {
		if ((error = ensure_is_an_updatable_direct_reference(ref)) < 0)
			return error;

		error = reference__create(&ref_new, repo, ref->name, id, NULL, 1, who,
					  git_str_cstr(&reflog_msg), &ref->target.oid, NULL);
	}
	else
		error = git_reference__update_terminal(
			repo, ref_name, id, who, git_str_cstr(&reflog_msg));

done:
	git_reference_free(ref_new);
	git_str_dispose(&reflog_msg);
	git_commit_free(commit);
	return error;
}

int git_reference_has_log(git_repository *repo, const char *refname)
{
	int error;
	git_refdb *refdb;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(refname);

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return error;

	return git_refdb_has_log(refdb, refname);
}

int git_reference_ensure_log(git_repository *repo, const char *refname)
{
	int error;
	git_refdb *refdb;

	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(refname);

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return error;

	return git_refdb_ensure_log(refdb, refname);
}

int git_reference__is_branch(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_HEADS_DIR) == 0;
}

int git_reference_is_branch(const git_reference *ref)
{
	GIT_ASSERT_ARG(ref);
	return git_reference__is_branch(ref->name);
}

int git_reference__is_remote(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_REMOTES_DIR) == 0;
}

int git_reference_is_remote(const git_reference *ref)
{
	GIT_ASSERT_ARG(ref);
	return git_reference__is_remote(ref->name);
}

int git_reference__is_tag(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_TAGS_DIR) == 0;
}

int git_reference_is_tag(const git_reference *ref)
{
	GIT_ASSERT_ARG(ref);
	return git_reference__is_tag(ref->name);
}

int git_reference__is_note(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_NOTES_DIR) == 0;
}

int git_reference_is_note(const git_reference *ref)
{
	GIT_ASSERT_ARG(ref);
	return git_reference__is_note(ref->name);
}

static int peel_error(int error, const git_reference *ref, const char *msg)
{
	git_error_set(
		GIT_ERROR_INVALID,
		"the reference '%s' cannot be peeled - %s", git_reference_name(ref), msg);
	return error;
}

int git_reference_peel(
	git_object **peeled,
	const git_reference *ref,
	git_object_t target_type)
{
	const git_reference *resolved = NULL;
	git_reference *allocated = NULL;
	git_object *target = NULL;
	int error;

	GIT_ASSERT_ARG(ref);

	if (ref->type == GIT_REFERENCE_DIRECT) {
		resolved = ref;
	} else {
		if ((error = git_reference_resolve(&allocated, ref)) < 0)
			return peel_error(error, ref, "Cannot resolve reference");

		resolved = allocated;
	}

	/*
	 * If we try to peel an object to a tag, we cannot use
	 * the fully peeled object, as that will always resolve
	 * to a commit. So we only want to use the peeled value
	 * if it is not zero and the target is not a tag.
	 */
	if (target_type != GIT_OBJECT_TAG && !git_oid_is_zero(&resolved->peel)) {
		error = git_object_lookup(&target,
			git_reference_owner(ref), &resolved->peel, GIT_OBJECT_ANY);
	} else {
		error = git_object_lookup(&target,
			git_reference_owner(ref), &resolved->target.oid, GIT_OBJECT_ANY);
	}

	if (error < 0) {
		peel_error(error, ref, "Cannot retrieve reference target");
		goto cleanup;
	}

	if (target_type == GIT_OBJECT_ANY && git_object_type(target) != GIT_OBJECT_TAG)
		error = git_object_dup(peeled, target);
	else
		error = git_object_peel(peeled, target, target_type);

cleanup:
	git_object_free(target);
	git_reference_free(allocated);

	return error;
}

int git_reference__name_is_valid(
	int *valid,
	const char *refname,
	unsigned int flags)
{
	int error;

	GIT_ASSERT(valid && refname);

	*valid = 0;

	error = git_reference__normalize_name(NULL, refname, flags);

	if (!error)
		*valid = 1;
	else if (error == GIT_EINVALIDSPEC)
		error = 0;

	return error;
}

int git_reference_name_is_valid(int *valid, const char *refname)
{
	return git_reference__name_is_valid(valid, refname, GIT_REFERENCE_FORMAT_ALLOW_ONELEVEL);
}

const char *git_reference__shorthand(const char *name)
{
	if (!git__prefixcmp(name, GIT_REFS_HEADS_DIR))
		return name + strlen(GIT_REFS_HEADS_DIR);
	else if (!git__prefixcmp(name, GIT_REFS_TAGS_DIR))
		return name + strlen(GIT_REFS_TAGS_DIR);
	else if (!git__prefixcmp(name, GIT_REFS_REMOTES_DIR))
		return name + strlen(GIT_REFS_REMOTES_DIR);
	else if (!git__prefixcmp(name, GIT_REFS_DIR))
		return name + strlen(GIT_REFS_DIR);

	/* No shorthands are available, so just return the name. */
	return name;
}

const char *git_reference_shorthand(const git_reference *ref)
{
	return git_reference__shorthand(ref->name);
}

int git_reference__is_unborn_head(bool *unborn, const git_reference *ref, git_repository *repo)
{
	int error;
	git_reference *tmp_ref;

	GIT_ASSERT_ARG(unborn);
	GIT_ASSERT_ARG(ref);
	GIT_ASSERT_ARG(repo);

	if (ref->type == GIT_REFERENCE_DIRECT) {
		*unborn = 0;
		return 0;
	}

	error = git_reference_lookup_resolved(&tmp_ref, repo, ref->name, -1);
	git_reference_free(tmp_ref);

	if (error != 0 && error != GIT_ENOTFOUND)
		return error;
	else if (error == GIT_ENOTFOUND && git__strcmp(ref->name, GIT_HEAD_FILE) == 0)
		*unborn = true;
	else
		*unborn = false;

	return 0;
}

/* Deprecated functions */

#ifndef GIT_DEPRECATE_HARD

int git_reference_is_valid_name(const char *refname)
{
	int valid = 0;

	git_reference__name_is_valid(&valid, refname, GIT_REFERENCE_FORMAT_ALLOW_ONELEVEL);

	return valid;
}

#endif
