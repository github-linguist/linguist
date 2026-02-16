/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "notes.h"

#include "buf.h"
#include "refs.h"
#include "config.h"
#include "iterator.h"
#include "signature.h"
#include "blob.h"

static int note_error_notfound(void)
{
	git_error_set(GIT_ERROR_INVALID, "note could not be found");
	return GIT_ENOTFOUND;
}

static int find_subtree_in_current_level(
	git_tree **out,
	git_repository *repo,
	git_tree *parent,
	const char *annotated_object_sha,
	int fanout)
{
	size_t i;
	const git_tree_entry *entry;

	*out = NULL;

	if (parent == NULL)
		return note_error_notfound();

	for (i = 0; i < git_tree_entrycount(parent); i++) {
		entry = git_tree_entry_byindex(parent, i);

		if (!git__ishex(git_tree_entry_name(entry)))
			continue;

		if (S_ISDIR(git_tree_entry_filemode(entry))
			&& strlen(git_tree_entry_name(entry)) == 2
			&& !strncmp(git_tree_entry_name(entry), annotated_object_sha + fanout, 2))
			return git_tree_lookup(out, repo, git_tree_entry_id(entry));

		/* Not a DIR, so do we have an already existing blob? */
		if (!strcmp(git_tree_entry_name(entry), annotated_object_sha + fanout))
			return GIT_EEXISTS;
	}

	return note_error_notfound();
}

static int find_subtree_r(git_tree **out, git_tree *root,
			git_repository *repo, const char *target, int *fanout)
{
	int error;
	git_tree *subtree = NULL;

	*out = NULL;

	error = find_subtree_in_current_level(&subtree, repo, root, target, *fanout);
	if (error == GIT_EEXISTS)
		return git_tree_lookup(out, repo, git_tree_id(root));

	if (error < 0)
		return error;

	*fanout += 2;
	error = find_subtree_r(out, subtree, repo, target, fanout);
	git_tree_free(subtree);

	return error;
}

static int find_blob(git_oid *blob, git_tree *tree, const char *target)
{
	size_t i;
	const git_tree_entry *entry;

	for (i=0; i<git_tree_entrycount(tree); i++) {
		entry = git_tree_entry_byindex(tree, i);

		if (!strcmp(git_tree_entry_name(entry), target)) {
			/* found matching note object - return */

			git_oid_cpy(blob, git_tree_entry_id(entry));
			return 0;
		}
	}

	return note_error_notfound();
}

static int tree_write(
	git_tree **out,
	git_repository *repo,
	git_tree *source_tree,
	const git_oid *object_oid,
	const char *treeentry_name,
	unsigned int attributes)
{
	int error;
	git_treebuilder *tb = NULL;
	const git_tree_entry *entry;
	git_oid tree_oid;

	if ((error = git_treebuilder_new(&tb, repo, source_tree)) < 0)
		goto cleanup;

	if (object_oid) {
		if ((error = git_treebuilder_insert(
				&entry, tb, treeentry_name, object_oid, attributes)) < 0)
			goto cleanup;
	} else {
		if ((error = git_treebuilder_remove(tb, treeentry_name)) < 0)
			goto cleanup;
	}

	if ((error = git_treebuilder_write(&tree_oid, tb)) < 0)
		goto cleanup;

	error = git_tree_lookup(out, repo, &tree_oid);

cleanup:
	git_treebuilder_free(tb);
	return error;
}

static int manipulate_note_in_tree_r(
	git_tree **out,
	git_repository *repo,
	git_tree *parent,
	git_oid *note_oid,
	const char *annotated_object_sha,
	int fanout,
	int (*note_exists_cb)(
		git_tree **out,
		git_repository *repo,
		git_tree *parent,
		git_oid *note_oid,
		const char *annotated_object_sha,
		int fanout,
		int current_error),
	int (*note_notfound_cb)(
		git_tree **out,
		git_repository *repo,
		git_tree *parent,
		git_oid *note_oid,
		const char *annotated_object_sha,
		int fanout,
		int current_error))
{
	int error;
	git_tree *subtree = NULL, *new = NULL;
	char subtree_name[3];

	error = find_subtree_in_current_level(
		&subtree, repo, parent, annotated_object_sha, fanout);

	if (error == GIT_EEXISTS) {
		error = note_exists_cb(
			out, repo, parent, note_oid, annotated_object_sha, fanout, error);
		goto cleanup;
	}

	if (error == GIT_ENOTFOUND) {
		error = note_notfound_cb(
			out, repo, parent, note_oid, annotated_object_sha, fanout, error);
		goto cleanup;
	}

	if (error < 0)
		goto cleanup;

	/* An existing fanout has been found, let's dig deeper */
	error = manipulate_note_in_tree_r(
		&new, repo, subtree, note_oid, annotated_object_sha,
		fanout + 2, note_exists_cb, note_notfound_cb);

	if (error < 0)
		goto cleanup;

	strncpy(subtree_name, annotated_object_sha + fanout, 2);
	subtree_name[2] = '\0';

	error = tree_write(out, repo, parent, git_tree_id(new),
			   subtree_name, GIT_FILEMODE_TREE);


cleanup:
	git_tree_free(new);
	git_tree_free(subtree);
	return error;
}

static int remove_note_in_tree_eexists_cb(
	git_tree **out,
	git_repository *repo,
	git_tree *parent,
	git_oid *note_oid,
	const char *annotated_object_sha,
	int fanout,
	int current_error)
{
	GIT_UNUSED(note_oid);
	GIT_UNUSED(current_error);

	return tree_write(out, repo, parent, NULL, annotated_object_sha + fanout, 0);
}

static int remove_note_in_tree_enotfound_cb(
	git_tree **out,
	git_repository *repo,
	git_tree *parent,
	git_oid *note_oid,
	const char *annotated_object_sha,
	int fanout,
	int current_error)
{
	GIT_UNUSED(out);
	GIT_UNUSED(repo);
	GIT_UNUSED(parent);
	GIT_UNUSED(note_oid);
	GIT_UNUSED(fanout);

	git_error_set(GIT_ERROR_REPOSITORY, "object '%s' has no note", annotated_object_sha);
	return current_error;
}

static int insert_note_in_tree_eexists_cb(git_tree **out,
	git_repository *repo,
	git_tree *parent,
	git_oid *note_oid,
	const char *annotated_object_sha,
	int fanout,
	int current_error)
{
	GIT_UNUSED(out);
	GIT_UNUSED(repo);
	GIT_UNUSED(parent);
	GIT_UNUSED(note_oid);
	GIT_UNUSED(fanout);

	git_error_set(GIT_ERROR_REPOSITORY, "note for '%s' exists already", annotated_object_sha);
	return current_error;
}

static int insert_note_in_tree_enotfound_cb(git_tree **out,
	git_repository *repo,
	git_tree *parent,
	git_oid *note_oid,
	const char *annotated_object_sha,
	int fanout,
	int current_error)
{
	GIT_UNUSED(current_error);

	/* No existing fanout at this level, insert in place */
	return tree_write(
		out,
		repo,
		parent,
		note_oid,
		annotated_object_sha + fanout,
		GIT_FILEMODE_BLOB);
}

static int note_write(
	git_oid *notes_commit_out,
	git_oid *notes_blob_out,
	git_repository *repo,
	const git_signature *author,
	const git_signature *committer,
	const char *notes_ref,
	const char *note,
	git_tree *commit_tree,
	const char *target,
	git_commit **parents,
	int allow_note_overwrite)
{
	int error;
	git_oid oid;
	git_tree *tree = NULL;

	/* TODO: should we apply filters? */
	/* create note object */
	if ((error = git_blob_create_from_buffer(&oid, repo, note, strlen(note))) < 0)
		goto cleanup;

	if ((error = manipulate_note_in_tree_r(
		&tree, repo, commit_tree, &oid, target, 0,
		allow_note_overwrite ? insert_note_in_tree_enotfound_cb : insert_note_in_tree_eexists_cb,
		insert_note_in_tree_enotfound_cb)) < 0)
		goto cleanup;

	if (notes_blob_out)
		git_oid_cpy(notes_blob_out, &oid);


	error = git_commit_create(&oid, repo, notes_ref, author, committer,
				  NULL, GIT_NOTES_DEFAULT_MSG_ADD,
				  tree, *parents == NULL ? 0 : 1, (const git_commit **) parents);

	if (notes_commit_out)
		git_oid_cpy(notes_commit_out, &oid);

cleanup:
	git_tree_free(tree);
	return error;
}

static int note_new(
	git_note **out,
	git_oid *note_oid,
	git_commit *commit,
	git_blob *blob)
{
	git_note *note = NULL;
	git_object_size_t blobsize;

	note = git__malloc(sizeof(git_note));
	GIT_ERROR_CHECK_ALLOC(note);

	git_oid_cpy(&note->id, note_oid);

	if (git_signature_dup(&note->author, git_commit_author(commit)) < 0 ||
		git_signature_dup(&note->committer, git_commit_committer(commit)) < 0)
		return -1;

	blobsize = git_blob_rawsize(blob);
	GIT_ERROR_CHECK_BLOBSIZE(blobsize);

	note->message = git__strndup(git_blob_rawcontent(blob), (size_t)blobsize);
	GIT_ERROR_CHECK_ALLOC(note->message);

	*out = note;
	return 0;
}

static int note_lookup(
	git_note **out,
	git_repository *repo,
	git_commit *commit,
	git_tree *tree,
	const char *target)
{
	int error, fanout = 0;
	git_oid oid;
	git_blob *blob = NULL;
	git_note *note = NULL;
	git_tree *subtree = NULL;

	if ((error = find_subtree_r(&subtree, tree, repo, target, &fanout)) < 0)
		goto cleanup;

	if ((error = find_blob(&oid, subtree, target + fanout)) < 0)
		goto cleanup;

	if ((error = git_blob_lookup(&blob, repo, &oid)) < 0)
		goto cleanup;

	if ((error = note_new(&note, &oid, commit, blob)) < 0)
		goto cleanup;

	*out = note;

cleanup:
	git_tree_free(subtree);
	git_blob_free(blob);
	return error;
}

static int note_remove(
		git_oid *notes_commit_out,
		git_repository *repo,
		const git_signature *author, const git_signature *committer,
		const char *notes_ref, git_tree *tree,
		const char *target, git_commit **parents)
{
	int error;
	git_tree *tree_after_removal = NULL;
	git_oid oid;

	if ((error = manipulate_note_in_tree_r(
		&tree_after_removal, repo, tree, NULL, target, 0,
		remove_note_in_tree_eexists_cb, remove_note_in_tree_enotfound_cb)) < 0)
		goto cleanup;

	error = git_commit_create(&oid, repo, notes_ref, author, committer,
	  NULL, GIT_NOTES_DEFAULT_MSG_RM,
	  tree_after_removal,
	  *parents == NULL ? 0 : 1,
	  (const git_commit **) parents);

	if (error < 0)
		goto cleanup;

	if (notes_commit_out)
		git_oid_cpy(notes_commit_out, &oid);

cleanup:
	git_tree_free(tree_after_removal);
	return error;
}

static int note_get_default_ref(git_str *out, git_repository *repo)
{
	git_config *cfg;
	int error;

	if ((error = git_repository_config__weakptr(&cfg, repo)) < 0)
		return error;

	error = git_config__get_string_buf(out, cfg, "core.notesref");

	if (error == GIT_ENOTFOUND)
		error = git_str_puts(out, GIT_NOTES_DEFAULT_REF);

	return error;
}

static int normalize_namespace(git_str *out, git_repository *repo, const char *notes_ref)
{
	if (notes_ref)
		return git_str_puts(out, notes_ref);

	return note_get_default_ref(out, repo);
}

static int retrieve_note_commit(
	git_commit **commit_out,
	git_str *notes_ref_out,
	git_repository *repo,
	const char *notes_ref)
{
	int error;
	git_oid oid;

	if ((error = normalize_namespace(notes_ref_out, repo, notes_ref)) < 0)
		return error;

	if ((error = git_reference_name_to_id(&oid, repo, notes_ref_out->ptr)) < 0)
		return error;

	if (git_commit_lookup(commit_out, repo, &oid) < 0)
		return error;

	return 0;
}

int git_note_commit_read(
	git_note **out,
	git_repository *repo,
	git_commit *notes_commit,
	const git_oid *oid)
{
	int error;
	git_tree *tree = NULL;
	char target[GIT_OID_MAX_HEXSIZE + 1];

	git_oid_tostr(target, sizeof(target), oid);

	if ((error = git_commit_tree(&tree, notes_commit)) < 0)
		goto cleanup;

	error = note_lookup(out, repo, notes_commit, tree, target);

cleanup:
	git_tree_free(tree);
	return error;
}

int git_note_read(git_note **out, git_repository *repo,
		  const char *notes_ref_in, const git_oid *oid)
{
	int error;
	git_str notes_ref = GIT_STR_INIT;
	git_commit *commit = NULL;

	error = retrieve_note_commit(&commit, &notes_ref, repo, notes_ref_in);

	if (error < 0)
		goto cleanup;

	error = git_note_commit_read(out, repo, commit, oid);

cleanup:
	git_str_dispose(&notes_ref);
	git_commit_free(commit);
	return error;
}

int git_note_commit_create(
	git_oid *notes_commit_out,
	git_oid *notes_blob_out,
	git_repository *repo,
	git_commit *parent,
	const git_signature *author,
	const git_signature *committer,
	const git_oid *oid,
	const char *note,
	int allow_note_overwrite)
{
	int error;
	git_tree *tree = NULL;
	char target[GIT_OID_MAX_HEXSIZE + 1];

	git_oid_tostr(target, sizeof(target), oid);

	if (parent != NULL && (error = git_commit_tree(&tree, parent)) < 0)
		goto cleanup;

	error = note_write(notes_commit_out, notes_blob_out, repo, author,
			committer, NULL, note, tree, target, &parent, allow_note_overwrite);

	if (error < 0)
		goto cleanup;

cleanup:
	git_tree_free(tree);
	return error;
}

int git_note_create(
	git_oid *out,
	git_repository *repo,
	const char *notes_ref_in,
	const git_signature *author,
	const git_signature *committer,
	const git_oid *oid,
	const char *note,
	int allow_note_overwrite)
{
	int error;
	git_str notes_ref = GIT_STR_INIT;
	git_commit *existing_notes_commit = NULL;
	git_reference *ref = NULL;
	git_oid notes_blob_oid, notes_commit_oid;

	error = retrieve_note_commit(&existing_notes_commit, &notes_ref,
			repo, notes_ref_in);

	if (error < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	error = git_note_commit_create(&notes_commit_oid,
			&notes_blob_oid,
			repo, existing_notes_commit, author,
			committer, oid, note,
			allow_note_overwrite);
	if (error < 0)
		goto cleanup;

	error = git_reference_create(&ref, repo, notes_ref.ptr,
				&notes_commit_oid, 1, NULL);

	if (out != NULL)
		git_oid_cpy(out, &notes_blob_oid);

cleanup:
	git_str_dispose(&notes_ref);
	git_commit_free(existing_notes_commit);
	git_reference_free(ref);
	return error;
}

int git_note_commit_remove(
		git_oid *notes_commit_out,
		git_repository *repo,
		git_commit *notes_commit,
		const git_signature *author,
		const git_signature *committer,
		const git_oid *oid)
{
	int error;
	git_tree *tree = NULL;
	char target[GIT_OID_MAX_HEXSIZE + 1];

	git_oid_tostr(target, sizeof(target), oid);

	if ((error = git_commit_tree(&tree, notes_commit)) < 0)
		goto cleanup;

	error = note_remove(notes_commit_out,
		repo, author, committer, NULL, tree, target, &notes_commit);

cleanup:
	git_tree_free(tree);
	return error;
}

int git_note_remove(git_repository *repo, const char *notes_ref_in,
		const git_signature *author, const git_signature *committer,
		const git_oid *oid)
{
	int error;
	git_str notes_ref_target = GIT_STR_INIT;
	git_commit *existing_notes_commit = NULL;
	git_oid new_notes_commit;
	git_reference *notes_ref = NULL;

	error = retrieve_note_commit(&existing_notes_commit, &notes_ref_target,
			repo, notes_ref_in);

	if (error < 0)
		goto cleanup;

	error = git_note_commit_remove(&new_notes_commit, repo,
			existing_notes_commit, author, committer, oid);
	if (error < 0)
		goto cleanup;

	error = git_reference_create(&notes_ref, repo, notes_ref_target.ptr,
			&new_notes_commit, 1, NULL);

cleanup:
	git_str_dispose(&notes_ref_target);
	git_reference_free(notes_ref);
	git_commit_free(existing_notes_commit);
	return error;
}

int git_note_default_ref(git_buf *out, git_repository *repo)
{
	GIT_BUF_WRAP_PRIVATE(out, note_get_default_ref, repo);
}

const git_signature *git_note_committer(const git_note *note)
{
	GIT_ASSERT_ARG_WITH_RETVAL(note, NULL);
	return note->committer;
}

const git_signature *git_note_author(const git_note *note)
{
	GIT_ASSERT_ARG_WITH_RETVAL(note, NULL);
	return note->author;
}

const char *git_note_message(const git_note *note)
{
	GIT_ASSERT_ARG_WITH_RETVAL(note, NULL);
	return note->message;
}

const git_oid *git_note_id(const git_note *note)
{
	GIT_ASSERT_ARG_WITH_RETVAL(note, NULL);
	return &note->id;
}

void git_note_free(git_note *note)
{
	if (note == NULL)
		return;

	git_signature_free(note->committer);
	git_signature_free(note->author);
	git__free(note->message);
	git__free(note);
}

static int process_entry_path(
	git_oid *annotated_object_id,
	git_note_iterator *it,
	const char *entry_path)
{
	int error = 0;
	size_t i = 0, j = 0, len;
	git_str buf = GIT_STR_INIT;

	if ((error = git_str_puts(&buf, entry_path)) < 0)
		goto cleanup;

	len = git_str_len(&buf);

	while (i < len) {
		if (buf.ptr[i] == '/') {
			i++;
			continue;
		}

		if (git__fromhex(buf.ptr[i]) < 0) {
			/* This is not a note entry */
			goto cleanup;
		}

		if (i != j)
			buf.ptr[j] = buf.ptr[i];

		i++;
		j++;
	}

	buf.ptr[j] = '\0';
	buf.size = j;

	if (j != git_oid_hexsize(it->repo->oid_type)) {
		/* This is not a note entry */
		goto cleanup;
	}

	error = git_oid__fromstr(annotated_object_id, buf.ptr, it->repo->oid_type);

cleanup:
	git_str_dispose(&buf);
	return error;
}

int git_note_foreach(
	git_repository *repo,
	const char *notes_ref,
	git_note_foreach_cb note_cb,
	void *payload)
{
	int error;
	git_note_iterator *iter = NULL;
	git_oid note_id, annotated_id;

	if ((error = git_note_iterator_new(&iter, repo, notes_ref)) < 0)
		return error;

	while (!(error = git_note_next(&note_id, &annotated_id, iter))) {
		if ((error = note_cb(&note_id, &annotated_id, payload)) != 0) {
			git_error_set_after_callback(error);
			break;
		}
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_note_iterator_free(iter);
	return error;
}

void git_note_iterator_free(git_note_iterator *it)
{
	if (it == NULL)
		return;

	git_iterator_free(it);
}

int git_note_commit_iterator_new(
	git_note_iterator **it,
	git_commit *notes_commit)
{
	int error;
	git_tree *tree;

	if ((error = git_commit_tree(&tree, notes_commit)) < 0)
		goto cleanup;

	if ((error = git_iterator_for_tree(it, tree, NULL)) < 0)
		git_iterator_free(*it);

cleanup:
	git_tree_free(tree);

	return error;
}

int git_note_iterator_new(
	git_note_iterator **it,
	git_repository *repo,
	const char *notes_ref_in)
{
	int error;
	git_commit *commit = NULL;
	git_str notes_ref = GIT_STR_INIT;

	error = retrieve_note_commit(&commit, &notes_ref, repo, notes_ref_in);
	if (error < 0)
		goto cleanup;

	error = git_note_commit_iterator_new(it, commit);

cleanup:
	git_str_dispose(&notes_ref);
	git_commit_free(commit);

	return error;
}

int git_note_next(
	git_oid *note_id,
	git_oid *annotated_id,
	git_note_iterator *it)
{
	int error;
	const git_index_entry *item;

	if ((error = git_iterator_current(&item, it)) < 0)
		return error;

	git_oid_cpy(note_id, &item->id);

	if ((error = process_entry_path(annotated_id, it, item->path)) < 0)
		return error;

	if ((error = git_iterator_advance(NULL, it)) < 0 && error != GIT_ITEROVER)
		return error;

	return 0;
}
