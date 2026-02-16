/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "annotated_commit.h"

#include "refs.h"
#include "cache.h"

#include "git2/commit.h"
#include "git2/refs.h"
#include "git2/repository.h"
#include "git2/annotated_commit.h"
#include "git2/revparse.h"
#include "git2/tree.h"
#include "git2/index.h"

static int annotated_commit_init(
	git_annotated_commit **out,
	git_commit *commit,
	const char *description)
{
	git_annotated_commit *annotated_commit;
	int error = 0;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(commit);

	*out = NULL;

	annotated_commit = git__calloc(1, sizeof(git_annotated_commit));
	GIT_ERROR_CHECK_ALLOC(annotated_commit);

	annotated_commit->type = GIT_ANNOTATED_COMMIT_REAL;

	if ((error = git_commit_dup(&annotated_commit->commit, commit)) < 0)
		goto done;

	git_oid_tostr(annotated_commit->id_str, GIT_OID_MAX_HEXSIZE + 1,
		git_commit_id(commit));

	if (!description)
		description = annotated_commit->id_str;

	annotated_commit->description = git__strdup(description);
	GIT_ERROR_CHECK_ALLOC(annotated_commit->description);

done:
	if (!error)
		*out = annotated_commit;

	return error;
}

static int annotated_commit_init_from_id(
	git_annotated_commit **out,
	git_repository *repo,
	const git_oid *id,
	const char *description)
{
	git_commit *commit = NULL;
	int error = 0;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(id);

	*out = NULL;

	if ((error = git_commit_lookup(&commit, repo, id)) < 0)
		goto done;

	error = annotated_commit_init(out, commit, description);

done:
	git_commit_free(commit);
	return error;
}

int git_annotated_commit_lookup(
	git_annotated_commit **out,
	git_repository *repo,
	const git_oid *id)
{
	return annotated_commit_init_from_id(out, repo, id, NULL);
}

int git_annotated_commit_from_commit(
	git_annotated_commit **out,
	git_commit *commit)
{
	return annotated_commit_init(out, commit, NULL);
}

int git_annotated_commit_from_revspec(
	git_annotated_commit **out,
	git_repository *repo,
	const char *revspec)
{
	git_object *obj, *commit;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(revspec);

	if ((error = git_revparse_single(&obj, repo, revspec)) < 0)
		return error;

	if ((error = git_object_peel(&commit, obj, GIT_OBJECT_COMMIT))) {
		git_object_free(obj);
		return error;
	}

	error = annotated_commit_init(out, (git_commit *)commit, revspec);

	git_object_free(obj);
	git_object_free(commit);

	return error;
}

int git_annotated_commit_from_ref(
	git_annotated_commit **out,
	git_repository *repo,
	const git_reference *ref)
{
	git_object *peeled;
	int error = 0;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(ref);

	*out = NULL;

	if ((error = git_reference_peel(&peeled, ref, GIT_OBJECT_COMMIT)) < 0)
		return error;

	error = annotated_commit_init_from_id(out,
		repo,
		git_object_id(peeled),
		git_reference_name(ref));

	if (!error) {
		(*out)->ref_name = git__strdup(git_reference_name(ref));
		GIT_ERROR_CHECK_ALLOC((*out)->ref_name);
	}

	git_object_free(peeled);
	return error;
}

int git_annotated_commit_from_head(
	git_annotated_commit **out,
	git_repository *repo)
{
	git_reference *head;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);

	*out = NULL;

	if ((error = git_reference_lookup(&head, repo, GIT_HEAD_FILE)) < 0)
		return -1;

	error = git_annotated_commit_from_ref(out, repo, head);

	git_reference_free(head);
	return error;
}

int git_annotated_commit_from_fetchhead(
	git_annotated_commit **out,
	git_repository *repo,
	const char *branch_name,
	const char *remote_url,
	const git_oid *id)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(branch_name);
	GIT_ASSERT_ARG(remote_url);
	GIT_ASSERT_ARG(id);

	if (annotated_commit_init_from_id(out, repo, id, branch_name) < 0)
		return -1;

	(*out)->ref_name = git__strdup(branch_name);
	GIT_ERROR_CHECK_ALLOC((*out)->ref_name);

	(*out)->remote_url = git__strdup(remote_url);
	GIT_ERROR_CHECK_ALLOC((*out)->remote_url);

	return 0;
}


const git_oid *git_annotated_commit_id(
	const git_annotated_commit *annotated_commit)
{
	GIT_ASSERT_ARG_WITH_RETVAL(annotated_commit, NULL);
	return git_commit_id(annotated_commit->commit);
}

const char *git_annotated_commit_ref(
	const git_annotated_commit *annotated_commit)
{
	GIT_ASSERT_ARG_WITH_RETVAL(annotated_commit, NULL);
	return annotated_commit->ref_name;
}

void git_annotated_commit_free(git_annotated_commit *annotated_commit)
{
	if (annotated_commit == NULL)
		return;

	switch (annotated_commit->type) {
		case GIT_ANNOTATED_COMMIT_REAL:
			git_commit_free(annotated_commit->commit);
			git_tree_free(annotated_commit->tree);
			git__free((char *)annotated_commit->description);
			git__free((char *)annotated_commit->ref_name);
			git__free((char *)annotated_commit->remote_url);
			break;
		case GIT_ANNOTATED_COMMIT_VIRTUAL:
			git_index_free(annotated_commit->index);
			git_array_clear(annotated_commit->parents);
			break;
		default:
			abort();
	}

	git__free(annotated_commit);
}
