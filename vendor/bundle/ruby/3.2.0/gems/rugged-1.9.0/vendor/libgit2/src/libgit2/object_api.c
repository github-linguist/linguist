/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/object.h"

#include "repository.h"
#include "commit.h"
#include "tree.h"
#include "blob.h"
#include "tag.h"

/**
 * Commit
 */
int git_commit_lookup(git_commit **out, git_repository *repo, const git_oid *id)
{
	return git_object_lookup((git_object **)out, repo, id, GIT_OBJECT_COMMIT);
}

int git_commit_lookup_prefix(git_commit **out, git_repository *repo, const git_oid *id, size_t len)
{
	return git_object_lookup_prefix((git_object **)out, repo, id, len, GIT_OBJECT_COMMIT);
}

void git_commit_free(git_commit *obj)
{
	git_object_free((git_object *)obj);
}

const git_oid *git_commit_id(const git_commit *obj)
{
	return git_object_id((const git_object *)obj);
}

git_repository *git_commit_owner(const git_commit *obj)
{
	return git_object_owner((const git_object *)obj);
}

int git_commit_dup(git_commit **out, git_commit *obj)
{
	return git_object_dup((git_object **)out, (git_object *)obj);
}

/**
 * Tree
 */
int git_tree_lookup(git_tree **out, git_repository *repo, const git_oid *id)
{
	return git_object_lookup((git_object **)out, repo, id, GIT_OBJECT_TREE);
}

int git_tree_lookup_prefix(git_tree **out, git_repository *repo, const git_oid *id, size_t len)
{
	return git_object_lookup_prefix((git_object **)out, repo, id, len, GIT_OBJECT_TREE);
}

void git_tree_free(git_tree *obj)
{
	git_object_free((git_object *)obj);
}

const git_oid *git_tree_id(const git_tree *obj)
{
	return git_object_id((const git_object *)obj);
}

git_repository *git_tree_owner(const git_tree *obj)
{
	return git_object_owner((const git_object *)obj);
}

int git_tree_dup(git_tree **out, git_tree *obj)
{
	return git_object_dup((git_object **)out, (git_object *)obj);
}

/**
 * Tag
 */
int git_tag_lookup(git_tag **out, git_repository *repo, const git_oid *id)
{
	return git_object_lookup((git_object **)out, repo, id, GIT_OBJECT_TAG);
}

int git_tag_lookup_prefix(git_tag **out, git_repository *repo, const git_oid *id, size_t len)
{
	return git_object_lookup_prefix((git_object **)out, repo, id, len, GIT_OBJECT_TAG);
}

void git_tag_free(git_tag *obj)
{
	git_object_free((git_object *)obj);
}

const git_oid *git_tag_id(const git_tag *obj)
{
	return git_object_id((const git_object *)obj);
}

git_repository *git_tag_owner(const git_tag *obj)
{
	return git_object_owner((const git_object *)obj);
}

int git_tag_dup(git_tag **out, git_tag *obj)
{
	return git_object_dup((git_object **)out, (git_object *)obj);
}

/**
 * Blob
 */
int git_blob_lookup(git_blob **out, git_repository *repo, const git_oid *id)
{
	return git_object_lookup((git_object **)out, repo, id, GIT_OBJECT_BLOB);
}

int git_blob_lookup_prefix(git_blob **out, git_repository *repo, const git_oid *id, size_t len)
{
	return git_object_lookup_prefix((git_object **)out, repo, id, len, GIT_OBJECT_BLOB);
}

void git_blob_free(git_blob *obj)
{
	git_object_free((git_object *)obj);
}

const git_oid *git_blob_id(const git_blob *obj)
{
	return git_object_id((const git_object *)obj);
}

git_repository *git_blob_owner(const git_blob *obj)
{
	return git_object_owner((const git_object *)obj);
}

int git_blob_dup(git_blob **out, git_blob *obj)
{
	return git_object_dup((git_object **)out, (git_object *)obj);
}
