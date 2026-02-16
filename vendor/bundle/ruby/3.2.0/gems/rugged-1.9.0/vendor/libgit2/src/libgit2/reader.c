/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "reader.h"

#include "futils.h"
#include "blob.h"

#include "git2/tree.h"
#include "git2/blob.h"
#include "git2/index.h"
#include "git2/repository.h"

/* tree reader */

typedef struct {
	git_reader reader;
	git_tree *tree;
} tree_reader;

static int tree_reader_read(
	git_str *out,
	git_oid *out_id,
	git_filemode_t *out_filemode,
	git_reader *_reader,
	const char *filename)
{
	tree_reader *reader = (tree_reader *)_reader;
	git_tree_entry *tree_entry = NULL;
	git_blob *blob = NULL;
	git_object_size_t blobsize;
	int error;

	if ((error = git_tree_entry_bypath(&tree_entry, reader->tree, filename)) < 0 ||
	    (error = git_blob_lookup(&blob, git_tree_owner(reader->tree), git_tree_entry_id(tree_entry))) < 0)
		goto done;

	blobsize = git_blob_rawsize(blob);
	GIT_ERROR_CHECK_BLOBSIZE(blobsize);

	if ((error = git_str_set(out, git_blob_rawcontent(blob), (size_t)blobsize)) < 0)
		goto done;

	if (out_id)
		git_oid_cpy(out_id, git_tree_entry_id(tree_entry));

	if (out_filemode)
		*out_filemode = git_tree_entry_filemode(tree_entry);

done:
	git_blob_free(blob);
	git_tree_entry_free(tree_entry);
	return error;
}

int git_reader_for_tree(git_reader **out, git_tree *tree)
{
	tree_reader *reader;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(tree);

	reader = git__calloc(1, sizeof(tree_reader));
	GIT_ERROR_CHECK_ALLOC(reader);

	reader->reader.read = tree_reader_read;
	reader->tree = tree;

	*out = (git_reader *)reader;
	return 0;
}

/* workdir reader */

typedef struct {
	git_reader reader;
	git_repository *repo;
	git_index *index;
} workdir_reader;

static int workdir_reader_read(
	git_str *out,
	git_oid *out_id,
	git_filemode_t *out_filemode,
	git_reader *_reader,
	const char *filename)
{
	workdir_reader *reader = (workdir_reader *)_reader;
	git_str path = GIT_STR_INIT;
	struct stat st;
	git_filemode_t filemode;
	git_filter_list *filters = NULL;
	const git_index_entry *idx_entry;
	git_oid id;
	int error;

	if ((error = git_repository_workdir_path(&path, reader->repo, filename)) < 0)
		goto done;

	if ((error = p_lstat(path.ptr, &st)) < 0) {
		if (error == -1 && errno == ENOENT)
			error = GIT_ENOTFOUND;

		git_error_set(GIT_ERROR_OS, "could not stat '%s'", path.ptr);
		goto done;
	}

	filemode = git_futils_canonical_mode(st.st_mode);

	/*
	 * Patch application - for example - uses the filtered version of
	 * the working directory data to match git.  So we will run the
	 * workdir -> ODB filter on the contents in this workdir reader.
	 */
	if ((error = git_filter_list_load(&filters, reader->repo, NULL, filename,
		GIT_FILTER_TO_ODB, GIT_FILTER_DEFAULT)) < 0)
		goto done;

	if ((error = git_filter_list__apply_to_file(out,
	    filters, reader->repo, path.ptr)) < 0)
		goto done;

	if (out_id || reader->index) {
		if ((error = git_odb__hash(&id, out->ptr, out->size, GIT_OBJECT_BLOB, reader->repo->oid_type)) < 0)
			goto done;
	}

	if (reader->index) {
		if (!(idx_entry = git_index_get_bypath(reader->index, filename, 0)) ||
		    filemode != idx_entry->mode ||
		    !git_oid_equal(&id, &idx_entry->id)) {
			error = GIT_READER_MISMATCH;
			goto done;
		}
	}

	if (out_id)
		git_oid_cpy(out_id, &id);

	if (out_filemode)
		*out_filemode = filemode;

done:
	git_filter_list_free(filters);
	git_str_dispose(&path);
	return error;
}

int git_reader_for_workdir(
	git_reader **out,
	git_repository *repo,
	bool validate_index)
{
	workdir_reader *reader;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);

	reader = git__calloc(1, sizeof(workdir_reader));
	GIT_ERROR_CHECK_ALLOC(reader);

	reader->reader.read = workdir_reader_read;
	reader->repo = repo;

	if (validate_index &&
	    (error = git_repository_index__weakptr(&reader->index, repo)) < 0) {
		git__free(reader);
		return error;
	}

	*out = (git_reader *)reader;
	return 0;
}

/* index reader */

typedef struct {
	git_reader reader;
	git_repository *repo;
	git_index *index;
} index_reader;

static int index_reader_read(
	git_str *out,
	git_oid *out_id,
	git_filemode_t *out_filemode,
	git_reader *_reader,
	const char *filename)
{
	index_reader *reader = (index_reader *)_reader;
	const git_index_entry *entry;
	git_blob *blob;
	int error;

	if ((entry = git_index_get_bypath(reader->index, filename, 0)) == NULL)
		return GIT_ENOTFOUND;

	if ((error = git_blob_lookup(&blob, reader->repo, &entry->id)) < 0)
		goto done;

	if (out_id)
		git_oid_cpy(out_id, &entry->id);

	if (out_filemode)
		*out_filemode = entry->mode;

	error = git_blob__getbuf(out, blob);

done:
	git_blob_free(blob);
	return error;
}

int git_reader_for_index(
	git_reader **out,
	git_repository *repo,
	git_index *index)
{
	index_reader *reader;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);

	reader = git__calloc(1, sizeof(index_reader));
	GIT_ERROR_CHECK_ALLOC(reader);

	reader->reader.read = index_reader_read;
	reader->repo = repo;

	if (index) {
		reader->index = index;
	} else if ((error = git_repository_index__weakptr(&reader->index, repo)) < 0) {
		git__free(reader);
		return error;
	}

	*out = (git_reader *)reader;
	return 0;
}

/* generic */

int git_reader_read(
	git_str *out,
	git_oid *out_id,
	git_filemode_t *out_filemode,
	git_reader *reader,
	const char *filename)
{
	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(reader);
	GIT_ASSERT_ARG(filename);

	return reader->read(out, out_id, out_filemode, reader, filename);
}

void git_reader_free(git_reader *reader)
{
	if (!reader)
		return;

	git__free(reader);
}
