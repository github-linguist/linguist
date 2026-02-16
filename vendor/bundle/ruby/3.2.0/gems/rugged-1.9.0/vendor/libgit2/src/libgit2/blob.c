/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "blob.h"

#include "git2/common.h"
#include "git2/object.h"
#include "git2/repository.h"
#include "git2/odb_backend.h"

#include "buf.h"
#include "filebuf.h"
#include "filter.h"

const void *git_blob_rawcontent(const git_blob *blob)
{
	GIT_ASSERT_ARG_WITH_RETVAL(blob, NULL);

	if (blob->raw)
		return blob->data.raw.data;
	else
		return git_odb_object_data(blob->data.odb);
}

git_object_size_t git_blob_rawsize(const git_blob *blob)
{
	GIT_ASSERT_ARG(blob);

	if (blob->raw)
		return blob->data.raw.size;
	else
		return (git_object_size_t)git_odb_object_size(blob->data.odb);
}

int git_blob__getbuf(git_str *buffer, git_blob *blob)
{
	git_object_size_t size = git_blob_rawsize(blob);

	GIT_ERROR_CHECK_BLOBSIZE(size);
	return git_str_set(buffer, git_blob_rawcontent(blob), (size_t)size);
}

void git_blob__free(void *_blob)
{
	git_blob *blob = (git_blob *) _blob;
	if (!blob->raw)
		git_odb_object_free(blob->data.odb);
	git__free(blob);
}

int git_blob__parse_raw(void *_blob, const char *data, size_t size, git_oid_t oid_type)
{
	git_blob *blob = (git_blob *) _blob;

	GIT_ASSERT_ARG(blob);
	GIT_UNUSED(oid_type);

	blob->raw = 1;
	blob->data.raw.data = data;
	blob->data.raw.size = size;
	return 0;
}

int git_blob__parse(void *_blob, git_odb_object *odb_obj, git_oid_t oid_type)
{
	git_blob *blob = (git_blob *) _blob;

	GIT_ASSERT_ARG(blob);
	GIT_UNUSED(oid_type);

	git_cached_obj_incref((git_cached_obj *)odb_obj);
	blob->raw = 0;
	blob->data.odb = odb_obj;
	return 0;
}

int git_blob_create_from_buffer(
	git_oid *id, git_repository *repo, const void *buffer, size_t len)
{
	int error;
	git_odb *odb;
	git_odb_stream *stream;

	GIT_ASSERT_ARG(id);
	GIT_ASSERT_ARG(repo);

	if ((error = git_repository_odb__weakptr(&odb, repo)) < 0 ||
		(error = git_odb_open_wstream(&stream, odb, len, GIT_OBJECT_BLOB)) < 0)
		return error;

	if ((error = git_odb_stream_write(stream, buffer, len)) == 0)
		error = git_odb_stream_finalize_write(id, stream);

	git_odb_stream_free(stream);
	return error;
}

static int write_file_stream(
	git_oid *id, git_odb *odb, const char *path, git_object_size_t file_size)
{
	int fd, error;
	char buffer[GIT_BUFSIZE_FILEIO];
	git_odb_stream *stream = NULL;
	ssize_t read_len = -1;
	git_object_size_t written = 0;

	if ((error = git_odb_open_wstream(
			&stream, odb, file_size, GIT_OBJECT_BLOB)) < 0)
		return error;

	if ((fd = git_futils_open_ro(path)) < 0) {
		git_odb_stream_free(stream);
		return -1;
	}

	while (!error && (read_len = p_read(fd, buffer, sizeof(buffer))) > 0) {
		error = git_odb_stream_write(stream, buffer, read_len);
		written += read_len;
	}

	p_close(fd);

	if (written != file_size || read_len < 0) {
		git_error_set(GIT_ERROR_OS, "failed to read file into stream");
		error = -1;
	}

	if (!error)
		error = git_odb_stream_finalize_write(id, stream);

	git_odb_stream_free(stream);
	return error;
}

static int write_file_filtered(
	git_oid *id,
	git_object_size_t *size,
	git_odb *odb,
	const char *full_path,
	git_filter_list *fl,
	git_repository* repo)
{
	int error;
	git_str tgt = GIT_STR_INIT;

	error = git_filter_list__apply_to_file(&tgt, fl, repo, full_path);

	/* Write the file to disk if it was properly filtered */
	if (!error) {
		*size = tgt.size;

		error = git_odb_write(id, odb, tgt.ptr, tgt.size, GIT_OBJECT_BLOB);
	}

	git_str_dispose(&tgt);
	return error;
}

static int write_symlink(
	git_oid *id, git_odb *odb, const char *path, size_t link_size)
{
	char *link_data;
	ssize_t read_len;
	int error;

	link_data = git__malloc(link_size);
	GIT_ERROR_CHECK_ALLOC(link_data);

	read_len = p_readlink(path, link_data, link_size);
	if (read_len != (ssize_t)link_size) {
		git_error_set(GIT_ERROR_OS, "failed to create blob: cannot read symlink '%s'", path);
		git__free(link_data);
		return -1;
	}

	error = git_odb_write(id, odb, (void *)link_data, link_size, GIT_OBJECT_BLOB);
	git__free(link_data);
	return error;
}

int git_blob__create_from_paths(
	git_oid *id,
	struct stat *out_st,
	git_repository *repo,
	const char *content_path,
	const char *hint_path,
	mode_t hint_mode,
	bool try_load_filters)
{
	int error;
	struct stat st;
	git_odb *odb = NULL;
	git_object_size_t size;
	mode_t mode;
	git_str path = GIT_STR_INIT;

	GIT_ASSERT_ARG(hint_path || !try_load_filters);

	if (!content_path) {
		if (git_repository_workdir_path(&path, repo, hint_path) < 0)
			return -1;

		content_path = path.ptr;
	}

	if ((error = git_fs_path_lstat(content_path, &st)) < 0 ||
		(error = git_repository_odb(&odb, repo)) < 0)
		goto done;

	if (S_ISDIR(st.st_mode)) {
		git_error_set(GIT_ERROR_ODB, "cannot create blob from '%s': it is a directory", content_path);
		error = GIT_EDIRECTORY;
		goto done;
	}

	if (out_st)
		memcpy(out_st, &st, sizeof(st));

	size = st.st_size;
	mode = hint_mode ? hint_mode : st.st_mode;

	if (S_ISLNK(mode)) {
		error = write_symlink(id, odb, content_path, (size_t)size);
	} else {
		git_filter_list *fl = NULL;

		if (try_load_filters)
			/* Load the filters for writing this file to the ODB */
			error = git_filter_list_load(
				&fl, repo, NULL, hint_path,
				GIT_FILTER_TO_ODB, GIT_FILTER_DEFAULT);

		if (error < 0)
			/* well, that didn't work */;
		else if (fl == NULL)
			/* No filters need to be applied to the document: we can stream
			 * directly from disk */
			error = write_file_stream(id, odb, content_path, size);
		else {
			/* We need to apply one or more filters */
			error = write_file_filtered(id, &size, odb, content_path, fl, repo);

			git_filter_list_free(fl);
		}

		/*
		 * TODO: eventually support streaming filtered files, for files
		 * which are bigger than a given threshold. This is not a priority
		 * because applying a filter in streaming mode changes the final
		 * size of the blob, and without knowing its final size, the blob
		 * cannot be written in stream mode to the ODB.
		 *
		 * The plan is to do streaming writes to a tempfile on disk and then
		 * opening streaming that file to the ODB, using
		 * `write_file_stream`.
		 *
		 * CAREFULLY DESIGNED APIS YO
		 */
	}

done:
	git_odb_free(odb);
	git_str_dispose(&path);

	return error;
}

int git_blob_create_from_workdir(
	git_oid *id, git_repository *repo, const char *path)
{
	return git_blob__create_from_paths(id, NULL, repo, NULL, path, 0, true);
}

int git_blob_create_from_disk(
	git_oid *id, git_repository *repo, const char *path)
{
	int error;
	git_str full_path = GIT_STR_INIT;
	const char *workdir, *hintpath = NULL;

	if ((error = git_fs_path_prettify(&full_path, path, NULL)) < 0) {
		git_str_dispose(&full_path);
		return error;
	}

	workdir  = git_repository_workdir(repo);

	if (workdir && !git__prefixcmp(full_path.ptr, workdir))
		hintpath = full_path.ptr + strlen(workdir);

	error = git_blob__create_from_paths(
		id, NULL, repo, git_str_cstr(&full_path), hintpath, 0, !!hintpath);

	git_str_dispose(&full_path);
	return error;
}

typedef struct {
	git_writestream parent;
	git_filebuf fbuf;
	git_repository *repo;
	char *hintpath;
} blob_writestream;

static int blob_writestream_close(git_writestream *_stream)
{
	blob_writestream *stream = (blob_writestream *) _stream;

	git_filebuf_cleanup(&stream->fbuf);
	return 0;
}

static void blob_writestream_free(git_writestream *_stream)
{
	blob_writestream *stream = (blob_writestream *) _stream;

	git_filebuf_cleanup(&stream->fbuf);
	git__free(stream->hintpath);
	git__free(stream);
}

static int blob_writestream_write(git_writestream *_stream, const char *buffer, size_t len)
{
	blob_writestream *stream = (blob_writestream *) _stream;

	return git_filebuf_write(&stream->fbuf, buffer, len);
}

int git_blob_create_from_stream(git_writestream **out, git_repository *repo, const char *hintpath)
{
	int error;
	git_str path = GIT_STR_INIT;
	blob_writestream *stream;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);

	stream = git__calloc(1, sizeof(blob_writestream));
	GIT_ERROR_CHECK_ALLOC(stream);

	if (hintpath) {
		stream->hintpath = git__strdup(hintpath);
		GIT_ERROR_CHECK_ALLOC(stream->hintpath);
	}

	stream->repo = repo;
	stream->parent.write = blob_writestream_write;
	stream->parent.close = blob_writestream_close;
	stream->parent.free  = blob_writestream_free;

	if ((error = git_repository__item_path(&path, repo, GIT_REPOSITORY_ITEM_OBJECTS)) < 0
		|| (error = git_str_joinpath(&path, path.ptr, "streamed")) < 0)
		goto cleanup;

	if ((error = git_filebuf_open_withsize(&stream->fbuf, git_str_cstr(&path), GIT_FILEBUF_TEMPORARY,
					       0666, 2 * 1024 * 1024)) < 0)
		goto cleanup;

	*out = (git_writestream *) stream;

cleanup:
	if (error < 0)
		blob_writestream_free((git_writestream *) stream);

	git_str_dispose(&path);
	return error;
}

int git_blob_create_from_stream_commit(git_oid *out, git_writestream *_stream)
{
	int error;
	blob_writestream *stream = (blob_writestream *) _stream;

	/*
	 * We can make this more officient by avoiding writing to
	 * disk, but for now let's re-use the helper functions we
	 * have.
	 */
	if ((error = git_filebuf_flush(&stream->fbuf)) < 0)
		goto cleanup;

	error = git_blob__create_from_paths(out, NULL, stream->repo, stream->fbuf.path_lock,
					    stream->hintpath, 0, !!stream->hintpath);

cleanup:
	blob_writestream_free(_stream);
	return error;

}

int git_blob_is_binary(const git_blob *blob)
{
	git_str content = GIT_STR_INIT;
	git_object_size_t size;

	GIT_ASSERT_ARG(blob);

	size = git_blob_rawsize(blob);

	git_str_attach_notowned(&content, git_blob_rawcontent(blob),
		(size_t)min(size, GIT_FILTER_BYTES_TO_CHECK_NUL));
	return git_str_is_binary(&content);
}

int git_blob_data_is_binary(const char *str, size_t len)
{
	git_str content = GIT_STR_INIT;

	git_str_attach_notowned(&content, str, len);

	return git_str_is_binary(&content);
}

int git_blob_filter_options_init(
	git_blob_filter_options *opts,
	unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(opts, version,
		git_blob_filter_options, GIT_BLOB_FILTER_OPTIONS_INIT);
	return 0;
}

int git_blob_filter(
	git_buf *out,
	git_blob *blob,
	const char *path,
	git_blob_filter_options *given_opts)
{
	git_blob_filter_options opts = GIT_BLOB_FILTER_OPTIONS_INIT;
	git_filter_options filter_opts = GIT_FILTER_OPTIONS_INIT;
	git_filter_list *fl = NULL;
	int error = 0;

	GIT_ASSERT_ARG(blob);
	GIT_ASSERT_ARG(path);
	GIT_ASSERT_ARG(out);

	GIT_ERROR_CHECK_VERSION(
		given_opts, GIT_BLOB_FILTER_OPTIONS_VERSION, "git_blob_filter_options");

	if (given_opts != NULL)
		memcpy(&opts, given_opts, sizeof(git_blob_filter_options));

	if ((opts.flags & GIT_BLOB_FILTER_CHECK_FOR_BINARY) != 0 &&
	    git_blob_is_binary(blob))
		return 0;

	if ((opts.flags & GIT_BLOB_FILTER_NO_SYSTEM_ATTRIBUTES) != 0)
		filter_opts.flags |= GIT_FILTER_NO_SYSTEM_ATTRIBUTES;

	if ((opts.flags & GIT_BLOB_FILTER_ATTRIBUTES_FROM_HEAD) != 0)
		filter_opts.flags |= GIT_FILTER_ATTRIBUTES_FROM_HEAD;

	if ((opts.flags & GIT_BLOB_FILTER_ATTRIBUTES_FROM_COMMIT) != 0) {
		filter_opts.flags |= GIT_FILTER_ATTRIBUTES_FROM_COMMIT;

#ifndef GIT_DEPRECATE_HARD
		if (opts.commit_id)
			git_oid_cpy(&filter_opts.attr_commit_id, opts.commit_id);
		else
#endif
		git_oid_cpy(&filter_opts.attr_commit_id, &opts.attr_commit_id);
	}

	if (!(error = git_filter_list_load_ext(
			&fl, git_blob_owner(blob), blob, path,
			GIT_FILTER_TO_WORKTREE, &filter_opts))) {

		error = git_filter_list_apply_to_blob(out, fl, blob);

		git_filter_list_free(fl);
	}

	return error;
}

/* Deprecated functions */

#ifndef GIT_DEPRECATE_HARD
int git_blob_create_frombuffer(
	git_oid *id, git_repository *repo, const void *buffer, size_t len)
{
	return git_blob_create_from_buffer(id, repo, buffer, len);
}

int git_blob_create_fromworkdir(git_oid *id, git_repository *repo, const char *relative_path)
{
	return git_blob_create_from_workdir(id, repo, relative_path);
}

int git_blob_create_fromdisk(git_oid *id, git_repository *repo, const char *path)
{
	return git_blob_create_from_disk(id, repo, path);
}

int git_blob_create_fromstream(
    git_writestream **out,
    git_repository *repo,
    const char *hintpath)
{
	return  git_blob_create_from_stream(out, repo, hintpath);
}

int git_blob_create_fromstream_commit(
	git_oid *out,
	git_writestream *stream)
{
	return git_blob_create_from_stream_commit(out, stream);
}

int git_blob_filtered_content(
	git_buf *out,
	git_blob *blob,
	const char *path,
	int check_for_binary_data)
{
	git_blob_filter_options opts = GIT_BLOB_FILTER_OPTIONS_INIT;

	if (check_for_binary_data)
		opts.flags |= GIT_BLOB_FILTER_CHECK_FOR_BINARY;
	else
		opts.flags &= ~GIT_BLOB_FILTER_CHECK_FOR_BINARY;

	return git_blob_filter(out, blob, path, &opts);
}
#endif
