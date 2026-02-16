/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "patch_generate.h"

#include "git2/blob.h"
#include "diff.h"
#include "diff_generate.h"
#include "diff_file.h"
#include "diff_driver.h"
#include "diff_xdiff.h"
#include "delta.h"
#include "zstream.h"
#include "futils.h"

static void diff_output_init(
	git_patch_generated_output *, const git_diff_options *, git_diff_file_cb,
	git_diff_binary_cb, git_diff_hunk_cb, git_diff_line_cb, void*);

static void diff_output_to_patch(
	git_patch_generated_output *, git_patch_generated *);

static void patch_generated_free(git_patch *p)
{
	git_patch_generated *patch = (git_patch_generated *)p;

	git_array_clear(patch->base.lines);
	git_array_clear(patch->base.hunks);

	git__free((char *)patch->base.binary.old_file.data);
	git__free((char *)patch->base.binary.new_file.data);

	git_diff_file_content__clear(&patch->ofile);
	git_diff_file_content__clear(&patch->nfile);

	git_diff_free(patch->diff); /* decrements refcount */
	patch->diff = NULL;

	git_pool_clear(&patch->flattened);

	git__free((char *)patch->base.diff_opts.old_prefix);
	git__free((char *)patch->base.diff_opts.new_prefix);

	if (patch->flags & GIT_PATCH_GENERATED_ALLOCATED)
		git__free(patch);
}

static void patch_generated_update_binary(git_patch_generated *patch)
{
	if ((patch->base.delta->flags & DIFF_FLAGS_KNOWN_BINARY) != 0)
		return;

	if ((patch->ofile.file->flags & GIT_DIFF_FLAG_BINARY) != 0 ||
		(patch->nfile.file->flags & GIT_DIFF_FLAG_BINARY) != 0)
		patch->base.delta->flags |= GIT_DIFF_FLAG_BINARY;

	else if (patch->ofile.file->size > GIT_XDIFF_MAX_SIZE ||
		patch->nfile.file->size > GIT_XDIFF_MAX_SIZE)
		patch->base.delta->flags |= GIT_DIFF_FLAG_BINARY;

	else if ((patch->ofile.file->flags & DIFF_FLAGS_NOT_BINARY) != 0 &&
		(patch->nfile.file->flags & DIFF_FLAGS_NOT_BINARY) != 0)
		patch->base.delta->flags |= GIT_DIFF_FLAG_NOT_BINARY;
}

static void patch_generated_init_common(git_patch_generated *patch)
{
	patch->base.free_fn = patch_generated_free;

	patch_generated_update_binary(patch);

	patch->flags |= GIT_PATCH_GENERATED_INITIALIZED;

	if (patch->diff)
		git_diff_addref(patch->diff);
}

static int patch_generated_normalize_options(
	git_diff_options *out,
	const git_diff_options *opts,
	git_repository *repo)
{
	if (opts) {
		GIT_ERROR_CHECK_VERSION(opts, GIT_DIFF_OPTIONS_VERSION, "git_diff_options");
		memcpy(out, opts, sizeof(git_diff_options));
	} else {
		git_diff_options default_opts = GIT_DIFF_OPTIONS_INIT;
		memcpy(out, &default_opts, sizeof(git_diff_options));
	}

	if (repo && opts && opts->oid_type && repo->oid_type != opts->oid_type) {
		/*
		 * This limitation feels unnecessary - we should consider
		 * allowing users to generate diffs with a different object
		 * ID format than the repository.
		 */
		git_error_set(GIT_ERROR_INVALID,
			"specified object ID type does not match repository object ID type");
		return -1;
	} else if (repo) {
		out->oid_type = repo->oid_type;
	} else if (opts && opts->oid_type) {
		out->oid_type = opts->oid_type;
	} else {
		out->oid_type = GIT_OID_DEFAULT;
	}

	out->old_prefix = opts && opts->old_prefix ?
		git__strdup(opts->old_prefix) :
		git__strdup(DIFF_OLD_PREFIX_DEFAULT);

	out->new_prefix = opts && opts->new_prefix ?
		git__strdup(opts->new_prefix) :
		git__strdup(DIFF_NEW_PREFIX_DEFAULT);

	GIT_ERROR_CHECK_ALLOC(out->old_prefix);
	GIT_ERROR_CHECK_ALLOC(out->new_prefix);

	return 0;
}

static int patch_generated_init(
	git_patch_generated *patch, git_diff *diff, size_t delta_index)
{
	int error = 0;

	memset(patch, 0, sizeof(*patch));

	patch->diff = diff;
	patch->base.repo = diff->repo;
	patch->base.delta = git_vector_get(&diff->deltas, delta_index);
	patch->delta_index = delta_index;

	if ((error = patch_generated_normalize_options(
			&patch->base.diff_opts, &diff->opts, diff->repo)) < 0 ||
		(error = git_diff_file_content__init_from_diff(
			&patch->ofile, diff, patch->base.delta, true)) < 0 ||
		(error = git_diff_file_content__init_from_diff(
			&patch->nfile, diff, patch->base.delta, false)) < 0)
		return error;

	patch_generated_init_common(patch);

	return 0;
}

static int patch_generated_alloc_from_diff(
	git_patch_generated **out, git_diff *diff, size_t delta_index)
{
	int error;
	git_patch_generated *patch = git__calloc(1, sizeof(git_patch_generated));
	GIT_ERROR_CHECK_ALLOC(patch);

	if (!(error = patch_generated_init(patch, diff, delta_index))) {
		patch->flags |= GIT_PATCH_GENERATED_ALLOCATED;
		GIT_REFCOUNT_INC(&patch->base);
	} else {
		git__free(patch);
		patch = NULL;
	}

	*out = patch;
	return error;
}

GIT_INLINE(bool) should_skip_binary(git_patch_generated *patch, git_diff_file *file)
{
	if ((patch->base.diff_opts.flags & GIT_DIFF_SHOW_BINARY) != 0)
		return false;

	return (file->flags & GIT_DIFF_FLAG_BINARY) != 0;
}

static bool patch_generated_diffable(git_patch_generated *patch)
{
	size_t olen, nlen;

	if (patch->base.delta->status == GIT_DELTA_UNMODIFIED)
		return false;

	/* if we've determined this to be binary (and we are not showing binary
	 * data) then we have skipped loading the map data.  instead, query the
	 * file data itself.
	 */
	if ((patch->base.delta->flags & GIT_DIFF_FLAG_BINARY) != 0 &&
		(patch->base.diff_opts.flags & GIT_DIFF_SHOW_BINARY) == 0) {
		olen = (size_t)patch->ofile.file->size;
		nlen = (size_t)patch->nfile.file->size;
	} else {
		olen = patch->ofile.map.len;
		nlen = patch->nfile.map.len;
	}

	/* if both sides are empty, files are identical */
	if (!olen && !nlen)
		return false;

	/* otherwise, check the file sizes and the oid */
	return (olen != nlen ||
		!git_oid_equal(&patch->ofile.file->id, &patch->nfile.file->id));
}

static int patch_generated_load(git_patch_generated *patch, git_patch_generated_output *output)
{
	int error = 0;
	bool incomplete_data;

	if ((patch->flags & GIT_PATCH_GENERATED_LOADED) != 0)
		return 0;

	/* if no hunk and data callbacks and user doesn't care if data looks
	 * binary, then there is no need to actually load the data
	 */
	if ((patch->ofile.opts_flags & GIT_DIFF_SKIP_BINARY_CHECK) != 0 &&
		output && !output->binary_cb && !output->hunk_cb && !output->data_cb)
		return 0;

	incomplete_data =
		(((patch->ofile.flags & GIT_DIFF_FLAG__NO_DATA) != 0 ||
		  (patch->ofile.file->flags & GIT_DIFF_FLAG_VALID_ID) != 0) &&
		 ((patch->nfile.flags & GIT_DIFF_FLAG__NO_DATA) != 0 ||
		  (patch->nfile.file->flags & GIT_DIFF_FLAG_VALID_ID) != 0));

	if ((error = git_diff_file_content__load(
			&patch->ofile, &patch->base.diff_opts)) < 0 ||
	    (error = git_diff_file_content__load(
			&patch->nfile, &patch->base.diff_opts)) < 0 ||
		should_skip_binary(patch, patch->nfile.file))
		goto cleanup;

	/* if previously missing an oid, and now that we have it the two sides
	 * are the same (and not submodules), update MODIFIED -> UNMODIFIED
	 */
	if (incomplete_data &&
		patch->ofile.file->mode == patch->nfile.file->mode &&
		patch->ofile.file->mode != GIT_FILEMODE_COMMIT &&
		git_oid_equal(&patch->ofile.file->id, &patch->nfile.file->id) &&
		patch->base.delta->status == GIT_DELTA_MODIFIED) /* not RENAMED/COPIED! */
		patch->base.delta->status = GIT_DELTA_UNMODIFIED;

cleanup:
	patch_generated_update_binary(patch);

	if (!error) {
		if (patch_generated_diffable(patch))
			patch->flags |= GIT_PATCH_GENERATED_DIFFABLE;

		patch->flags |= GIT_PATCH_GENERATED_LOADED;
	}

	return error;
}

static int patch_generated_invoke_file_callback(
	git_patch_generated *patch, git_patch_generated_output *output)
{
	float progress = patch->diff ?
		((float)patch->delta_index / patch->diff->deltas.length) : 1.0f;

	if (!output->file_cb)
		return 0;

	return git_error_set_after_callback_function(
		output->file_cb(patch->base.delta, progress, output->payload),
		"git_patch");
}

static int create_binary(
	git_diff_binary_t *out_type,
	char **out_data,
	size_t *out_datalen,
	size_t *out_inflatedlen,
	const char *a_data,
	size_t a_datalen,
	const char *b_data,
	size_t b_datalen)
{
	git_str deflate = GIT_STR_INIT, delta = GIT_STR_INIT;
	size_t delta_data_len = 0;
	int error;

	/* The git_delta function accepts unsigned long only */
	if (!git__is_ulong(a_datalen) || !git__is_ulong(b_datalen))
		return GIT_EBUFS;

	if ((error = git_zstream_deflatebuf(&deflate, b_data, b_datalen)) < 0)
		goto done;

	/* The git_delta function accepts unsigned long only */
	if (!git__is_ulong(deflate.size)) {
		error = GIT_EBUFS;
		goto done;
	}

	if (a_datalen && b_datalen) {
		void *delta_data;

		error = git_delta(&delta_data, &delta_data_len,
			a_data, a_datalen,
			b_data, b_datalen,
			deflate.size);

		if (error == 0) {
			error = git_zstream_deflatebuf(
				&delta, delta_data, delta_data_len);

			git__free(delta_data);
		} else if (error == GIT_EBUFS) {
			error = 0;
		}

		if (error < 0)
			goto done;
	}

	if (delta.size && delta.size < deflate.size) {
		*out_type = GIT_DIFF_BINARY_DELTA;
		*out_datalen = delta.size;
		*out_data = git_str_detach(&delta);
		*out_inflatedlen = delta_data_len;
	} else {
		*out_type = GIT_DIFF_BINARY_LITERAL;
		*out_datalen = deflate.size;
		*out_data = git_str_detach(&deflate);
		*out_inflatedlen = b_datalen;
	}

done:
	git_str_dispose(&deflate);
	git_str_dispose(&delta);

	return error;
}

static int diff_binary(git_patch_generated_output *output, git_patch_generated *patch)
{
	git_diff_binary binary = {0};
	const char *old_data = patch->ofile.map.data;
	const char *new_data = patch->nfile.map.data;
	size_t old_len = patch->ofile.map.len,
		new_len = patch->nfile.map.len;
	int error;

	/* Only load contents if the user actually wants to diff
	 * binary files. */
	if (patch->base.diff_opts.flags & GIT_DIFF_SHOW_BINARY) {
		binary.contains_data = 1;

		/* Create the old->new delta (as the "new" side of the patch),
		 * and the new->old delta (as the "old" side)
		 */
		if ((error = create_binary(&binary.old_file.type,
				(char **)&binary.old_file.data,
				&binary.old_file.datalen,
				&binary.old_file.inflatedlen,
				new_data, new_len, old_data, old_len)) < 0 ||
			(error = create_binary(&binary.new_file.type,
				(char **)&binary.new_file.data,
				&binary.new_file.datalen,
				&binary.new_file.inflatedlen,
				old_data, old_len, new_data, new_len)) < 0)
			return error;
	}

	error = git_error_set_after_callback_function(
		output->binary_cb(patch->base.delta, &binary, output->payload),
		"git_patch");

	git__free((char *) binary.old_file.data);
	git__free((char *) binary.new_file.data);

	return error;
}

static int patch_generated_create(
	git_patch_generated *patch,
	git_patch_generated_output *output)
{
	int error = 0;

	if ((patch->flags & GIT_PATCH_GENERATED_DIFFED) != 0)
		return 0;

	/* if we are not looking at the binary or text data, don't do the diff */
	if (!output->binary_cb && !output->hunk_cb && !output->data_cb)
		return 0;

	if ((patch->flags & GIT_PATCH_GENERATED_LOADED) == 0 &&
		(error = patch_generated_load(patch, output)) < 0)
		return error;

	if ((patch->flags & GIT_PATCH_GENERATED_DIFFABLE) == 0)
		return 0;

	if ((patch->base.delta->flags & GIT_DIFF_FLAG_BINARY) != 0) {
		if (output->binary_cb)
			error = diff_binary(output, patch);
	}
	else {
		if (output->diff_cb)
			error = output->diff_cb(output, patch);
	}

	patch->flags |= GIT_PATCH_GENERATED_DIFFED;
	return error;
}

static int diff_required(git_diff *diff, const char *action)
{
	if (diff)
		return 0;
	git_error_set(GIT_ERROR_INVALID, "must provide valid diff to %s", action);
	return -1;
}

typedef struct {
	git_patch_generated patch;
	git_diff_delta delta;
	char paths[GIT_FLEX_ARRAY];
} patch_generated_with_delta;

static int diff_single_generate(patch_generated_with_delta *pd, git_xdiff_output *xo)
{
	int error = 0;
	git_patch_generated *patch = &pd->patch;
	bool has_old = ((patch->ofile.flags & GIT_DIFF_FLAG__NO_DATA) == 0);
	bool has_new = ((patch->nfile.flags & GIT_DIFF_FLAG__NO_DATA) == 0);

	pd->delta.status = has_new ?
		(has_old ? GIT_DELTA_MODIFIED : GIT_DELTA_ADDED) :
		(has_old ? GIT_DELTA_DELETED : GIT_DELTA_UNTRACKED);

	if (git_oid_equal(&patch->nfile.file->id, &patch->ofile.file->id))
		pd->delta.status = GIT_DELTA_UNMODIFIED;

	patch->base.delta = &pd->delta;

	patch_generated_init_common(patch);

	if (pd->delta.status == GIT_DELTA_UNMODIFIED &&
		!(patch->ofile.opts_flags & GIT_DIFF_INCLUDE_UNMODIFIED)) {

		/* Even empty patches are flagged as binary, and even though
		 * there's no difference, we flag this as "containing data"
		 * (the data is known to be empty, as opposed to wholly unknown).
		 */
		if (patch->base.diff_opts.flags & GIT_DIFF_SHOW_BINARY)
			patch->base.binary.contains_data = 1;

		return error;
	}

	error = patch_generated_invoke_file_callback(patch, (git_patch_generated_output *)xo);

	if (!error)
		error = patch_generated_create(patch, (git_patch_generated_output *)xo);

	return error;
}

static int patch_generated_from_sources(
	patch_generated_with_delta *pd,
	git_xdiff_output *xo,
	git_diff_file_content_src *oldsrc,
	git_diff_file_content_src *newsrc,
	const git_diff_options *given_opts)
{
	int error = 0;
	git_repository *repo =
		oldsrc->blob ? git_blob_owner(oldsrc->blob) :
		newsrc->blob ? git_blob_owner(newsrc->blob) : NULL;
	git_diff_file *lfile = &pd->delta.old_file, *rfile = &pd->delta.new_file;
	git_diff_file_content *ldata = &pd->patch.ofile, *rdata = &pd->patch.nfile;
	git_diff_options *opts = &pd->patch.base.diff_opts;

	if ((error = patch_generated_normalize_options(opts, given_opts, repo)) < 0)
		return error;

	if ((opts->flags & GIT_DIFF_REVERSE) != 0) {
		void *tmp = lfile; lfile = rfile; rfile = tmp;
		tmp = ldata; ldata = rdata; rdata = tmp;
	}

	pd->patch.base.delta = &pd->delta;

	if (!oldsrc->as_path) {
		if (newsrc->as_path)
			oldsrc->as_path = newsrc->as_path;
		else
			oldsrc->as_path = newsrc->as_path = "file";
	}
	else if (!newsrc->as_path)
		newsrc->as_path = oldsrc->as_path;

	lfile->path = oldsrc->as_path;
	rfile->path = newsrc->as_path;

	if ((error = git_diff_file_content__init_from_src(
			ldata, repo, opts, oldsrc, lfile)) < 0 ||
		(error = git_diff_file_content__init_from_src(
			rdata, repo, opts, newsrc, rfile)) < 0)
		return error;

	return diff_single_generate(pd, xo);
}

static int patch_generated_with_delta_alloc(
	patch_generated_with_delta **out,
	const char **old_path,
	const char **new_path)
{
	patch_generated_with_delta *pd;
	size_t old_len = *old_path ? strlen(*old_path) : 0;
	size_t new_len = *new_path ? strlen(*new_path) : 0;
	size_t alloc_len;

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, sizeof(*pd), old_len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, new_len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 2);

	*out = pd = git__calloc(1, alloc_len);
	GIT_ERROR_CHECK_ALLOC(pd);

	pd->patch.flags = GIT_PATCH_GENERATED_ALLOCATED;

	if (*old_path) {
		memcpy(&pd->paths[0], *old_path, old_len);
		*old_path = &pd->paths[0];
	} else if (*new_path)
		*old_path = &pd->paths[old_len + 1];

	if (*new_path) {
		memcpy(&pd->paths[old_len + 1], *new_path, new_len);
		*new_path = &pd->paths[old_len + 1];
	} else if (*old_path)
		*new_path = &pd->paths[0];

	return 0;
}

static int diff_from_sources(
	git_diff_file_content_src *oldsrc,
	git_diff_file_content_src *newsrc,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	int error = 0;
	patch_generated_with_delta pd;
	git_xdiff_output xo;

	memset(&xo, 0, sizeof(xo));
	diff_output_init(
		&xo.output, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
	git_xdiff_init(&xo, opts);

	memset(&pd, 0, sizeof(pd));

	error = patch_generated_from_sources(&pd, &xo, oldsrc, newsrc, opts);

	git_patch_free(&pd.patch.base);

	return error;
}

static int patch_from_sources(
	git_patch **out,
	git_diff_file_content_src *oldsrc,
	git_diff_file_content_src *newsrc,
	const git_diff_options *opts)
{
	int error = 0;
	patch_generated_with_delta *pd;
	git_xdiff_output xo;

	GIT_ASSERT_ARG(out);
	*out = NULL;

	if ((error = patch_generated_with_delta_alloc(
			&pd, &oldsrc->as_path, &newsrc->as_path)) < 0)
		return error;

	memset(&xo, 0, sizeof(xo));
	diff_output_to_patch(&xo.output, &pd->patch);
	git_xdiff_init(&xo, opts);

	if (!(error = patch_generated_from_sources(pd, &xo, oldsrc, newsrc, opts)))
		*out = (git_patch *)pd;
	else
		git_patch_free((git_patch *)pd);

	return error;
}

int git_diff_blobs(
	const git_blob *old_blob,
	const char *old_path,
	const git_blob *new_blob,
	const char *new_path,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(new_blob, new_path);
	return diff_from_sources(
		&osrc, &nsrc, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
}

int git_patch_from_blobs(
	git_patch **out,
	const git_blob *old_blob,
	const char *old_path,
	const git_blob *new_blob,
	const char *new_path,
	const git_diff_options *opts)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(new_blob, new_path);
	return patch_from_sources(out, &osrc, &nsrc, opts);
}

int git_diff_blob_to_buffer(
	const git_blob *old_blob,
	const char *old_path,
	const char *buf,
	size_t buflen,
	const char *buf_path,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(buf, buflen, buf_path);
	return diff_from_sources(
		&osrc, &nsrc, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
}

int git_patch_from_blob_and_buffer(
	git_patch **out,
	const git_blob *old_blob,
	const char *old_path,
	const void *buf,
	size_t buflen,
	const char *buf_path,
	const git_diff_options *opts)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(buf, buflen, buf_path);
	return patch_from_sources(out, &osrc, &nsrc, opts);
}

int git_diff_buffers(
	const void *old_buf,
	size_t old_len,
	const char *old_path,
	const void *new_buf,
	size_t new_len,
	const char *new_path,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(old_buf, old_len, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(new_buf, new_len, new_path);
	return diff_from_sources(
		&osrc, &nsrc, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
}

int git_patch_from_buffers(
	git_patch **out,
	const void *old_buf,
	size_t old_len,
	const char *old_path,
	const void *new_buf,
	size_t new_len,
	const char *new_path,
	const git_diff_options *opts)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(old_buf, old_len, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(new_buf, new_len, new_path);
	return patch_from_sources(out, &osrc, &nsrc, opts);
}

int git_patch_generated_from_diff(
	git_patch **patch_ptr, git_diff *diff, size_t idx)
{
	int error = 0;
	git_xdiff_output xo;
	git_diff_delta *delta = NULL;
	git_patch_generated *patch = NULL;

	if (patch_ptr) *patch_ptr = NULL;

	if (diff_required(diff, "git_patch_from_diff") < 0)
		return -1;

	delta = git_vector_get(&diff->deltas, idx);
	if (!delta) {
		git_error_set(GIT_ERROR_INVALID, "index out of range for delta in diff");
		return GIT_ENOTFOUND;
	}

	if (git_diff_delta__should_skip(&diff->opts, delta))
		return 0;

	/* don't load the patch data unless we need it for binary check */
	if (!patch_ptr &&
		((delta->flags & DIFF_FLAGS_KNOWN_BINARY) != 0 ||
		 (diff->opts.flags & GIT_DIFF_SKIP_BINARY_CHECK) != 0))
		return 0;

	if ((error = patch_generated_alloc_from_diff(&patch, diff, idx)) < 0)
		return error;

	memset(&xo, 0, sizeof(xo));
	diff_output_to_patch(&xo.output, patch);
	git_xdiff_init(&xo, &diff->opts);

	error = patch_generated_invoke_file_callback(patch, &xo.output);

	if (!error)
		error = patch_generated_create(patch, &xo.output);

	if (!error) {
		/* TODO: if cumulative diff size is < 0.5 total size, flatten patch */
		/* TODO: and unload the file content */
	}

	if (error || !patch_ptr)
		git_patch_free(&patch->base);
	else
		*patch_ptr = &patch->base;

	return error;
}

git_diff_driver *git_patch_generated_driver(git_patch_generated *patch)
{
	/* ofile driver is representative for whole patch */
	return patch->ofile.driver;
}

int git_patch_generated_old_data(
	char **ptr, long *len, git_patch_generated *patch)
{
	if (patch->ofile.map.len > LONG_MAX ||
	    patch->ofile.map.len > GIT_XDIFF_MAX_SIZE) {
		git_error_set(GIT_ERROR_INVALID, "files too large for diff");
		return -1;
	}

	*ptr = patch->ofile.map.data;
	*len = (long)patch->ofile.map.len;

	return 0;
}

int git_patch_generated_new_data(
	char **ptr, long *len, git_patch_generated *patch)
{
	if (patch->ofile.map.len > LONG_MAX ||
	    patch->ofile.map.len > GIT_XDIFF_MAX_SIZE) {
		git_error_set(GIT_ERROR_INVALID, "files too large for diff");
		return -1;
	}

	*ptr = patch->nfile.map.data;
	*len = (long)patch->nfile.map.len;

	return 0;
}

static int patch_generated_file_cb(
	const git_diff_delta *delta,
	float progress,
	void *payload)
{
	GIT_UNUSED(delta); GIT_UNUSED(progress); GIT_UNUSED(payload);
	return 0;
}

static int patch_generated_binary_cb(
	const git_diff_delta *delta,
	const git_diff_binary *binary,
	void *payload)
{
	git_patch *patch = payload;

	GIT_UNUSED(delta);

	memcpy(&patch->binary, binary, sizeof(git_diff_binary));

	if (binary->old_file.data) {
		patch->binary.old_file.data = git__malloc(binary->old_file.datalen);
		GIT_ERROR_CHECK_ALLOC(patch->binary.old_file.data);

		memcpy((char *)patch->binary.old_file.data,
			binary->old_file.data, binary->old_file.datalen);
	}

	if (binary->new_file.data) {
		patch->binary.new_file.data = git__malloc(binary->new_file.datalen);
		GIT_ERROR_CHECK_ALLOC(patch->binary.new_file.data);

		memcpy((char *)patch->binary.new_file.data,
			binary->new_file.data, binary->new_file.datalen);
	}

	return 0;
}

static int git_patch_hunk_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk_,
	void *payload)
{
	git_patch_generated  *patch = payload;
	git_patch_hunk *hunk;

	GIT_UNUSED(delta);

	hunk = git_array_alloc(patch->base.hunks);
	GIT_ERROR_CHECK_ALLOC(hunk);

	memcpy(&hunk->hunk, hunk_, sizeof(hunk->hunk));

	patch->base.header_size += hunk_->header_len;

	hunk->line_start = git_array_size(patch->base.lines);
	hunk->line_count = 0;

	return 0;
}

static int patch_generated_line_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk_,
	const git_diff_line *line_,
	void *payload)
{
	git_patch_generated  *patch = payload;
	git_patch_hunk *hunk;
	git_diff_line *line;

	GIT_UNUSED(delta);
	GIT_UNUSED(hunk_);

	hunk = git_array_last(patch->base.hunks);
	GIT_ASSERT(hunk); /* programmer error if no hunk is available */

	line = git_array_alloc(patch->base.lines);
	GIT_ERROR_CHECK_ALLOC(line);

	memcpy(line, line_, sizeof(*line));

	/* do some bookkeeping so we can provide old/new line numbers */

	patch->base.content_size += line->content_len;

	if (line->origin == GIT_DIFF_LINE_ADDITION ||
		line->origin == GIT_DIFF_LINE_DELETION)
		patch->base.content_size += 1;
	else if (line->origin == GIT_DIFF_LINE_CONTEXT) {
		patch->base.content_size += 1;
		patch->base.context_size += line->content_len + 1;
	} else if (line->origin == GIT_DIFF_LINE_CONTEXT_EOFNL)
		patch->base.context_size += line->content_len;

	hunk->line_count++;

	return 0;
}

static void diff_output_init(
	git_patch_generated_output *out,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	GIT_UNUSED(opts);

	memset(out, 0, sizeof(*out));

	out->file_cb = file_cb;
	out->binary_cb = binary_cb;
	out->hunk_cb = hunk_cb;
	out->data_cb = data_cb;
	out->payload = payload;
}

static void diff_output_to_patch(
	git_patch_generated_output *out, git_patch_generated *patch)
{
	diff_output_init(
		out,
		NULL,
		patch_generated_file_cb,
		patch_generated_binary_cb,
		git_patch_hunk_cb,
		patch_generated_line_cb,
		patch);
}
