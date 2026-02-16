/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "repository.h"
#include "posix.h"
#include "futils.h"
#include "index.h"
#include "diff_xdiff.h"
#include "merge.h"

#include "git2/repository.h"
#include "git2/object.h"
#include "git2/index.h"
#include "git2/merge.h"

/* only examine the first 8000 bytes for binaryness.
 * https://github.com/git/git/blob/77bd3ea9f54f1584147b594abc04c26ca516d987/xdiff-interface.c#L197
 */
#define GIT_MERGE_FILE_BINARY_SIZE 8000

#define GIT_MERGE_FILE_SIDE_EXISTS(X)	((X)->mode != 0)

static int merge_file_input_from_index(
	git_merge_file_input *input_out,
	git_odb_object **odb_object_out,
	git_odb *odb,
	const git_index_entry *entry)
{
	int error = 0;

	GIT_ASSERT_ARG(input_out);
	GIT_ASSERT_ARG(odb_object_out);
	GIT_ASSERT_ARG(odb);
	GIT_ASSERT_ARG(entry);

	if ((error = git_odb_read(odb_object_out, odb, &entry->id)) < 0)
		goto done;

	input_out->path = entry->path;
	input_out->mode = entry->mode;
	input_out->ptr = (char *)git_odb_object_data(*odb_object_out);
	input_out->size = git_odb_object_size(*odb_object_out);

done:
	return error;
}

static void merge_file_normalize_opts(
	git_merge_file_options *out,
	const git_merge_file_options *given_opts)
{
	if (given_opts)
		memcpy(out, given_opts, sizeof(git_merge_file_options));
	else {
		git_merge_file_options default_opts = GIT_MERGE_FILE_OPTIONS_INIT;
		memcpy(out, &default_opts, sizeof(git_merge_file_options));
	}
}

static int merge_file__xdiff(
	git_merge_file_result *out,
	const git_merge_file_input *ancestor,
	const git_merge_file_input *ours,
	const git_merge_file_input *theirs,
	const git_merge_file_options *given_opts)
{
	xmparam_t xmparam;
	mmfile_t ancestor_mmfile = {0}, our_mmfile = {0}, their_mmfile = {0};
	mmbuffer_t mmbuffer;
	git_merge_file_options options = GIT_MERGE_FILE_OPTIONS_INIT;
	const char *path;
	int xdl_result;
	int error = 0;

	memset(out, 0x0, sizeof(git_merge_file_result));

	merge_file_normalize_opts(&options, given_opts);

	memset(&xmparam, 0x0, sizeof(xmparam_t));

	if (ours->size > LONG_MAX ||
	    theirs->size > LONG_MAX ||
	    (ancestor && ancestor->size > LONG_MAX)) {
		git_error_set(GIT_ERROR_MERGE, "failed to merge files");
		error = -1;
		goto done;
	}

	if (ancestor) {
		xmparam.ancestor = (options.ancestor_label) ?
			options.ancestor_label : ancestor->path;
		ancestor_mmfile.ptr = (char *)ancestor->ptr;
		ancestor_mmfile.size = (long)ancestor->size;
	}

	xmparam.file1 = (options.our_label) ?
		options.our_label : ours->path;
	our_mmfile.ptr = (char *)ours->ptr;
	our_mmfile.size = (long)ours->size;

	xmparam.file2 = (options.their_label) ?
		options.their_label : theirs->path;
	their_mmfile.ptr = (char *)theirs->ptr;
	their_mmfile.size = (long)theirs->size;

	if (options.favor == GIT_MERGE_FILE_FAVOR_OURS)
		xmparam.favor = XDL_MERGE_FAVOR_OURS;
	else if (options.favor == GIT_MERGE_FILE_FAVOR_THEIRS)
		xmparam.favor = XDL_MERGE_FAVOR_THEIRS;
	else if (options.favor == GIT_MERGE_FILE_FAVOR_UNION)
		xmparam.favor = XDL_MERGE_FAVOR_UNION;

	xmparam.level = (options.flags & GIT_MERGE_FILE_SIMPLIFY_ALNUM) ?
		XDL_MERGE_ZEALOUS_ALNUM : XDL_MERGE_ZEALOUS;

	if (options.flags & GIT_MERGE_FILE_STYLE_DIFF3)
		xmparam.style = XDL_MERGE_DIFF3;
	if (options.flags & GIT_MERGE_FILE_STYLE_ZDIFF3)
		xmparam.style = XDL_MERGE_ZEALOUS_DIFF3;

	if (options.flags & GIT_MERGE_FILE_IGNORE_WHITESPACE)
		xmparam.xpp.flags |= XDF_IGNORE_WHITESPACE;
	if (options.flags & GIT_MERGE_FILE_IGNORE_WHITESPACE_CHANGE)
		xmparam.xpp.flags |= XDF_IGNORE_WHITESPACE_CHANGE;
	if (options.flags & GIT_MERGE_FILE_IGNORE_WHITESPACE_EOL)
		xmparam.xpp.flags |= XDF_IGNORE_WHITESPACE_AT_EOL;

	if (options.flags & GIT_MERGE_FILE_DIFF_PATIENCE)
		xmparam.xpp.flags |= XDF_PATIENCE_DIFF;

	if (options.flags & GIT_MERGE_FILE_DIFF_MINIMAL)
		xmparam.xpp.flags |= XDF_NEED_MINIMAL;

	xmparam.marker_size = options.marker_size;

	if ((xdl_result = xdl_merge(&ancestor_mmfile, &our_mmfile,
		&their_mmfile, &xmparam, &mmbuffer)) < 0) {
		git_error_set(GIT_ERROR_MERGE, "failed to merge files");
		error = -1;
		goto done;
	}

	path = git_merge_file__best_path(
		ancestor ? ancestor->path : NULL,
		ours->path,
		theirs->path);

	if (path != NULL && (out->path = git__strdup(path)) == NULL) {
		error = -1;
		goto done;
	}

	out->automergeable = (xdl_result == 0);
	out->ptr = (const char *)mmbuffer.ptr;
	out->len = mmbuffer.size;
	out->mode = git_merge_file__best_mode(
		ancestor ? ancestor->mode : 0,
		ours->mode,
		theirs->mode);

done:
	if (error < 0)
		git_merge_file_result_free(out);

	return error;
}

static bool merge_file__is_binary(const git_merge_file_input *file)
{
	size_t len = file ? file->size : 0;

	if (len > GIT_XDIFF_MAX_SIZE)
		return true;
	if (len > GIT_MERGE_FILE_BINARY_SIZE)
		len = GIT_MERGE_FILE_BINARY_SIZE;

	return len ? (memchr(file->ptr, 0, len) != NULL) : false;
}

static int merge_file__binary(
	git_merge_file_result *out,
	const git_merge_file_input *ours,
	const git_merge_file_input *theirs,
	const git_merge_file_options *given_opts)
{
	const git_merge_file_input *favored = NULL;

	memset(out, 0x0, sizeof(git_merge_file_result));

	if (given_opts && given_opts->favor == GIT_MERGE_FILE_FAVOR_OURS)
		favored = ours;
	else if (given_opts && given_opts->favor == GIT_MERGE_FILE_FAVOR_THEIRS)
		favored = theirs;
	else
		goto done;

	if ((out->path = git__strdup(favored->path)) == NULL ||
		(out->ptr = git__malloc(favored->size)) == NULL)
		goto done;

	memcpy((char *)out->ptr, favored->ptr, favored->size);
	out->len = favored->size;
	out->mode = favored->mode;
	out->automergeable = 1;

done:
	return 0;
}

static int merge_file__from_inputs(
	git_merge_file_result *out,
	const git_merge_file_input *ancestor,
	const git_merge_file_input *ours,
	const git_merge_file_input *theirs,
	const git_merge_file_options *given_opts)
{
	if (merge_file__is_binary(ancestor) ||
		merge_file__is_binary(ours) ||
		merge_file__is_binary(theirs))
		return merge_file__binary(out, ours, theirs, given_opts);

	return merge_file__xdiff(out, ancestor, ours, theirs, given_opts);
}

static git_merge_file_input *git_merge_file__normalize_inputs(
	git_merge_file_input *out,
	const git_merge_file_input *given)
{
	memcpy(out, given, sizeof(git_merge_file_input));

	if (!out->path)
		out->path = "file.txt";

	if (!out->mode)
		out->mode = 0100644;

	return out;
}

int git_merge_file(
	git_merge_file_result *out,
	const git_merge_file_input *ancestor,
	const git_merge_file_input *ours,
	const git_merge_file_input *theirs,
	const git_merge_file_options *options)
{
	git_merge_file_input inputs[3] = { {0} };

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(ours);
	GIT_ASSERT_ARG(theirs);

	memset(out, 0x0, sizeof(git_merge_file_result));

	if (ancestor)
		ancestor = git_merge_file__normalize_inputs(&inputs[0], ancestor);

	ours = git_merge_file__normalize_inputs(&inputs[1], ours);
	theirs = git_merge_file__normalize_inputs(&inputs[2], theirs);

	return merge_file__from_inputs(out, ancestor, ours, theirs, options);
}

int git_merge_file_from_index(
	git_merge_file_result *out,
	git_repository *repo,
	const git_index_entry *ancestor,
	const git_index_entry *ours,
	const git_index_entry *theirs,
	const git_merge_file_options *options)
{
	git_merge_file_input *ancestor_ptr = NULL,
		ancestor_input = {0}, our_input = {0}, their_input = {0};
	git_odb *odb = NULL;
	git_odb_object *odb_object[3] = { 0 };
	int error = 0;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(ours);
	GIT_ASSERT_ARG(theirs);

	memset(out, 0x0, sizeof(git_merge_file_result));

	if ((error = git_repository_odb(&odb, repo)) < 0)
		goto done;

	if (ancestor) {
		if ((error = merge_file_input_from_index(
			&ancestor_input, &odb_object[0], odb, ancestor)) < 0)
			goto done;

		ancestor_ptr = &ancestor_input;
	}

	if ((error = merge_file_input_from_index(&our_input, &odb_object[1], odb, ours)) < 0 ||
	    (error = merge_file_input_from_index(&their_input, &odb_object[2], odb, theirs)) < 0)
		goto done;

	error = merge_file__from_inputs(out,
		ancestor_ptr, &our_input, &their_input, options);

done:
	git_odb_object_free(odb_object[0]);
	git_odb_object_free(odb_object[1]);
	git_odb_object_free(odb_object[2]);
	git_odb_free(odb);

	return error;
}

void git_merge_file_result_free(git_merge_file_result *result)
{
	if (result == NULL)
		return;

	git__free((char *)result->path);
	git__free((char *)result->ptr);
}
