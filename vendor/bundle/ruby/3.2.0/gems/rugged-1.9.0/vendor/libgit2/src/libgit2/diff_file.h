/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_diff_file_h__
#define INCLUDE_diff_file_h__

#include "common.h"

#include "diff.h"
#include "diff_driver.h"
#include "map.h"

/* expanded information for one side of a delta */
typedef struct {
	git_repository *repo;
	git_diff_file *file;
	git_diff_driver *driver;
	uint32_t flags;
	uint32_t opts_flags;
	git_object_size_t opts_max_size;
	git_iterator_t src;
	const git_blob *blob;
	git_map map;
} git_diff_file_content;

extern int git_diff_file_content__init_from_diff(
	git_diff_file_content *fc,
	git_diff *diff,
	git_diff_delta *delta,
	bool use_old);

typedef struct {
	const git_blob *blob;
	const void *buf;
	size_t buflen;
	const char *as_path;
} git_diff_file_content_src;

#define GIT_DIFF_FILE_CONTENT_SRC__BLOB(BLOB,PATH) { (BLOB),NULL,0,(PATH) }
#define GIT_DIFF_FILE_CONTENT_SRC__BUF(BUF,LEN,PATH) { NULL,(BUF),(LEN),(PATH) }

extern int git_diff_file_content__init_from_src(
	git_diff_file_content *fc,
	git_repository *repo,
	const git_diff_options *opts,
	const git_diff_file_content_src *src,
	git_diff_file *as_file);

/* this loads the blob/file-on-disk as needed */
extern int git_diff_file_content__load(
	git_diff_file_content *fc,
	git_diff_options *diff_opts);

/* this releases the blob/file-in-memory */
extern void git_diff_file_content__unload(git_diff_file_content *fc);

/* this unloads and also releases any other resources */
extern void git_diff_file_content__clear(git_diff_file_content *fc);

#endif
