/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_reader_h__
#define INCLUDE_reader_h__

#include "common.h"

/* Returned when the workdir does not match the index */
#define GIT_READER_MISMATCH	1

typedef struct git_reader git_reader;

/*
 * The `git_reader` structure is a generic interface for reading the
 * contents of a file by its name, and implementations are provided
 * for reading out of a tree, the index, and the working directory.
 *
 * Note that the reader implementation is meant to have a short
 * lifecycle and does not increase the refcount of the object that
 * it's reading.  Callers should ensure that they do not use a
 * reader after disposing the underlying object that it reads.
 */
struct git_reader {
	int (*read)(git_str *out, git_oid *out_oid, git_filemode_t *mode, git_reader *reader, const char *filename);
};

/**
 * Create a `git_reader` that will allow random access to the given
 * tree.  Paths requested via `git_reader_read` will be rooted at this
 * tree, callers are not expected to recurse through tree lookups.  Thus,
 * you can request to read `/src/foo.c` and the tree provided to this
 * function will be searched to find another tree named `src`, which
 * will then be opened to find `foo.c`.
 *
 * @param out The reader for the given tree
 * @param tree The tree object to read
 * @return 0 on success, or an error code < 0
 */
extern int git_reader_for_tree(
	git_reader **out,
	git_tree *tree);

/**
 * Create a `git_reader` that will allow random access to the given
 * index, or the repository's index.
 *
 * @param out The reader for the given index
 * @param repo The repository containing the index
 * @param index The index to read, or NULL to use the repository's index
 * @return 0 on success, or an error code < 0
 */
extern int git_reader_for_index(
	git_reader **out,
	git_repository *repo,
	git_index *index);

/**
 * Create a `git_reader` that will allow random access to the given
 * repository's working directory.  Note that the contents are read
 * in repository format, meaning any workdir -> odb filters are
 * applied.
 *
 * If `validate_index` is set to true, reads of files will hash the
 * on-disk contents and ensure that the resulting object ID matches
 * the repository's index.  This ensures that the working directory
 * is unmodified from the index contents.
 *
 * @param out The reader for the given working directory
 * @param repo The repository containing the working directory
 * @param validate_index If true, the working directory contents will
 *        be compared to the index contents during read to ensure that
 *        the working directory is unmodified.
 * @return 0 on success, or an error code < 0
 */
extern int git_reader_for_workdir(
	git_reader **out,
	git_repository *repo,
	bool validate_index);

/**
 * Read the given filename from the reader and populate the given buffer
 * with the contents and the given oid with the object ID.
 *
 * @param out The buffer to populate with the file contents
 * @param out_id The oid to populate with the object ID
 * @param reader The reader to read
 * @param filename The filename to read from the reader
 */
extern int git_reader_read(
	git_str *out,
	git_oid *out_id,
	git_filemode_t *out_filemode,
	git_reader *reader,
	const char *filename);

/**
 * Free the given reader and any associated objects.
 *
 * @param reader The reader to free
 */
extern void git_reader_free(git_reader *reader);

#endif
