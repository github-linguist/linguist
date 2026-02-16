/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_commit_graph_h__
#define INCLUDE_commit_graph_h__

#include "common.h"

#include "git2/types.h"
#include "git2/sys/commit_graph.h"

#include "map.h"
#include "vector.h"
#include "oid.h"
#include "hash.h"

/**
 * A commit-graph file.
 *
 * This file contains metadata about commits, particularly the generation
 * number for each one. This can help speed up graph operations without
 * requiring a full graph traversal.
 *
 * Support for this feature was added in git 2.19.
 */
typedef struct git_commit_graph_file {
	git_map graph_map;

	/* The type of object IDs in the commit graph file. */
	git_oid_t oid_type;

	/* The OID Fanout table. */
	const uint32_t *oid_fanout;
	/* The total number of commits in the graph. */
	uint32_t num_commits;

	/* The OID Lookup table. */
	unsigned char *oid_lookup;

	/*
	 * The Commit Data table. Each entry contains the OID of the commit followed
	 * by two 8-byte fields in network byte order:
	 * - The indices of the first two parents (32 bits each).
	 * - The generation number (first 30 bits) and commit time in seconds since
	 *   UNIX epoch (34 bits).
	 */
	const unsigned char *commit_data;

	/*
	 * The Extra Edge List table. Each 4-byte entry is a network byte order index
	 * of one of the i-th (i > 0) parents of commits in the `commit_data` table,
	 * when the commit has more than 2 parents.
	 */
	const unsigned char *extra_edge_list;
	/* The number of entries in the Extra Edge List table. Each entry is 4 bytes wide. */
	size_t num_extra_edge_list;

	/* The trailer of the file. Contains the SHA1-checksum of the whole file. */
	unsigned char checksum[GIT_HASH_SHA1_SIZE];
} git_commit_graph_file;

/**
 * An entry in the commit-graph file. Provides a subset of the information that
 * can be obtained from the commit header.
 */
typedef struct git_commit_graph_entry {
	/* The generation number of the commit within the graph */
	size_t generation;

	/* Time in seconds from UNIX epoch. */
	git_time_t commit_time;

	/* The number of parents of the commit. */
	size_t parent_count;

	/*
	 * The indices of the parent commits within the Commit Data table. The value
	 * of `GIT_COMMIT_GRAPH_MISSING_PARENT` indicates that no parent is in that
	 * position.
	 */
	size_t parent_indices[2];

	/* The index within the Extra Edge List of any parent after the first two. */
	size_t extra_parents_index;

	/* The object ID of the root tree of the commit. */
	git_oid tree_oid;

	/* The object ID hash of the requested commit. */
	git_oid sha1;
} git_commit_graph_entry;

/* A wrapper for git_commit_graph_file to enable lazy loading in the ODB. */
struct git_commit_graph {
	/* The path to the commit-graph file. Something like ".git/objects/info/commit-graph". */
	git_str filename;

	/* The underlying commit-graph file. */
	git_commit_graph_file *file;

	/* The object ID types in the commit graph. */
	git_oid_t oid_type;

	/* Whether the commit-graph file was already checked for validity. */
	bool checked;
};

/** Create a new commit-graph, optionally opening the underlying file. */
int git_commit_graph_new(
	git_commit_graph **cgraph_out,
	const char *objects_dir,
	bool open_file,
	git_oid_t oid_type);

/** Validate the checksum of a commit graph */
int git_commit_graph_validate(git_commit_graph *cgraph);

/** Open and validate a commit-graph file. */
int git_commit_graph_file_open(
	git_commit_graph_file **file_out,
	const char *path,
	git_oid_t oid_type);

/*
 * Attempt to get the git_commit_graph's commit-graph file. This object is
 * still owned by the git_commit_graph. If the repository does not contain a commit graph,
 * it will return GIT_ENOTFOUND.
 *
 * This function is not thread-safe.
 */
int git_commit_graph_get_file(git_commit_graph_file **file_out, git_commit_graph *cgraph);

/* Marks the commit-graph file as needing a refresh. */
void git_commit_graph_refresh(git_commit_graph *cgraph);

/*
 * A writer for `commit-graph` files.
 */
struct git_commit_graph_writer {
	/*
	 * The path of the `objects/info` directory where the `commit-graph` will be
	 * stored.
	 */
	git_str objects_info_dir;

	/* The object ID type of the commit graph. */
	git_oid_t oid_type;

	/* The list of packed commits. */
	git_vector commits;
};

int git_commit_graph__writer_dump(
	git_str *cgraph,
	git_commit_graph_writer *w);

/*
 * Returns whether the git_commit_graph_file needs to be reloaded since the
 * contents of the commit-graph file have changed on disk.
 */
bool git_commit_graph_file_needs_refresh(
		const git_commit_graph_file *file, const char *path);

int git_commit_graph_entry_find(
		git_commit_graph_entry *e,
		const git_commit_graph_file *file,
		const git_oid *short_oid,
		size_t len);
int git_commit_graph_entry_parent(
		git_commit_graph_entry *parent,
		const git_commit_graph_file *file,
		const git_commit_graph_entry *entry,
		size_t n);
int git_commit_graph_file_close(git_commit_graph_file *cgraph);
void git_commit_graph_file_free(git_commit_graph_file *cgraph);

/* This is exposed for use in the fuzzers. */
int git_commit_graph_file_parse(
		git_commit_graph_file *file,
		const unsigned char *data,
		size_t size);

#endif
