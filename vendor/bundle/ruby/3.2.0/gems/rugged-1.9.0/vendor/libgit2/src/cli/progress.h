/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef CLI_progress_h__
#define CLI_progress_h__

#include <git2.h>
#include "str.h"

/*
 * A general purpose set of progress printing functions.  An individual
 * `cli_progress` object is capable of displaying progress for a single
 * function, even if that function displays multiple pieces of progress
 * (like `git_clone`).  `cli_progress_finish` should be called after
 * any function invocation to re-set state.
 */

typedef enum {
	CLI_PROGRESS_NONE,
	CLI_PROGRESS_RECEIVING,
	CLI_PROGRESS_INDEXING,
	CLI_PROGRESS_RESOLVING,
	CLI_PROGRESS_CHECKING_OUT
} cli_progress_t;

typedef struct {
	cli_progress_t action;

	/* Actions may time themselves (eg fetch) but are not required to */
	uint64_t action_start;
	uint64_t action_finish;

	/* Last console update, avoid too frequent updates. */
	uint64_t last_update;

	/* Accumulators for partial output and deferred updates. */
	git_str sideband;
	git_str onscreen;
	git_str deferred;

	/* Last update about throughput */
	uint64_t throughput_update;
	double throughput_bytes;
} cli_progress;

#define CLI_PROGRESS_INIT { 0 }

/**
 * Prints sideband data from fetch to the console.  Suitable for a
 * `sideband_progress` callback for `git_fetch_options`.
 *
 * @param str The sideband string
 * @param len The length of the sideband string
 * @param payload A pointer to the cli_progress
 * @return 0 on success, -1 on failure
 */
extern int cli_progress_fetch_sideband(
	const char *str,
	int len,
	void *payload);

/**
 * Prints fetch transfer statistics to the console.  Suitable for a
 * `transfer_progress` callback for `git_fetch_options`.
 *
 * @param stats The indexer stats
 * @param payload A pointer to the cli_progress
 * @return 0 on success, -1 on failure
 */
extern int cli_progress_fetch_transfer(
	const git_indexer_progress *stats,
	void *payload);

/**
 * Prints indexer progress to the console. Suitable for a
 * `progress_cb` callback for `git_indexer_options`.
 *
 * @param stats The indexer stats
 * @param payload A pointer to the cli_progress
 */
extern int cli_progress_indexer(
	const git_indexer_progress *stats,
	void *payload);

/**
 * Prints checkout progress to the console.  Suitable for a
 * `progress_cb` callback for `git_checkout_options`.
 *
 * @param path The path being written
 * @param completed_steps The completed checkout steps
 * @param total_steps The total number of checkout steps
 * @param payload A pointer to the cli_progress
 */
extern void cli_progress_checkout(
	const char *path,
	size_t completed_steps,
	size_t total_steps,
	void *payload);

/**
 * Stop displaying progress quickly; suitable for stopping an application
 * quickly.  Does not display any lines that were buffered, just gets the
 * console back to a sensible place.
 *
 * @param progress The progress information
 * @return 0 on success, -1 on failure
 */
extern int cli_progress_abort(cli_progress *progress);

/**
 * Finishes displaying progress; flushes any buffered output.
 *
 * @param progress The progress information
 * @return 0 on success, -1 on failure
 */
extern int cli_progress_finish(cli_progress *progress);

/**
 * Disposes the progress information.
 *
 * @param progress The progress information
 */
extern void cli_progress_dispose(cli_progress *progress);

#endif /* CLI_progress_h__ */
