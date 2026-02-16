/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_push_h__
#define INCLUDE_push_h__

#include "common.h"

#include "git2.h"
#include "refspec.h"
#include "remote.h"

typedef struct push_spec {
	struct git_refspec refspec;

	git_oid loid;
	git_oid roid;
} push_spec;

typedef struct push_status {
	bool ok;

	char *ref;
	char *msg;
} push_status;

struct git_push {
	git_repository *repo;
	git_packbuilder *pb;
	git_remote *remote;
	git_vector specs;
	git_vector updates;
	bool report_status;
	git_vector remote_push_options;

	/* report-status */
	bool unpack_ok;
	git_vector status;

	/* options */
	unsigned pb_parallelism;
	git_remote_callbacks callbacks;
};

/**
 * Free the given push status object
 *
 * @param status The push status object
 */
void git_push_status_free(push_status *status);

/**
 * Create a new push object
 *
 * @param out New push object
 * @param remote Remote instance
 * @param opts Push options or NULL
 *
 * @return 0 or an error code
 */
int git_push_new(git_push **out, git_remote *remote, const git_push_options *opts);

/**
 * Add a refspec to be pushed
 *
 * @param push The push object
 * @param refspec Refspec string
 *
 * @return 0 or an error code
 */
int git_push_add_refspec(git_push *push, const char *refspec);

/**
 * Update remote tips after a push
 *
 * @param push The push object
 * @param callbacks the callbacks to use for this connection
 *
 * @return 0 or an error code
 */
int git_push_update_tips(git_push *push, const git_remote_callbacks *callbacks);

/**
 * Perform the push
 *
 * This function will return an error in case of a protocol error or
 * the server being unable to unpack the data we sent.
 *
 * The return value does not reflect whether the server accepted or
 * refused any reference updates. Use `git_push_status_foreach()` in
 * order to find out which updates were accepted or rejected.
 *
 * @param push The push object
 *
 * @return 0 or an error code
 */
int git_push_finish(git_push *push);

/**
 * Invoke callback `cb' on each status entry
 *
 * For each of the updated references, we receive a status report in the
 * form of `ok refs/heads/master` or `ng refs/heads/master <msg>`.
 * `msg != NULL` means the reference has not been updated for the given
 * reason.
 *
 * Return a non-zero value from the callback to stop the loop.
 *
 * @param push The push object
 * @param cb The callback to call on each object
 * @param data The payload passed to the callback
 *
 * @return 0 on success, non-zero callback return value, or error code
 */
int git_push_status_foreach(git_push *push,
			int (*cb)(const char *ref, const char *msg, void *data),
			void *data);

/**
 * Free the given push object
 *
 * @param push The push object
 */
void git_push_free(git_push *push);

#endif
