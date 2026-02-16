/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_buf_h__
#define INCLUDE_buf_h__

#include "git2/buffer.h"
#include "common.h"

/*
 * Adapts a private API that takes a `git_str` into a public API that
 * takes a `git_buf`.
 */

#define GIT_BUF_WRAP_PRIVATE(buf, fn, ...) \
  { \
	git_str str = GIT_STR_INIT; \
	int error; \
	if ((error = git_buf_tostr(&str, buf)) == 0 && \
	    (error = fn(&str, __VA_ARGS__)) == 0) \
		error = git_buf_fromstr(buf, &str); \
	git_str_dispose(&str); \
	return error; \
}

/**
 * "Sanitizes" a buffer from user input.  This simply ensures that the
 * `git_buf` has nice defaults if the user didn't set the members to
 * anything, so that if we return early we don't leave it populated
 * with nonsense.
 */
extern int git_buf_sanitize(git_buf *from_user);

/**
 * Populate a `git_str` from a `git_buf` for passing to libgit2 internal
 * functions.  Sanitizes the given `git_buf` before proceeding.  The
 * `git_buf` will no longer point to this memory.
 */
extern int git_buf_tostr(git_str *out, git_buf *buf);

/**
 * Populate a `git_buf` from a `git_str` for returning to a user.
 * The `git_str` will no longer point to this memory.
 */
extern int git_buf_fromstr(git_buf *out, git_str *str);

#endif
