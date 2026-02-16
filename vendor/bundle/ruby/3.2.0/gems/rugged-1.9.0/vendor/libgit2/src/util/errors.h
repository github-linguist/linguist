/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_errors_h__
#define INCLUDE_errors_h__

#include "git2_util.h"
#include "git2/sys/errors.h"

/* Initialize the error thread-state. */
int git_error_global_init(void);

/*
 * `vprintf`-style formatting for the error message for this thread.
 */
void git_error_vset(int error_class, const char *fmt, va_list ap);

/**
 * Determines whether an error exists.
 */
bool git_error_exists(void);

/**
 * Set error message for user callback if needed.
 *
 * If the error code in non-zero and no error message is set, this
 * sets a generic error message.
 *
 * @return This always returns the `error_code` parameter.
 */
GIT_INLINE(int) git_error_set_after_callback_function(
	int error_code, const char *action)
{
	if (error_code) {
		if (!git_error_exists())
			git_error_set(GIT_ERROR_CALLBACK,
				"%s callback returned %d", action, error_code);
	}
	return error_code;
}

#ifdef GIT_WIN32
#define git_error_set_after_callback(code) \
	git_error_set_after_callback_function((code), __FUNCTION__)
#else
#define git_error_set_after_callback(code) \
	git_error_set_after_callback_function((code), __func__)
#endif

/**
 * Gets the system error code for this thread.
 */
int git_error_system_last(void);

/**
 * Sets the system error code for this thread.
 */
void git_error_system_set(int code);

/**
 * Capture current error state to restore later, returning error code.
 * If `error_code` is zero, this does not clear the current error state.
 * You must either restore this error state, or free it.
 *
 * This function returns 0 on success, or -1 on failure. If the function
 * fails, the `out` structure is set to the failure error message and
 * the normal system error message is not updated.
 */
extern int git_error_save(git_error **out);

/**
 * Restore thread error state to the given value. The given value is
 * freed and `git_error_free` need not be called on it.
 */
extern int git_error_restore(git_error *error);

/** Free an error state. */
extern void git_error_free(git_error *error);

#endif
