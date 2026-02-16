/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_runtime_h__
#define INCLUDE_runtime_h__

#include "git2_util.h"

typedef int (*git_runtime_init_fn)(void);
typedef void (*git_runtime_shutdown_fn)(void);

/**
 * Start up a new runtime.  If this is the first time that this
 * function is called within the context of the current library
 * or executable, then the given `init_fns` will be invoked.  If
 * it is not the first time, they will be ignored.
 *
 * The given initialization functions _may_ register shutdown
 * handlers using `git_runtime_shutdown_register` to be notified
 * when the runtime is shutdown.
 *
 * @param init_fns The list of initialization functions to call
 * @param cnt The number of init_fns
 * @return The number of initializations performed (including this one) or an error
 */
int git_runtime_init(git_runtime_init_fn init_fns[], size_t cnt);

/*
 * Returns the number of initializations active (the number of calls to
 * `git_runtime_init` minus the number of calls sto `git_runtime_shutdown`).
 * If 0, the runtime is not currently initialized.
 *
 * @return The number of initializations performed or an error
 */
int git_runtime_init_count(void);

/**
 * Shut down the runtime.  If this is the last shutdown call,
 * such that there are no remaining `init` calls, then any
 * shutdown hooks that have been registered will be invoked.
 *
 * The number of outstanding initializations will be returned.
 * If this number is 0, then the runtime is shutdown.
 *
 * @return The number of outstanding initializations (after this one) or an error
 */
int git_runtime_shutdown(void);

/**
 * Register a shutdown handler for this runtime.  This should be done
 * by a function invoked by `git_runtime_init` to ensure that the
 * appropriate locks are taken.
 *
 * @param callback The shutdown handler callback
 * @return 0 or an error code
 */
int git_runtime_shutdown_register(git_runtime_shutdown_fn callback);

#endif
