/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_sysdir_h__
#define INCLUDE_sysdir_h__

#include "common.h"

#include "posix.h"
#include "str.h"

/**
 * Find a "global" file (i.e. one in a user's home directory).
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the home directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_global_file(git_str *path, const char *filename);

/**
 * Find an "XDG" file (i.e. one in user's XDG config path).
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the home directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_xdg_file(git_str *path, const char *filename);

/**
 * Find a "system" file (i.e. one shared for all users of the system).
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the home directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_system_file(git_str *path, const char *filename);

/**
 * Find a "ProgramData" file (i.e. one in %PROGRAMDATA%)
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the ProgramData directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_programdata_file(git_str *path, const char *filename);

/**
 * Find template directory.
 *
 * @param path buffer to write the full path into
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_template_dir(git_str *path);

/**
 * Find the home directory. On Windows, this will look at the `HOME`,
 * `HOMEPATH`, and `USERPROFILE` environment variables (in that order)
 * and return the first path that is set and exists. On other systems,
 * this will simply return the contents of the `HOME` environment variable.
 *
 * @param path buffer to write the full path into
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_homedir(git_str *path);

/**
 * Expand the name of a "global" file -- by default inside the user's
 * home directory, but can be overridden by the user configuration.
 * Unlike `find_global_file` (above), this makes no attempt to check
 * for the existence of the file, and is useful if you want the full
 * path regardless of existence.
 *
 * @param path buffer to write the full path into
 * @param filename name of file in the home directory
 * @return 0 on success or -1 on error
 */
extern int git_sysdir_expand_global_file(git_str *path, const char *filename);

/**
 * Expand the name of a file in the user's home directory. This
 * function makes no attempt to check for the existence of the file,
 * and is useful if you want the full path regardless of existence.
 *
 * @param path buffer to write the full path into
 * @param filename name of file in the home directory
 * @return 0 on success or -1 on error
 */
extern int git_sysdir_expand_homedir_file(git_str *path, const char *filename);

typedef enum {
	GIT_SYSDIR_SYSTEM      = 0,
	GIT_SYSDIR_GLOBAL      = 1,
	GIT_SYSDIR_XDG         = 2,
	GIT_SYSDIR_PROGRAMDATA = 3,
	GIT_SYSDIR_TEMPLATE    = 4,
	GIT_SYSDIR_HOME        = 5,
	GIT_SYSDIR__MAX        = 6
} git_sysdir_t;

/**
 * Configures global data for configuration file search paths.
 *
 * @return 0 on success, <0 on failure
 */
extern int git_sysdir_global_init(void);

/**
 * Get the search path for global/system/xdg files
 *
 * @param out pointer to git_str containing search path
 * @param which which list of paths to return
 * @return 0 on success, <0 on failure
 */
extern int git_sysdir_get(const git_str **out, git_sysdir_t which);

/**
 * Set search paths for global/system/xdg files
 *
 * The first occurrence of the magic string "$PATH" in the new value will
 * be replaced with the old value of the search path.
 *
 * @param which Which search path to modify
 * @param paths New search path (separated by GIT_PATH_LIST_SEPARATOR)
 * @return 0 on success, <0 on failure (allocation error)
 */
extern int git_sysdir_set(git_sysdir_t which, const char *paths);

/**
 * Reset search paths for global/system/xdg files.
 */
extern int git_sysdir_reset(void);

#ifdef GIT_WIN32
/** Sets the registry system dir to a mock; for testing.  */
extern int git_win32__set_registry_system_dir(const wchar_t *mock_sysdir);

/** Find the given system dir; for testing. */
extern int git_win32__find_system_dirs(git_str *out, const char *subdir);
#endif

#endif
