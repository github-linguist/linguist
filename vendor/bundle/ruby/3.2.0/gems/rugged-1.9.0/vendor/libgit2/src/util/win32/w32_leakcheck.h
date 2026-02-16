/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_win32_leakcheck_h__
#define INCLUDE_win32_leakcheck_h__

#include "git2_util.h"

/* Initialize the win32 leak checking system. */
int git_win32_leakcheck_global_init(void);

#if defined(GIT_WIN32_LEAKCHECK)

#include <stdlib.h>
#include <crtdbg.h>

#include "git2/errors.h"
#include "strnlen.h"

bool git_win32_leakcheck_has_leaks(void);

/* Stack frames (for stack tracing, below) */

/**
 * This type defines a callback to be used to augment a C stacktrace
 * with "aux" data. This can be used, for example, to allow LibGit2Sharp
 * (or other interpreted consumer libraries) to give us C# stacktrace
 * data for the PInvoke.
 *
 * This callback will be called during crtdbg-instrumented allocs.
 *
 * @param aux_id [out] A returned "aux_id" representing a unique
 * (de-duped at the C# layer) stacktrace.  "aux_id" 0 is reserved
 * to mean no aux stacktrace data.
 */
typedef void (*git_win32_leakcheck_stack_aux_cb_alloc)(unsigned int *aux_id);

/**
 * This type defines a callback to be used to augment the output of
 * a stacktrace.  This will be used to request the C# layer format
 * the C# stacktrace associated with "aux_id" into the provided
 * buffer.
 *
 * This callback will be called during leak reporting.
 *
 * @param aux_id The "aux_id" key associated with a stacktrace.
 * @param aux_msg A buffer where a formatted message should be written.
 * @param aux_msg_len The size of the buffer.
 */
typedef void (*git_win32_leakcheck_stack_aux_cb_lookup)(unsigned int aux_id, char *aux_msg, size_t aux_msg_len);

/**
 * Register an "aux" data provider to augment our C stacktrace data.
 *
 * This can be used, for example, to allow LibGit2Sharp (or other
 * interpreted consumer libraries) to give us the C# stacktrace of
 * the PInvoke.
 *
 * If you choose to use this feature, it should be registered during
 * initialization and not changed for the duration of the process.
 */
int git_win32_leakcheck_stack_set_aux_cb(
	git_win32_leakcheck_stack_aux_cb_alloc cb_alloc,
	git_win32_leakcheck_stack_aux_cb_lookup cb_lookup);

/**
 * Maximum number of stackframes to record for a
 * single stacktrace.
 */
#define GIT_WIN32_LEAKCHECK_STACK_MAX_FRAMES 30

/**
 * Wrapper containing the raw unprocessed stackframe
 * data for a single stacktrace and any "aux_id".
 *
 * I put the aux_id first so leaks will be sorted by it.
 * So, for example, if a specific callstack in C# leaks
 * a repo handle, all of the pointers within the associated
 * repo pointer will be grouped together.
 */
typedef struct {
	unsigned int aux_id;
	unsigned int nr_frames;
	void *frames[GIT_WIN32_LEAKCHECK_STACK_MAX_FRAMES];
} git_win32_leakcheck_stack_raw_data;

/**
 * Capture raw stack trace data for the current process/thread.
 *
 * @param skip Number of initial frames to skip.  Pass 0 to
 * begin with the caller of this routine. Pass 1 to begin
 * with its caller.  And so on.
 */
int git_win32_leakcheck_stack_capture(git_win32_leakcheck_stack_raw_data *pdata, int skip);

/**
 * Compare 2 raw stacktraces with the usual -1,0,+1 result.
 * This includes any "aux_id" values in the comparison, so that
 * our de-dup is also "aux" context relative.
 */
int git_win32_leakcheck_stack_compare(
	git_win32_leakcheck_stack_raw_data *d1,
	git_win32_leakcheck_stack_raw_data *d2);

/**
 * Format raw stacktrace data into buffer WITHOUT using any mallocs.
 *
 * @param prefix String written before each frame; defaults to "\t".
 * @param suffix String written after each frame; defaults to "\n".
 */
int git_win32_leakcheck_stack_format(
	char *pbuf, size_t buf_len,
	const git_win32_leakcheck_stack_raw_data *pdata,
	const char *prefix, const char *suffix);

/**
 * Convenience routine to capture and format stacktrace into
 * a buffer WITHOUT using any mallocs.  This is primarily a
 * wrapper for testing.
 *
 * @param skip Number of initial frames to skip. Pass 0 to
 * begin with the caller of this routine.  Pass 1 to begin
 * with its caller.  And so on.
 * @param prefix String written before each frame; defaults to "\t".
 * @param suffix String written after each frame; defaults to "\n".
 */
int git_win32_leakcheck_stack(
	char * pbuf, size_t buf_len,
	int skip,
	const char *prefix, const char *suffix);

/* Stack tracing */

/* MSVC CRTDBG memory leak reporting.
 *
 * We DO NOT use the "_CRTDBG_MAP_ALLOC" macro described in the MSVC
 * documentation because all allocs/frees in libgit2 already go through
 * the "git__" routines defined in this file.  Simply using the normal
 * reporting mechanism causes all leaks to be attributed to a routine
 * here in util.h (ie, the actual call to calloc()) rather than the
 * caller of git__calloc().
 *
 * Therefore, we declare a set of "git__crtdbg__" routines to replace
 * the corresponding "git__" routines and re-define the "git__" symbols
 * as macros.  This allows us to get and report the file:line info of
 * the real caller.
 *
 * We DO NOT replace the "git__free" routine because it needs to remain
 * a function pointer because it is used as a function argument when
 * setting up various structure "destructors".
 *
 * We also DO NOT use the "_CRTDBG_MAP_ALLOC" macro because it causes
 * "free" to be remapped to "_free_dbg" and this causes problems for
 * structures which define a field named "free".
 *
 * Finally, CRTDBG must be explicitly enabled and configured at program
 * startup.  See tests/main.c for an example.
 */

/**
 * Checkpoint options.
 */
typedef enum git_win32_leakcheck_stacktrace_options {
	/**
	 * Set checkpoint marker.
	 */
	GIT_WIN32_LEAKCHECK_STACKTRACE_SET_MARK         = (1 << 0),

	/**
	 * Dump leaks since last checkpoint marker.
	 * May not be combined with _LEAKS_TOTAL.
	 *
	 * Note that this may generate false positives for global TLS
	 * error state and other global caches that aren't cleaned up
	 * until the thread/process terminates.  So when using this
	 * around a region of interest, also check the final (at exit)
	 * dump before digging into leaks reported here.
	 */
	GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_SINCE_MARK = (1 << 1),

	/**
	 * Dump leaks since init.  May not be combined
	 * with _LEAKS_SINCE_MARK.
	 */
	GIT_WIN32_LEAKCHECK_STACKTRACE_LEAKS_TOTAL      = (1 << 2),

	/**
	 * Suppress printing during dumps.
	 * Just return leak count.
	 */
	GIT_WIN32_LEAKCHECK_STACKTRACE_QUIET            = (1 << 3),

} git_win32_leakcheck_stacktrace_options;

/**
 * Checkpoint memory state and/or dump unique stack traces of
 * current memory leaks.
 *
 * @return number of unique leaks (relative to requested starting
 * point) or error.
 */
int git_win32_leakcheck_stacktrace_dump(
	git_win32_leakcheck_stacktrace_options opt,
	const char *label);

/**
 * Construct stacktrace and append it to the global buffer.
 * Return pointer to start of this string.  On any error or
 * lack of buffer space, just return the given file buffer
 * so it will behave as usual.
 *
 * This should ONLY be called by our internal memory allocations
 * routines.
 */
const char *git_win32_leakcheck_stacktrace(int skip, const char *file);

#endif
#endif
