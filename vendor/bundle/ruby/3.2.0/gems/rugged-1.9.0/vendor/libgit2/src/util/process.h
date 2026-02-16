/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_process_h__
#define INCLUDE_process_h__

typedef struct git_process git_process;

typedef struct {
	unsigned int capture_in  : 1,
	             capture_out : 1,
	             capture_err : 1,
	             exclude_env : 1;

	char *cwd;
} git_process_options;

typedef enum {
	GIT_PROCESS_STATUS_NONE,
	GIT_PROCESS_STATUS_NORMAL,
	GIT_PROCESS_STATUS_ERROR
} git_process_result_status;

#define GIT_PROCESS_RESULT_INIT { GIT_PROCESS_STATUS_NONE }

typedef struct {
	git_process_result_status status;
	int exitcode;
	int signal;
} git_process_result;

#define GIT_PROCESS_OPTIONS_INIT { 0 }

#ifdef GIT_WIN32
# define p_pid_t DWORD
#else
# define p_pid_t pid_t
#endif

/**
 * Create a new process.  The command to run should be specified as the
 * element of the `arg` array, execv-style. This should be the full path
 * to the command to run, the PATH is not obeyed.
 *
 * This function will add the given environment variables (in `env`)
 * to the current environment.  Operations on environment variables
 * are not thread safe, so you may not modify the environment during
 * this call.  You can avoid this by setting `exclude_env` in the
 * options and providing the entire environment yourself.
 *
 * @param out location to store the process
 * @param args the command (with arguments) to run
 * @param args_len the length of the args array
 * @param env environment variables to add (or NULL)
 * @param env_len the length of the env len
 * @param opts the options for creating the process
 * @return 0 or an error code
 */
extern int git_process_new(
	git_process **out,
	const char **args,
	size_t args_len,
	const char **env,
	size_t env_len,
	git_process_options *opts);

/**
 * Create a new process. The command to run should be specified as the
 * `cmdline` option - which is the full text of the command line as it
 * would be specified or run by a user. The command to run will be
 * looked up in the PATH.
 *
 * On Unix, this will be executed by the system's shell (`/bin/sh`)
 * and may contain _Bourne-style_ shell quoting rules. On Windows,
 * this will be passed to `CreateProcess`, and similarly, may
 * contain _Windows-style_ shell quoting rules.
 *
 * This function will add the given environment variables (in `env`)
 * to the current environment.  Operations on environment variables
 * are not thread safe, so you may not modify the environment during
 * this call.  You can avoid this by setting `exclude_env` in the
 * options and providing the entire environment yourself.
 */
extern int git_process_new_from_cmdline(
	git_process **out,
	const char *cmdline,
	const char **env,
	size_t env_len,
	git_process_options *opts);

#ifdef GIT_WIN32

extern int git_process__appname(
	git_str *out,
	const char *cmdline);

/* Windows path parsing is tricky; this helper function is for testing. */
extern int git_process__cmdline(
	git_str *out,
	const char **in,
	size_t in_len);

#endif

/*
 * Whether the given string looks like a command line option (starts
 * with a dash). This is useful for examining strings that will become
 * cmdline arguments to ensure that they are not erroneously treated
 * as an option. For example, arguments to `ssh`.
 */
GIT_INLINE(bool) git_process__is_cmdline_option(const char *str)
{
	return (str && str[0] == '-');
}

/**
 * Start the process.
 *
 * @param process the process to start
 * @return 0 or an error code
 */
extern int git_process_start(git_process *process);

/**
 * Returns the process id of the process.
 *
 * @param out pointer to a pid_t to store the process id
 * @param process the process to query
 * @return 0 or an error code
 */
extern int git_process_id(p_pid_t *out, git_process *process);

/**
 * Read from the process's stdout.  The process must have been created with
 * `capture_out` set to true.
 *
 * @param process the process to read from
 * @param buf the buf to read into
 * @param count maximum number of bytes to read
 * @return number of bytes read or an error code
 */
extern ssize_t git_process_read(git_process *process, void *buf, size_t count);

/**
 * Read from the process's stderr.  The process must have been created with
 * `capture_err` set to true.
 *
 * @param process the process to read from
 * @param buf the buf to read into
 * @param count maximum number of bytes to read
 * @return number of bytes read or an error code
 */
extern ssize_t git_process_read_err(git_process *process, void *buf, size_t count);

/**
 * Write to the process's stdin.  The process must have been created with
 * `capture_in` set to true.
 *
 * @param process the process to write to
 * @param buf the buf to write
 * @param count maximum number of bytes to write
 * @return number of bytes written or an error code
 */
extern ssize_t git_process_write(git_process *process, const void *buf, size_t count);

/**
 * Wait for the process to finish.
 *
 * @param result the result of the process or NULL
 * @param process the process to wait on
 */
extern int git_process_wait(git_process_result *result, git_process *process);

/**
 * Close the input pipe from the child.
 *
 * @param process the process to close the pipe on
 */
extern int git_process_close_in(git_process *process);

/**
 * Close the output pipe from the child.
 *
 * @param process the process to close the pipe on
 */
extern int git_process_close_out(git_process *process);

/**
 * Close the error pipe from the child.
 *
 * @param process the process to close the pipe on
 */
extern int git_process_close_err(git_process *process);

/**
 * Close all resources that are used by the process.  This does not
 * wait for the process to complete.
 *
 * @parma process the process to close
 */
extern int git_process_close(git_process *process);

/**
 * Place a human-readable error message in the given git buffer.
 *
 * @param msg the buffer to store the message
 * @param result the process result that produced an error
 */
extern int git_process_result_msg(git_str *msg, git_process_result *result);

/**
 * Free a process structure
 *
 * @param process the process to free
 */
extern void git_process_free(git_process *process);

#endif
