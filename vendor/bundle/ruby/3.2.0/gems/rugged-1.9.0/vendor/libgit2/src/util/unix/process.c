/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stdio.h>
#include <sys/wait.h>
#include <signal.h>
#include <git2.h>

#include "git2_util.h"
#include "vector.h"
#include "process.h"
#include "strlist.h"

#ifdef __APPLE__
	#include <crt_externs.h>
	#define environ (*_NSGetEnviron())
#else
	extern char **environ;
#endif

struct git_process {
	char **args;
	char **env;

	char *cwd;

	unsigned int capture_in  : 1,
	             capture_out : 1,
	             capture_err : 1;

	pid_t pid;

	int child_in;
	int child_out;
	int child_err;
	git_process_result_status status;
};

GIT_INLINE(bool) is_delete_env(const char *env)
{
	char *c = strchr(env, '=');

	if (c == NULL)
		return false;

	return *(c+1) == '\0';
}

static int merge_env(
	char ***out,
	const char **env,
	size_t env_len,
	bool exclude_env)
{
	git_vector merged = GIT_VECTOR_INIT;
	char **kv, *dup;
	size_t max, cnt;
	int error = 0;

	for (max = env_len, kv = environ; !exclude_env && *kv; kv++)
		max++;

	if ((error = git_vector_init(&merged, max, NULL)) < 0)
		goto on_error;

	for (cnt = 0; env && cnt < env_len; cnt++) {
		if (is_delete_env(env[cnt]))
			continue;

		dup = git__strdup(env[cnt]);
		GIT_ERROR_CHECK_ALLOC(dup);

		if ((error = git_vector_insert(&merged, dup)) < 0)
			goto on_error;
	}

	if (!exclude_env) {
		for (kv = environ; *kv; kv++) {
			if (env && git_strlist_contains_key(env, env_len, *kv, '='))
				continue;

			dup = git__strdup(*kv);
			GIT_ERROR_CHECK_ALLOC(dup);

			if ((error = git_vector_insert(&merged, dup)) < 0)
				goto on_error;
		}
	}

	if (merged.length == 0) {
		*out = NULL;
		error = 0;
		goto on_error;
	}

	git_vector_insert(&merged, NULL);

	*out = (char **)merged.contents;

	return 0;

on_error:
	git_vector_dispose_deep(&merged);
	return error;
}

int git_process_new(
	git_process **out,
	const char **args,
	size_t args_len,
	const char **env,
	size_t env_len,
	git_process_options *opts)
{
	git_process *process;

	GIT_ASSERT_ARG(out && args && args_len > 0);

	*out = NULL;

	process = git__calloc(sizeof(git_process), 1);
	GIT_ERROR_CHECK_ALLOC(process);

	if (git_strlist_copy_with_null(&process->args, args, args_len) < 0 ||
	    merge_env(&process->env, env, env_len, opts ? opts->exclude_env : false) < 0) {
		git_process_free(process);
		return -1;
	}

	if (opts) {
		process->capture_in = opts->capture_in;
		process->capture_out = opts->capture_out;
		process->capture_err = opts->capture_err;

		if (opts->cwd) {
			process->cwd = git__strdup(opts->cwd);
			GIT_ERROR_CHECK_ALLOC(process->cwd);
		}
	}

	process->child_in  = -1;
	process->child_out = -1;
	process->child_err = -1;
	process->status    = -1;

	*out = process;
	return 0;
}

extern int git_process_new_from_cmdline(
	git_process **out,
	const char *cmdline,
	const char **env,
	size_t env_len,
	git_process_options *opts)
{
	const char *args[] = { "/bin/sh", "-c", cmdline };

	return git_process_new(out,
		args, ARRAY_SIZE(args), env, env_len, opts);
}

#define CLOSE_FD(fd) \
	if (fd >= 0) {     \
		close(fd); \
		fd = -1;   \
	}

static int try_read_status(size_t *out, int fd, void *buf, size_t len)
{
	size_t read_len = 0;
	int ret = -1;

	while (ret && read_len < len) {
		ret = read(fd, buf + read_len, len - read_len);

		if (ret < 0 && errno != EAGAIN && errno != EINTR) {
			git_error_set(GIT_ERROR_OS, "could not read child status");
			return -1;
		}

		read_len += ret;
	}

	*out = read_len;
	return 0;
}


static int read_status(int fd)
{
	size_t status_len = sizeof(int) * 3, read_len = 0;
	char buffer[status_len], fn[128];
	int error, fn_error, os_error, fn_len = 0;

	if ((error = try_read_status(&read_len, fd, buffer, status_len)) < 0)
		return error;

	/* Immediate EOF indicates the exec succeeded. */
	if (read_len == 0)
		return 0;

	if (read_len < status_len) {
		git_error_set(GIT_ERROR_INVALID, "child status truncated");
		return -1;
	}

	memcpy(&fn_error, &buffer[0], sizeof(int));
	memcpy(&os_error, &buffer[sizeof(int)], sizeof(int));
	memcpy(&fn_len, &buffer[sizeof(int) * 2], sizeof(int));

	if (fn_len > 0) {
		fn_len = min(fn_len, (int)(ARRAY_SIZE(fn) - 1));

		if ((error = try_read_status(&read_len, fd, fn, fn_len)) < 0)
			return error;

		fn[fn_len] = '\0';
	} else {
		fn[0] = '\0';
	}

	if (fn_error) {
		errno = os_error;
		git_error_set(GIT_ERROR_OS, "could not %s", fn[0] ? fn : "(unknown)");
	}

	return fn_error;
}

static bool try_write_status(int fd, const void *buf, size_t len)
{
	size_t write_len;
	int ret;

	for (write_len = 0; write_len < len; ) {
		ret = write(fd, buf + write_len, len - write_len);

		if (ret <= 0)
			break;

		write_len += ret;
	}

	return (len == write_len);
}

static void write_status(int fd, const char *fn, int error, int os_error)
{
	size_t status_len = sizeof(int) * 3, fn_len;
	char buffer[status_len];

	fn_len = strlen(fn);

	if (fn_len > INT_MAX)
		fn_len = INT_MAX;

	memcpy(&buffer[0], &error, sizeof(int));
	memcpy(&buffer[sizeof(int)], &os_error, sizeof(int));
	memcpy(&buffer[sizeof(int) * 2], &fn_len, sizeof(int));

	/* Do our best effort to write all the status. */
	if (!try_write_status(fd, buffer, status_len))
		return;

	if (fn_len)
		try_write_status(fd, fn, fn_len);
}

int git_process_start(git_process *process)
{
	int in[2] = { -1, -1 }, out[2] = { -1, -1 },
	    err[2] = { -1, -1 }, status[2] = { -1, -1 };
	int fdflags, state, error;
	pid_t pid;

	/* Set up the pipes to read from/write to the process */
	if ((process->capture_in && pipe(in) < 0) ||
	    (process->capture_out && pipe(out) < 0) ||
	    (process->capture_err && pipe(err) < 0)) {
		git_error_set(GIT_ERROR_OS, "could not create pipe");
		goto on_error;
	}

	/* Set up a self-pipe for status from the forked process. */
	if (pipe(status) < 0 ||
	    (fdflags = fcntl(status[1], F_GETFD)) < 0 ||
	    fcntl(status[1], F_SETFD, fdflags | FD_CLOEXEC) < 0) {
		git_error_set(GIT_ERROR_OS, "could not create pipe");
		goto on_error;
	}

	switch (pid = fork()) {
	case -1:
		git_error_set(GIT_ERROR_OS, "could not fork");
		goto on_error;

	/* Child: start the process. */
	case 0:
		/* Close the opposing side of the pipes */
		CLOSE_FD(status[0]);

		if (process->capture_in) {
			CLOSE_FD(in[1]);
			dup2(in[0],  STDIN_FILENO);
		}

		if (process->capture_out) {
			CLOSE_FD(out[0]);
			dup2(out[1], STDOUT_FILENO);
		}

		if (process->capture_err) {
			CLOSE_FD(err[0]);
			dup2(err[1], STDERR_FILENO);
		}

		if (process->cwd && (error = chdir(process->cwd)) < 0) {
			write_status(status[1], "chdir", error, errno);
			exit(0);
		}

		/*
		 * Exec the process and write the results back if the
		 * call fails.  If it succeeds, we'll close the status
		 * pipe (via CLOEXEC) and the parent will know.
		 */
		error = execve(process->args[0],
		               process->args,
			       process->env);

		write_status(status[1], "execve", error, errno);
		exit(0);

	/* Parent: make sure the child process exec'd correctly. */
	default:
		/* Close the opposing side of the pipes */
		CLOSE_FD(status[1]);

		if (process->capture_in) {
			CLOSE_FD(in[0]);
			process->child_in  = in[1];
		}

		if (process->capture_out) {
			CLOSE_FD(out[1]);
			process->child_out = out[0];
		}

		if (process->capture_err) {
			CLOSE_FD(err[1]);
			process->child_err = err[0];
		}

		/* Try to read the status */
		process->status = status[0];
		if ((error = read_status(status[0])) < 0) {
			waitpid(process->pid, &state, 0);
			goto on_error;
		}

		process->pid = pid;
		return 0;
	}

on_error:
	CLOSE_FD(in[0]);     CLOSE_FD(in[1]);
	CLOSE_FD(out[0]);    CLOSE_FD(out[1]);
	CLOSE_FD(err[0]);    CLOSE_FD(err[1]);
	CLOSE_FD(status[0]); CLOSE_FD(status[1]);
	return -1;
}

int git_process_id(p_pid_t *out, git_process *process)
{
	GIT_ASSERT(out && process);

	if (!process->pid) {
		git_error_set(GIT_ERROR_INVALID, "process not running");
		return -1;
	}

	*out = process->pid;
	return 0;
}

static ssize_t process_read(int fd, void *buf, size_t count)
{
	ssize_t ret;

	if (count > SSIZE_MAX)
		count = SSIZE_MAX;

	if ((ret = read(fd, buf, count)) < 0) {
		git_error_set(GIT_ERROR_OS, "could not read from child process");
		return -1;
	}

	return ret;
}

ssize_t git_process_read(git_process *process, void *buf, size_t count)
{
	GIT_ASSERT_ARG(process);
	GIT_ASSERT(process->capture_out);

	return process_read(process->child_out, buf, count);
}

ssize_t git_process_read_err(git_process *process, void *buf, size_t count)
{
	GIT_ASSERT_ARG(process);
	GIT_ASSERT(process->capture_err);

	return process_read(process->child_err, buf, count);
}

#ifdef GIT_THREADS

# define signal_state sigset_t

/*
 * Since signal-handling is process-wide, we cannot simply use
 * SIG_IGN to avoid SIGPIPE. Instead: http://www.microhowto.info:80/howto/ignore_sigpipe_without_affecting_other_threads_in_a_process.html
 */

GIT_INLINE(int) disable_signals(sigset_t *saved_mask)
{
	sigset_t sigpipe_mask;

	sigemptyset(&sigpipe_mask);
	sigaddset(&sigpipe_mask, SIGPIPE);

	if (pthread_sigmask(SIG_BLOCK, &sigpipe_mask, saved_mask) < 0) {
		git_error_set(GIT_ERROR_OS, "could not configure signal mask");
		return -1;
	}

	return 0;
}

GIT_INLINE(int) restore_signals(sigset_t *saved_mask)
{
	sigset_t sigpipe_mask, pending;
	int signal;

	sigemptyset(&sigpipe_mask);
	sigaddset(&sigpipe_mask, SIGPIPE);

	if (sigpending(&pending) < 0) {
		git_error_set(GIT_ERROR_OS, "could not examine pending signals");
		return -1;
	}

	if (sigismember(&pending, SIGPIPE) == 1 &&
	    sigwait(&sigpipe_mask, &signal) < 0) {
		git_error_set(GIT_ERROR_OS, "could not wait for (blocking) signal delivery");
		return -1;
	}

	if (pthread_sigmask(SIG_SETMASK, saved_mask, 0) < 0) {
		git_error_set(GIT_ERROR_OS, "could not configure signal mask");
		return -1;
	}

	return 0;
}

#else

# define signal_state struct sigaction

GIT_INLINE(int) disable_signals(struct sigaction *saved_handler)
{
	struct sigaction ign_handler = { 0 };

	ign_handler.sa_handler = SIG_IGN;

	if (sigaction(SIGPIPE, &ign_handler, saved_handler) < 0) {
		git_error_set(GIT_ERROR_OS, "could not configure signal handler");
		return -1;
	}

	return 0;
}

GIT_INLINE(int) restore_signals(struct sigaction *saved_handler)
{
	if (sigaction(SIGPIPE, saved_handler, NULL) < 0) {
		git_error_set(GIT_ERROR_OS, "could not configure signal handler");
		return -1;
	}

	return 0;
}

#endif

ssize_t git_process_write(git_process *process, const void *buf, size_t count)
{
	signal_state saved_signal;
	ssize_t ret;

	GIT_ASSERT_ARG(process);
	GIT_ASSERT(process->capture_in);

	if (count > SSIZE_MAX)
		count = SSIZE_MAX;

	if (disable_signals(&saved_signal) < 0)
		return -1;

	if ((ret = write(process->child_in, buf, count)) < 0)
		git_error_set(GIT_ERROR_OS, "could not write to child process");

	if (restore_signals(&saved_signal) < 0)
		return -1;

	return (ret < 0) ? -1 : ret;
}

int git_process_close_in(git_process *process)
{
	if (!process->capture_in) {
		git_error_set(GIT_ERROR_INVALID, "input is not open");
		return -1;
	}

	CLOSE_FD(process->child_in);
	return 0;
}

int git_process_close_out(git_process *process)
{
	if (!process->capture_out) {
		git_error_set(GIT_ERROR_INVALID, "output is not open");
		return -1;
	}

	CLOSE_FD(process->child_out);
	return 0;
}

int git_process_close_err(git_process *process)
{
	if (!process->capture_err) {
		git_error_set(GIT_ERROR_INVALID, "error is not open");
		return -1;
	}

	CLOSE_FD(process->child_err);
	return 0;
}

int git_process_close(git_process *process)
{
	CLOSE_FD(process->child_in);
	CLOSE_FD(process->child_out);
	CLOSE_FD(process->child_err);

	return 0;
}

int git_process_wait(git_process_result *result, git_process *process)
{
	int state;

	if (result)
		memset(result, 0, sizeof(git_process_result));

	if (!process->pid) {
		git_error_set(GIT_ERROR_INVALID, "process is stopped");
		return -1;
	}

	if (waitpid(process->pid, &state, 0) < 0) {
		git_error_set(GIT_ERROR_OS, "could not wait for child");
		return -1;
	}

	process->pid = 0;

	if (result) {
		if (WIFEXITED(state)) {
			result->status = GIT_PROCESS_STATUS_NORMAL;
			result->exitcode = WEXITSTATUS(state);
		} else if (WIFSIGNALED(state)) {
			result->status = GIT_PROCESS_STATUS_ERROR;
			result->signal = WTERMSIG(state);
		} else {
			result->status = GIT_PROCESS_STATUS_ERROR;
		}
	}

	return 0;
}

int git_process_result_msg(git_str *out, git_process_result *result)
{
	if (result->status == GIT_PROCESS_STATUS_NONE) {
		return git_str_puts(out, "process not started");
	} else if (result->status == GIT_PROCESS_STATUS_NORMAL) {
		return git_str_printf(out, "process exited with code %d",
		                      result->exitcode);
	} else if (result->signal) {
		return git_str_printf(out, "process exited on signal %d",
		                      result->signal);
	}

	return git_str_puts(out, "unknown error");
}

void git_process_free(git_process *process)
{
	if (!process)
		return;

	if (process->pid)
		git_process_close(process);

	git__free(process->cwd);
	git_strlist_free_with_null(process->args);
	git_strlist_free_with_null(process->env);
	git__free(process);
}
