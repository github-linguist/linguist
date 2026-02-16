/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stdio.h>
#include <git2.h>

#include "git2_util.h"
#include "process.h"
#include "strlist.h"

#ifndef DWORD_MAX
# define DWORD_MAX INT32_MAX
#endif

#define ENV_MAX 32767

struct git_process {
	wchar_t *appname;
	wchar_t *cmdline;
	wchar_t *env;

	wchar_t *cwd;

	unsigned int capture_in  : 1,
	             capture_out : 1,
	             capture_err : 1;

	PROCESS_INFORMATION process_info;

	HANDLE child_in;
	HANDLE child_out;
	HANDLE child_err;

	git_process_result_status status;
};

/*
 * Windows processes have a single command-line that is split by the
 * invoked application into arguments (instead of an array of
 * command-line arguments).  This command-line is split by space or
 * tab delimiters, unless that whitespace is within a double quote.
 * Literal double-quotes themselves can be escaped by a backslash,
 * but only when not within double quotes.  Literal backslashes can
 * be escaped by a backslash.
 *
 * Effectively, this means that instead of thinking about quoting
 * individual strings, think about double quotes as an escaping
 * mechanism for whitespace.
 *
 * In other words (using ` as a string boundary):
 * [ `foo`, `bar` ] => `foo bar`
 * [ `foo bar` ] => `foo" "bar`
 * [ `foo bar`, `foo bar` ] => `foo" "bar foo" "bar`
 * [ `foo "bar" foo` ] => `foo" "\"bar\"" "foo`
 */
int git_process__cmdline(
	git_str *out,
	const char **in,
	size_t in_len)
{
	bool quoted = false;
	const char *c;
	size_t i;

	for (i = 0; i < in_len; i++) {
		/* Arguments are delimited by an unquoted space */
		if (i)
			git_str_putc(out, ' ');

		for (c = in[i]; *c; c++) {
			/* Start or stop quoting spaces within an argument */
			if ((*c == ' ' || *c == '\t') && !quoted) {
				git_str_putc(out, '"');
				quoted = true;
			} else if (*c != ' ' && *c != '\t' && quoted) {
				git_str_putc(out, '"');
				quoted = false;
			}

			/* Escape double-quotes and backslashes */
			if (*c == '"' || *c == '\\')
				git_str_putc(out, '\\');

			git_str_putc(out, *c);
		}
	}

	return git_str_oom(out) ? -1 : 0;
}

GIT_INLINE(bool) is_delete_env(const char *env)
{
	char *c = strchr(env, '=');

	if (c == NULL)
		return false;

	return *(c+1) == '\0';
}

static int merge_env(wchar_t **out, const char **in, size_t in_len, bool exclude_env)
{
	git_str merged = GIT_STR_INIT;
	wchar_t *in16 = NULL, *env = NULL, *e;
	char *e8 = NULL;
	size_t e_len;
	int ret = 0;
	size_t i;

	*out = NULL;

	in16 = git__malloc(ENV_MAX * sizeof(wchar_t));
	GIT_ERROR_CHECK_ALLOC(in16);

	e8 = git__malloc(ENV_MAX);
	GIT_ERROR_CHECK_ALLOC(e8);

	for (i = 0; in && i < in_len; i++) {
		if (is_delete_env(in[i]))
			continue;

		if ((ret = git_utf8_to_16(in16, ENV_MAX, in[i])) < 0)
			goto done;

		git_str_put(&merged, (const char *)in16, ret * 2);
		git_str_put(&merged, "\0\0", 2);
	}

	if (!exclude_env) {
		env = GetEnvironmentStringsW();

		for (e = env; *e; e += (e_len + 1)) {
			e_len = wcslen(e);

			if ((ret = git_utf8_from_16(e8, ENV_MAX, e)) < 0)
				goto done;

			if (git_strlist_contains_key(in, in_len, e8, '='))
				continue;

			git_str_put(&merged, (const char *)e, e_len * 2);
			git_str_put(&merged, "\0\0", 2);
		}
	}

	git_str_put(&merged, "\0\0", 2);

	*out = (wchar_t *)git_str_detach(&merged);

done:
	if (env)
		FreeEnvironmentStringsW(env);

	git_str_dispose(&merged);
	git__free(e8);
	git__free(in16);

	return ret < 0 ? -1 : 0;
}

static int process_new(
	git_process **out,
	const char *appname,
	const char *cmdline,
	const char **env,
	size_t env_len,
	git_process_options *opts)
{
	git_process *process;
	int error = 0;

	*out = NULL;

	process = git__calloc(1, sizeof(git_process));
	GIT_ERROR_CHECK_ALLOC(process);

	if (appname &&
	    git_utf8_to_16_alloc(&process->appname, appname) < 0) {
		error = -1;
		goto done;
	}

	if (git_utf8_to_16_alloc(&process->cmdline, cmdline) < 0) {
		error = -1;
		goto done;
	}

	if (opts && opts->cwd &&
	    git_utf8_to_16_alloc(&process->cwd, opts->cwd) < 0) {
		error = -1;
		goto done;
	}

	if (env && (error = merge_env(&process->env, env, env_len, opts && opts->exclude_env) < 0))
		goto done;

	if (opts) {
		process->capture_in = opts->capture_in;
		process->capture_out = opts->capture_out;
		process->capture_err = opts->capture_err;
	}

done:
	if (error)
		git_process_free(process);
	else
		*out = process;

	return error;
}

int git_process_new_from_cmdline(
	git_process **out,
	const char *cmdline,
	const char **env,
	size_t env_len,
	git_process_options *opts)
{
	GIT_ASSERT_ARG(out && cmdline);

	return process_new(out, NULL, cmdline, env, env_len, opts);
}

int git_process_new(
	git_process **out,
	const char **args,
	size_t args_len,
	const char **env,
	size_t env_len,
	git_process_options *opts)
{
	git_str cmdline = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(out && args && args_len > 0);

	if ((error = git_process__cmdline(&cmdline, args, args_len)) < 0)
		goto done;

	error = process_new(out, args[0], cmdline.ptr, env, env_len, opts);

done:
	git_str_dispose(&cmdline);
	return error;
}

#define CLOSE_HANDLE(h) do { if ((h) != NULL) CloseHandle(h); } while(0)

int git_process_start(git_process *process)
{
	STARTUPINFOW startup_info;
	SECURITY_ATTRIBUTES security_attrs;
	DWORD flags = CREATE_UNICODE_ENVIRONMENT;
	HANDLE in[2]  = { NULL, NULL },
	       out[2] = { NULL, NULL },
	       err[2] = { NULL, NULL };

	memset(&security_attrs, 0, sizeof(SECURITY_ATTRIBUTES));
	security_attrs.bInheritHandle = TRUE;

	memset(&startup_info, 0, sizeof(STARTUPINFOW));
	startup_info.cb = sizeof(STARTUPINFOW);
	startup_info.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	startup_info.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	startup_info.hStdError = GetStdHandle(STD_ERROR_HANDLE);

	if (process->capture_in) {
		if (!CreatePipe(&in[0], &in[1], &security_attrs, 0) ||
		    !SetHandleInformation(in[1], HANDLE_FLAG_INHERIT, 0)) {
			git_error_set(GIT_ERROR_OS, "could not create pipe");
			goto on_error;
		}

		startup_info.hStdInput = in[0];
		startup_info.dwFlags |= STARTF_USESTDHANDLES;
	}

	if (process->capture_out) {
		if (!CreatePipe(&out[0], &out[1], &security_attrs, 0) ||
		    !SetHandleInformation(out[0], HANDLE_FLAG_INHERIT, 0)) {
			git_error_set(GIT_ERROR_OS, "could not create pipe");
			goto on_error;
		}

		startup_info.hStdOutput = out[1];
		startup_info.dwFlags |= STARTF_USESTDHANDLES;
	}

	if (process->capture_err) {
		if (!CreatePipe(&err[0], &err[1], &security_attrs, 0) ||
		    !SetHandleInformation(err[0], HANDLE_FLAG_INHERIT, 0)) {
			git_error_set(GIT_ERROR_OS, "could not create pipe");
			goto on_error;
		}

		startup_info.hStdError = err[1];
		startup_info.dwFlags |= STARTF_USESTDHANDLES;
	}

	memset(&process->process_info, 0, sizeof(PROCESS_INFORMATION));

	if (!CreateProcessW(process->appname, process->cmdline,
	                    NULL, NULL, TRUE, flags, process->env,
	                    process->cwd,
	                    &startup_info,
	                    &process->process_info)) {
		git_error_set(GIT_ERROR_OS, "could not create process");
		goto on_error;
	}

	CLOSE_HANDLE(in[0]);  process->child_in  = in[1];
	CLOSE_HANDLE(out[1]); process->child_out = out[0];
	CLOSE_HANDLE(err[1]); process->child_err = err[0];

	return 0;

on_error:
	CLOSE_HANDLE(in[0]);  CLOSE_HANDLE(in[1]);
	CLOSE_HANDLE(out[0]); CLOSE_HANDLE(out[1]);
	CLOSE_HANDLE(err[0]); CLOSE_HANDLE(err[1]);
	return -1;
}

int git_process_id(p_pid_t *out, git_process *process)
{
	GIT_ASSERT(out && process);

	if (!process->process_info.dwProcessId) {
		git_error_set(GIT_ERROR_INVALID, "process not running");
		return -1;
	}

	*out = process->process_info.dwProcessId;
	return 0;
}

ssize_t git_process_read(git_process *process, void *buf, size_t count)
{
	DWORD ret;

	if (count > DWORD_MAX)
		count = DWORD_MAX;
	if (count > SSIZE_MAX)
		count = SSIZE_MAX;

	if (!ReadFile(process->child_out, buf, (DWORD)count, &ret, NULL)) {
		if (GetLastError() == ERROR_BROKEN_PIPE)
			return 0;

		git_error_set(GIT_ERROR_OS, "could not read");
		return -1;
	}

	return ret;
}

ssize_t git_process_write(git_process *process, const void *buf, size_t count)
{
	DWORD ret;

	if (count > DWORD_MAX)
		count = DWORD_MAX;
	if (count > SSIZE_MAX)
		count = SSIZE_MAX;

	if (!WriteFile(process->child_in, buf, (DWORD)count, &ret, NULL)) {
		git_error_set(GIT_ERROR_OS, "could not write");
		return -1;
	}

	return ret;
}

int git_process_close_in(git_process *process)
{
	if (!process->capture_in) {
		git_error_set(GIT_ERROR_INVALID, "input is not open");
		return -1;
	}

	if (process->child_in) {
		CloseHandle(process->child_in);
		process->child_in = NULL;
	}

	return 0;
}

int git_process_close_out(git_process *process)
{
	if (!process->capture_out) {
		git_error_set(GIT_ERROR_INVALID, "output is not open");
		return -1;
	}

	if (process->child_out) {
		CloseHandle(process->child_out);
		process->child_out = NULL;
	}

	return 0;
}

int git_process_close_err(git_process *process)
{
	if (!process->capture_err) {
		git_error_set(GIT_ERROR_INVALID, "error is not open");
		return -1;
	}

	if (process->child_err) {
		CloseHandle(process->child_err);
		process->child_err = NULL;
	}

	return 0;
}

int git_process_close(git_process *process)
{
	if (process->child_in) {
		CloseHandle(process->child_in);
		process->child_in = NULL;
	}

	if (process->child_out) {
		CloseHandle(process->child_out);
		process->child_out = NULL;
	}

	if (process->child_err) {
		CloseHandle(process->child_err);
		process->child_err = NULL;
	}

	CloseHandle(process->process_info.hProcess);
	process->process_info.hProcess = NULL;

	CloseHandle(process->process_info.hThread);
	process->process_info.hThread = NULL;

	return 0;
}

int git_process_wait(git_process_result *result, git_process *process)
{
	DWORD exitcode;

	if (result)
		memset(result, 0, sizeof(git_process_result));

	if (!process->process_info.dwProcessId) {
		git_error_set(GIT_ERROR_INVALID, "process is stopped");
		return -1;
	}

	if (WaitForSingleObject(process->process_info.hProcess, INFINITE) == WAIT_FAILED) {
		git_error_set(GIT_ERROR_OS, "could not wait for process");
		return -1;
	}

	if (!GetExitCodeProcess(process->process_info.hProcess, &exitcode)) {
		git_error_set(GIT_ERROR_OS, "could not get process exit code");
		return -1;
	}

	result->status = GIT_PROCESS_STATUS_NORMAL;
	result->exitcode = exitcode;

	memset(&process->process_info, 0, sizeof(PROCESS_INFORMATION));
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

	if (process->process_info.hProcess)
		git_process_close(process);

	git__free(process->env);
	git__free(process->cwd);
	git__free(process->cmdline);
	git__free(process->appname);
	git__free(process);
}
