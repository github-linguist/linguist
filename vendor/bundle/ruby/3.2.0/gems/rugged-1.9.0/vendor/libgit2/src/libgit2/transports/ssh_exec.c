/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "ssh_exec.h"

#ifdef GIT_SSH_EXEC

#include "common.h"

#include "config.h"
#include "net.h"
#include "path.h"
#include "futils.h"
#include "process.h"
#include "transports/smart.h"

typedef struct {
	git_smart_subtransport_stream parent;
} ssh_exec_subtransport_stream;

typedef struct {
	git_smart_subtransport parent;
	git_transport *owner;

	ssh_exec_subtransport_stream *current_stream;

	char *cmd_uploadpack;
	char *cmd_receivepack;

	git_smart_service_t action;
	git_process *process;
} ssh_exec_subtransport;

static int ssh_exec_subtransport_stream_read(
	git_smart_subtransport_stream *s,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	ssh_exec_subtransport *transport;
	ssh_exec_subtransport_stream *stream = (ssh_exec_subtransport_stream *)s;
	ssize_t ret;

	GIT_ASSERT_ARG(stream);
	GIT_ASSERT(stream->parent.subtransport);

	transport = (ssh_exec_subtransport *)stream->parent.subtransport;

	if ((ret = git_process_read(transport->process, buffer, buf_size)) < 0) {
		return (int)ret;
	}

	*bytes_read = (size_t)ret;
	return 0;
}

static int ssh_exec_subtransport_stream_write(
        git_smart_subtransport_stream *s,
        const char *buffer,
        size_t len)
{
	ssh_exec_subtransport *transport;
	ssh_exec_subtransport_stream *stream = (ssh_exec_subtransport_stream *)s;
	ssize_t ret;

	GIT_ASSERT(stream && stream->parent.subtransport);

	transport = (ssh_exec_subtransport *)stream->parent.subtransport;

	while (len > 0) {
		if ((ret = git_process_write(transport->process, buffer, len)) < 0)
			return (int)ret;

		len -= ret;
	}

	return 0;
}

static void ssh_exec_subtransport_stream_free(git_smart_subtransport_stream *s)
{
	ssh_exec_subtransport_stream *stream = (ssh_exec_subtransport_stream *)s;

	git__free(stream);
}

static int ssh_exec_subtransport_stream_init(
	ssh_exec_subtransport_stream **out,
	ssh_exec_subtransport *transport)
{
	GIT_ASSERT_ARG(out);

	*out = git__calloc(sizeof(ssh_exec_subtransport_stream), 1);
	GIT_ERROR_CHECK_ALLOC(*out);

	(*out)->parent.subtransport = &transport->parent;
	(*out)->parent.read = ssh_exec_subtransport_stream_read;
	(*out)->parent.write = ssh_exec_subtransport_stream_write;
	(*out)->parent.free = ssh_exec_subtransport_stream_free;

	return 0;
}

GIT_INLINE(int) ensure_transport_state(
	ssh_exec_subtransport *transport,
	git_smart_service_t expected,
	git_smart_service_t next)
{
	if (transport->action != expected && transport->action != next) {
		git_error_set(GIT_ERROR_NET, "invalid transport state");

		return -1;
	}

	return 0;
}

static int get_ssh_cmdline(
	git_str *out,
	ssh_exec_subtransport *transport,
	git_net_url *url,
	const char *command)
{
	git_remote *remote = ((transport_smart *)transport->owner)->owner;
	git_repository *repo = remote->repo;
	git_config *cfg;
	git_str ssh_cmd = GIT_STR_INIT;
	const char *default_ssh_cmd = "ssh";
	int error;

	/*
	 * Safety check: like git, we forbid paths that look like an
	 * option as that could lead to injection to ssh that can make
	 * us do unexpected things
	 */
	if (git_process__is_cmdline_option(url->username)) {
		git_error_set(GIT_ERROR_NET, "cannot ssh: username '%s' is ambiguous with command-line option", url->username);
		return -1;
	} else if (git_process__is_cmdline_option(url->host)) {
		git_error_set(GIT_ERROR_NET, "cannot ssh: host '%s' is ambiguous with command-line option", url->host);
		return -1;
	} else if (git_process__is_cmdline_option(url->path)) {
		git_error_set(GIT_ERROR_NET, "cannot ssh: path '%s' is ambiguous with command-line option", url->path);
		return -1;
	}

	if ((error = git_repository_config_snapshot(&cfg, repo)) < 0)
		return error;

	if ((error = git__getenv(&ssh_cmd, "GIT_SSH")) == 0)
		;
	else if (error != GIT_ENOTFOUND)
		goto done;
	else if ((error = git_config__get_string_buf(&ssh_cmd, cfg, "core.sshcommand")) < 0 && error != GIT_ENOTFOUND)
		goto done;

	error = git_str_printf(out, "%s %s %s \"%s%s%s\" \"%s\" \"%s\"",
		ssh_cmd.size > 0 ? ssh_cmd.ptr : default_ssh_cmd,
		url->port_specified ? "-p" : "",
		url->port_specified ? url->port : "",
		url->username ? url->username : "",
		url->username ? "@" : "",
		url->host,
		command,
		url->path);

done:
	git_str_dispose(&ssh_cmd);
	git_config_free(cfg);
	return error;
}

static int start_ssh(
	ssh_exec_subtransport *transport,
	git_smart_service_t action,
	const char *sshpath)
{
	const char *env[] = { "GIT_DIR=" };

	git_process_options process_opts = GIT_PROCESS_OPTIONS_INIT;
	git_net_url url = GIT_NET_URL_INIT;
	git_str ssh_cmdline = GIT_STR_INIT;
	const char *command;
	int error;

	process_opts.capture_in = 1;
	process_opts.capture_out = 1;
	process_opts.capture_err = 0;

	switch (action) {
	case GIT_SERVICE_UPLOADPACK_LS:
		command = transport->cmd_uploadpack ?
		          transport->cmd_uploadpack : "git-upload-pack";
		break;
	case GIT_SERVICE_RECEIVEPACK_LS:
		command = transport->cmd_receivepack ?
		          transport->cmd_receivepack : "git-receive-pack";
		break;
	default:
		git_error_set(GIT_ERROR_NET, "invalid action");
		error = -1;
		goto done;
	}

	if (git_net_str_is_url(sshpath))
		error = git_net_url_parse(&url, sshpath);
	else
		error = git_net_url_parse_scp(&url, sshpath);

	if (error < 0)
		goto done;

	if ((error = get_ssh_cmdline(&ssh_cmdline, transport, &url, command)) < 0)
		goto done;

	if ((error = git_process_new_from_cmdline(&transport->process,
	     ssh_cmdline.ptr, env, ARRAY_SIZE(env), &process_opts)) < 0 ||
	    (error = git_process_start(transport->process)) < 0) {
		git_process_free(transport->process);
		transport->process = NULL;
		goto done;
	}

done:
	git_str_dispose(&ssh_cmdline);
	git_net_url_dispose(&url);
	return error;
}

static int ssh_exec_subtransport_action(
	git_smart_subtransport_stream **out,
	git_smart_subtransport *t,
	const char *sshpath,
	git_smart_service_t action)
{
	ssh_exec_subtransport *transport = (ssh_exec_subtransport *)t;
	ssh_exec_subtransport_stream *stream = NULL;
	git_smart_service_t expected;
	int error;

	switch (action) {
	case GIT_SERVICE_UPLOADPACK_LS:
	case GIT_SERVICE_RECEIVEPACK_LS:
		if ((error = ensure_transport_state(transport, 0, 0)) < 0 ||
		    (error = ssh_exec_subtransport_stream_init(&stream, transport)) < 0 ||
		    (error = start_ssh(transport, action, sshpath)) < 0)
		    goto on_error;

		transport->current_stream = stream;
		break;

	case GIT_SERVICE_UPLOADPACK:
	case GIT_SERVICE_RECEIVEPACK:
		expected = (action == GIT_SERVICE_UPLOADPACK) ?
			GIT_SERVICE_UPLOADPACK_LS : GIT_SERVICE_RECEIVEPACK_LS;

		if ((error = ensure_transport_state(transport, expected, action)) < 0)
			goto on_error;

		break;

	default:
		git_error_set(GIT_ERROR_INVALID, "invalid service request");
		goto on_error;
	}

	transport->action = action;
	*out = &transport->current_stream->parent;

	return 0;

on_error:
	if (stream != NULL)
		ssh_exec_subtransport_stream_free(&stream->parent);

	return -1;
}

static int ssh_exec_subtransport_close(git_smart_subtransport *t)
{
	ssh_exec_subtransport *transport = (ssh_exec_subtransport *)t;

	if (transport->process) {
		git_process_close(transport->process);
		git_process_free(transport->process);
		transport->process = NULL;
	}

	transport->action = 0;

	return 0;
}

static void ssh_exec_subtransport_free(git_smart_subtransport *t)
{
	ssh_exec_subtransport *transport = (ssh_exec_subtransport *)t;

	git__free(transport->cmd_uploadpack);
	git__free(transport->cmd_receivepack);
	git__free(transport);
}

int git_smart_subtransport_ssh_exec(
	git_smart_subtransport **out,
	git_transport *owner,
	void *payload)
{
	ssh_exec_subtransport *transport;

	GIT_UNUSED(payload);

	transport = git__calloc(sizeof(ssh_exec_subtransport), 1);
	GIT_ERROR_CHECK_ALLOC(transport);

	transport->owner = owner;
	transport->parent.action = ssh_exec_subtransport_action;
	transport->parent.close = ssh_exec_subtransport_close;
	transport->parent.free = ssh_exec_subtransport_free;

	*out = (git_smart_subtransport *) transport;
	return 0;
}

int git_smart_subtransport_ssh_exec_set_paths(
	git_smart_subtransport *subtransport,
	const char *cmd_uploadpack,
	const char *cmd_receivepack)
{
	ssh_exec_subtransport *t = (ssh_exec_subtransport *)subtransport;

	git__free(t->cmd_uploadpack);
	git__free(t->cmd_receivepack);

	t->cmd_uploadpack = git__strdup(cmd_uploadpack);
	GIT_ERROR_CHECK_ALLOC(t->cmd_uploadpack);

	t->cmd_receivepack = git__strdup(cmd_receivepack);
	GIT_ERROR_CHECK_ALLOC(t->cmd_receivepack);

	return 0;
}

#endif
