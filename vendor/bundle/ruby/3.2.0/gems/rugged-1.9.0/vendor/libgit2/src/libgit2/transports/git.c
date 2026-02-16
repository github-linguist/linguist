/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "net.h"
#include "stream.h"
#include "streams/socket.h"
#include "git2/sys/transport.h"

#define OWNING_SUBTRANSPORT(s) ((git_subtransport *)(s)->parent.subtransport)

static const char prefix_git[] = "git://";
static const char cmd_uploadpack[] = "git-upload-pack";
static const char cmd_receivepack[] = "git-receive-pack";

typedef struct {
	git_smart_subtransport_stream parent;
	git_stream *io;
	const char *cmd;
	char *url;
	unsigned sent_command : 1;
} git_proto_stream;

typedef struct {
	git_smart_subtransport parent;
	git_transport *owner;
	git_proto_stream *current_stream;
} git_subtransport;

/*
 * Create a git protocol request.
 *
 * For example: 0035git-upload-pack /libgit2/libgit2\0host=github.com\0
 */
static int gen_proto(git_str *request, const char *cmd, const char *url)
{
	char *delim, *repo;
	char host[] = "host=";
	size_t len;

	delim = strchr(url, '/');
	if (delim == NULL) {
		git_error_set(GIT_ERROR_NET, "malformed URL");
		return -1;
	}

	repo = delim;
	if (repo[1] == '~')
		++repo;

	delim = strchr(url, ':');
	if (delim == NULL)
		delim = strchr(url, '/');

	len = 4 + strlen(cmd) + 1 + strlen(repo) + 1 + strlen(host) + (delim - url) + 1;

	git_str_grow(request, len);
	git_str_printf(request, "%04x%s %s%c%s",
		(unsigned int)(len & 0x0FFFF), cmd, repo, 0, host);
	git_str_put(request, url, delim - url);
	git_str_putc(request, '\0');

	if (git_str_oom(request))
		return -1;

	return 0;
}

static int send_command(git_proto_stream *s)
{
	git_str request = GIT_STR_INIT;
	int error;

	if ((error = gen_proto(&request, s->cmd, s->url)) < 0)
		goto cleanup;

	if ((error = git_stream__write_full(s->io, request.ptr, request.size, 0)) < 0)
		goto cleanup;

	s->sent_command = 1;

cleanup:
	git_str_dispose(&request);
	return error;
}

static int git_proto_stream_read(
	git_smart_subtransport_stream *stream,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	git_proto_stream *s = (git_proto_stream *)stream;
	ssize_t ret;
	int error;

	*bytes_read = 0;

	if (!s->sent_command && (error = send_command(s)) < 0)
		return error;

	ret = git_stream_read(s->io, buffer, min(buf_size, INT_MAX));

	if (ret < 0)
		return -1;

	*bytes_read = (size_t)ret;
	return 0;
}

static int git_proto_stream_write(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	git_proto_stream *s = (git_proto_stream *)stream;
	int error;

	if (!s->sent_command && (error = send_command(s)) < 0)
		return error;

	return git_stream__write_full(s->io, buffer, len, 0);
}

static void git_proto_stream_free(git_smart_subtransport_stream *stream)
{
	git_proto_stream *s;
	git_subtransport *t;

	if (!stream)
		return;

	s = (git_proto_stream *)stream;
	t = OWNING_SUBTRANSPORT(s);

	t->current_stream = NULL;

	git_stream_close(s->io);
	git_stream_free(s->io);
	git__free(s->url);
	git__free(s);
}

static int git_proto_stream_alloc(
	git_subtransport *t,
	const char *url,
	const char *cmd,
	const char *host,
	const char *port,
	git_smart_subtransport_stream **stream)
{
	git_proto_stream *s;

	if (!stream)
		return -1;

	s = git__calloc(1, sizeof(git_proto_stream));
	GIT_ERROR_CHECK_ALLOC(s);

	s->parent.subtransport = &t->parent;
	s->parent.read = git_proto_stream_read;
	s->parent.write = git_proto_stream_write;
	s->parent.free = git_proto_stream_free;

	s->cmd = cmd;
	s->url = git__strdup(url);

	if (!s->url) {
		git__free(s);
		return -1;
	}

	if ((git_socket_stream_new(&s->io, host, port)) < 0)
		return -1;

	GIT_ERROR_CHECK_VERSION(s->io, GIT_STREAM_VERSION, "git_stream");

	*stream = &s->parent;
	return 0;
}

static int _git_uploadpack_ls(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	git_net_url urldata = GIT_NET_URL_INIT;
	const char *stream_url = url;
	const char *host, *port;
	git_proto_stream *s;
	int error;

	*stream = NULL;

	if (!git__prefixcmp(url, prefix_git))
		stream_url += strlen(prefix_git);

	if ((error = git_net_url_parse(&urldata, url)) < 0)
		return error;

	host = urldata.host;
	port = urldata.port ? urldata.port : GIT_DEFAULT_PORT;

	error = git_proto_stream_alloc(t, stream_url, cmd_uploadpack, host, port, stream);

	git_net_url_dispose(&urldata);

	if (error < 0) {
		git_proto_stream_free(*stream);
		return error;
	}

	s = (git_proto_stream *) *stream;
	if ((error = git_stream_connect(s->io)) < 0) {
		git_proto_stream_free(*stream);
		return error;
	}

	t->current_stream = s;

	return 0;
}

static int _git_uploadpack(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	GIT_UNUSED(url);

	if (t->current_stream) {
		*stream = &t->current_stream->parent;
		return 0;
	}

	git_error_set(GIT_ERROR_NET, "must call UPLOADPACK_LS before UPLOADPACK");
	return -1;
}

static int _git_receivepack_ls(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	git_net_url urldata = GIT_NET_URL_INIT;
	const char *stream_url = url;
	git_proto_stream *s;
	int error;

	*stream = NULL;
	if (!git__prefixcmp(url, prefix_git))
		stream_url += strlen(prefix_git);

	if ((error = git_net_url_parse(&urldata, url)) < 0)
		return error;

	error = git_proto_stream_alloc(t, stream_url, cmd_receivepack, urldata.host, urldata.port, stream);

	git_net_url_dispose(&urldata);

	if (error < 0) {
		git_proto_stream_free(*stream);
		return error;
	}

	s = (git_proto_stream *) *stream;

	if ((error = git_stream_connect(s->io)) < 0)
		return error;

	t->current_stream = s;

	return 0;
}

static int _git_receivepack(
	git_subtransport *t,
	const char *url,
	git_smart_subtransport_stream **stream)
{
	GIT_UNUSED(url);

	if (t->current_stream) {
		*stream = &t->current_stream->parent;
		return 0;
	}

	git_error_set(GIT_ERROR_NET, "must call RECEIVEPACK_LS before RECEIVEPACK");
	return -1;
}

static int _git_action(
	git_smart_subtransport_stream **stream,
	git_smart_subtransport *subtransport,
	const char *url,
	git_smart_service_t action)
{
	git_subtransport *t = (git_subtransport *) subtransport;

	switch (action) {
		case GIT_SERVICE_UPLOADPACK_LS:
			return _git_uploadpack_ls(t, url, stream);

		case GIT_SERVICE_UPLOADPACK:
			return _git_uploadpack(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK_LS:
			return _git_receivepack_ls(t, url, stream);

		case GIT_SERVICE_RECEIVEPACK:
			return _git_receivepack(t, url, stream);
	}

	*stream = NULL;
	return -1;
}

static int _git_close(git_smart_subtransport *subtransport)
{
	git_subtransport *t = (git_subtransport *) subtransport;

	GIT_ASSERT(!t->current_stream);

	GIT_UNUSED(t);

	return 0;
}

static void _git_free(git_smart_subtransport *subtransport)
{
	git_subtransport *t = (git_subtransport *) subtransport;

	git__free(t);
}

int git_smart_subtransport_git(git_smart_subtransport **out, git_transport *owner, void *param)
{
	git_subtransport *t;

	GIT_UNUSED(param);

	if (!out)
		return -1;

	t = git__calloc(1, sizeof(git_subtransport));
	GIT_ERROR_CHECK_ALLOC(t);

	t->owner = owner;
	t->parent.action = _git_action;
	t->parent.close = _git_close;
	t->parent.free = _git_free;

	*out = (git_smart_subtransport *) t;
	return 0;
}
