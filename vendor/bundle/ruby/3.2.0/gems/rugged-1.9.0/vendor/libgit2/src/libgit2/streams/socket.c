/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "streams/socket.h"

#include "posix.h"
#include "registry.h"
#include "runtime.h"
#include "stream.h"

#ifndef _WIN32
# include <sys/types.h>
# include <sys/socket.h>
# include <sys/select.h>
# include <sys/time.h>
# include <netdb.h>
# include <netinet/in.h>
# include <arpa/inet.h>
#else
# include <winsock2.h>
# include <ws2tcpip.h>
# ifdef _MSC_VER
#  pragma comment(lib, "ws2_32")
# endif
#endif

int git_socket_stream__connect_timeout = 0;
int git_socket_stream__timeout = 0;

#ifdef GIT_WIN32
static void net_set_error(const char *str)
{
	int error = WSAGetLastError();
	char * win32_error = git_win32_get_error_message(error);

	if (win32_error) {
		git_error_set(GIT_ERROR_NET, "%s: %s", str, win32_error);
		git__free(win32_error);
	} else {
		git_error_set(GIT_ERROR_NET, "%s", str);
	}
}
#else
static void net_set_error(const char *str)
{
	git_error_set(GIT_ERROR_NET, "%s: %s", str, strerror(errno));
}
#endif

static int close_socket(GIT_SOCKET s)
{
	if (s == INVALID_SOCKET)
		return 0;

#ifdef GIT_WIN32
	if (closesocket(s) != 0) {
		net_set_error("could not close socket");
		return -1;
	}

	return 0;
#else
	return close(s);
#endif

}

static int set_nonblocking(GIT_SOCKET s)
{
#ifdef GIT_WIN32
	unsigned long nonblocking = 1;

	if (ioctlsocket(s, FIONBIO, &nonblocking) != 0) {
		net_set_error("could not set socket non-blocking");
		return -1;
	}
#else
	int flags;

	if ((flags = fcntl(s, F_GETFL, 0)) == -1) {
		net_set_error("could not query socket flags");
		return -1;
	}

	flags |= O_NONBLOCK;

	if (fcntl(s, F_SETFL, flags) != 0) {
		net_set_error("could not set socket non-blocking");
		return -1;
	}
#endif

	return 0;
}

/* Promote a sockerr to an errno for our error handling routines */
static int handle_sockerr(GIT_SOCKET socket)
{
	int sockerr;
	socklen_t errlen = sizeof(sockerr);

	if (getsockopt(socket, SOL_SOCKET, SO_ERROR,
			(void *)&sockerr, &errlen) < 0)
		return -1;

	if (sockerr == ETIMEDOUT)
		return GIT_TIMEOUT;

	errno = sockerr;
	return -1;
}

GIT_INLINE(bool) connect_would_block(int error)
{
#ifdef GIT_WIN32
	if (error == SOCKET_ERROR && WSAGetLastError() == WSAEWOULDBLOCK)
		return true;
#endif

	if (error == -1 && errno == EINPROGRESS)
		return true;

	return false;
}

static int connect_with_timeout(
	GIT_SOCKET socket,
	const struct sockaddr *address,
	socklen_t address_len,
	int timeout)
{
	struct pollfd fd;
	int error;

	if (timeout && (error = set_nonblocking(socket)) < 0)
		return error;

	error = connect(socket, address, address_len);

	if (error == 0 || !connect_would_block(error))
		return error;

	fd.fd = socket;
	fd.events = POLLOUT;
	fd.revents = 0;

	error = p_poll(&fd, 1, timeout);

	if (error == 0) {
		return GIT_TIMEOUT;
	} else if (error != 1) {
		return -1;
	} else if ((fd.revents & (POLLPRI | POLLHUP | POLLERR))) {
		return handle_sockerr(socket);
	} else if ((fd.revents & POLLOUT) != POLLOUT) {
		git_error_set(GIT_ERROR_NET,
			"unknown error while polling for connect: %d",
			fd.revents);
		return -1;
	}

	return 0;
}

static int socket_connect(git_stream *stream)
{
	git_socket_stream *st = (git_socket_stream *) stream;
	GIT_SOCKET s = INVALID_SOCKET;
	struct addrinfo *info = NULL, *p;
	struct addrinfo hints;
	int error;

	memset(&hints, 0x0, sizeof(struct addrinfo));
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_family = AF_UNSPEC;

	if ((error = p_getaddrinfo(st->host, st->port, &hints, &info)) != 0) {
		git_error_set(GIT_ERROR_NET,
			   "failed to resolve address for %s: %s",
			   st->host, p_gai_strerror(error));
		return -1;
	}

	for (p = info; p != NULL; p = p->ai_next) {
		s = socket(p->ai_family, p->ai_socktype | SOCK_CLOEXEC, p->ai_protocol);

		if (s == INVALID_SOCKET)
			continue;

		error = connect_with_timeout(s, p->ai_addr,
				(socklen_t)p->ai_addrlen,
				st->parent.connect_timeout);

		if (error == 0)
			break;

		/* If we can't connect, try the next one */
		close_socket(s);
		s = INVALID_SOCKET;

		if (error == GIT_TIMEOUT)
			break;
	}

	/* Oops, we couldn't connect to any address */
	if (s == INVALID_SOCKET) {
		if (error == GIT_TIMEOUT)
			git_error_set(GIT_ERROR_NET, "failed to connect to %s: Operation timed out", st->host);
		else
			git_error_set(GIT_ERROR_OS, "failed to connect to %s", st->host);
		error = -1;
		goto done;
	}

	if (st->parent.timeout && !st->parent.connect_timeout &&
	    (error = set_nonblocking(s)) < 0)
		return error;

	st->s = s;
	error = 0;

done:
	p_freeaddrinfo(info);
	return error;
}

static ssize_t socket_write(
	git_stream *stream,
	const char *data,
	size_t len,
	int flags)
{
	git_socket_stream *st = (git_socket_stream *) stream;
	struct pollfd fd;
	ssize_t ret;

	GIT_ASSERT(flags == 0);
	GIT_UNUSED(flags);

	ret = p_send(st->s, data, len, 0);

	if (st->parent.timeout && ret < 0 &&
	    (errno == EAGAIN || errno != EWOULDBLOCK)) {
		fd.fd = st->s;
		fd.events = POLLOUT;
		fd.revents = 0;

		ret = p_poll(&fd, 1, st->parent.timeout);

		if (ret == 1) {
			ret = p_send(st->s, data, len, 0);
		} else if (ret == 0) {
			git_error_set(GIT_ERROR_NET,
				"could not write to socket: timed out");
			return GIT_TIMEOUT;
		}
	}

	if (ret < 0) {
		net_set_error("error receiving data from socket");
		return -1;
	}

	return ret;
}

static ssize_t socket_read(
	git_stream *stream,
	void *data,
	size_t len)
{
	git_socket_stream *st = (git_socket_stream *) stream;
	struct pollfd fd;
	ssize_t ret;

	ret = p_recv(st->s, data, len, 0);

	if (st->parent.timeout && ret < 0 &&
	    (errno == EAGAIN || errno != EWOULDBLOCK)) {
		fd.fd = st->s;
		fd.events = POLLIN;
		fd.revents = 0;

		ret = p_poll(&fd, 1, st->parent.timeout);

		if (ret == 1) {
			ret = p_recv(st->s, data, len, 0);
		} else if (ret == 0) {
			git_error_set(GIT_ERROR_NET,
				"could not read from socket: timed out");
			return GIT_TIMEOUT;
		}
	}

	if (ret < 0) {
		net_set_error("error receiving data from socket");
		return -1;
	}

	return ret;
}

static int socket_close(git_stream *stream)
{
	git_socket_stream *st = (git_socket_stream *) stream;
	int error;

	error = close_socket(st->s);
	st->s = INVALID_SOCKET;

	return error;
}

static void socket_free(git_stream *stream)
{
	git_socket_stream *st = (git_socket_stream *) stream;

	git__free(st->host);
	git__free(st->port);
	git__free(st);
}

static int default_socket_stream_new(
	git_stream **out,
	const char *host,
	const char *port)
{
	git_socket_stream *st;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(host);
	GIT_ASSERT_ARG(port);

	st = git__calloc(1, sizeof(git_socket_stream));
	GIT_ERROR_CHECK_ALLOC(st);

	st->host = git__strdup(host);
	GIT_ERROR_CHECK_ALLOC(st->host);

	if (port) {
		st->port = git__strdup(port);
		GIT_ERROR_CHECK_ALLOC(st->port);
	}

	st->parent.version = GIT_STREAM_VERSION;
	st->parent.timeout = git_socket_stream__timeout;
	st->parent.connect_timeout = git_socket_stream__connect_timeout;
	st->parent.connect = socket_connect;
	st->parent.write = socket_write;
	st->parent.read = socket_read;
	st->parent.close = socket_close;
	st->parent.free = socket_free;
	st->s = INVALID_SOCKET;

	*out = (git_stream *) st;
	return 0;
}

int git_socket_stream_new(
	git_stream **out,
	const char *host,
	const char *port)
{
	int (*init)(git_stream **, const char *, const char *) = NULL;
	git_stream_registration custom = {0};
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(host);
	GIT_ASSERT_ARG(port);

	if ((error = git_stream_registry_lookup(&custom, GIT_STREAM_STANDARD)) == 0)
		init = custom.init;
	else if (error == GIT_ENOTFOUND)
		init = default_socket_stream_new;
	else
		return error;

	if (!init) {
		git_error_set(GIT_ERROR_NET, "there is no socket stream available");
		return -1;
	}

	return init(out, host, port);
}

#ifdef GIT_WIN32

static void socket_stream_global_shutdown(void)
{
	WSACleanup();
}

int git_socket_stream_global_init(void)
{
	WORD winsock_version;
	WSADATA wsa_data;

	winsock_version = MAKEWORD(2, 2);

	if (WSAStartup(winsock_version, &wsa_data) != 0) {
		git_error_set(GIT_ERROR_OS, "could not initialize Windows Socket Library");
		return -1;
	}

	if (LOBYTE(wsa_data.wVersion) != 2 ||
	    HIBYTE(wsa_data.wVersion) != 2) {
		git_error_set(GIT_ERROR_SSL, "Windows Socket Library does not support Winsock 2.2");
		return -1;
	}

	return git_runtime_shutdown_register(socket_stream_global_shutdown);
}

#else

#include "stream.h"

int git_socket_stream_global_init(void)
{
	return 0;
}

 #endif
