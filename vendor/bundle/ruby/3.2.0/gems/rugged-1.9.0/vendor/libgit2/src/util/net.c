/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "net.h"

#include <ctype.h>

#include "posix.h"
#include "str.h"
#include "runtime.h"

#define DEFAULT_PORT_HTTP  "80"
#define DEFAULT_PORT_HTTPS "443"
#define DEFAULT_PORT_GIT   "9418"
#define DEFAULT_PORT_SSH   "22"

#define GIT_NET_URL_PARSER_INIT { 0 }

typedef struct {
	unsigned int hierarchical : 1;

	const char *scheme;
	const char *user;
	const char *password;
	const char *host;
	const char *port;
	const char *path;
	const char *query;
	const char *fragment;

	size_t scheme_len;
	size_t user_len;
	size_t password_len;
	size_t host_len;
	size_t port_len;
	size_t path_len;
	size_t query_len;
	size_t fragment_len;
} git_net_url_parser;

bool git_net_hostname_matches_cert(
	const char *hostname,
	const char *pattern)
{
	for (;;) {
		char c = git__tolower(*pattern++);

		if (c == '\0')
			return *hostname ? false : true;

		if (c == '*') {
			c = *pattern;

			/* '*' at the end matches everything left */
			if (c == '\0')
				return true;

			/*
			 * We've found a pattern, so move towards the
			 * next matching char. The '.' is handled
			 * specially because wildcards aren't allowed
			 * to cross subdomains.
			 */
			while(*hostname) {
				char h = git__tolower(*hostname);

				if (h == c)
					return git_net_hostname_matches_cert(hostname++, pattern);
				else if (h == '.')
					return git_net_hostname_matches_cert(hostname, pattern);

				hostname++;
			}

			return false;
		}

		if (c != git__tolower(*hostname++))
			return false;
	}

	return false;
}

#define is_valid_scheme_char(c) \
	(((c) >= 'a' && (c) <= 'z') || \
	 ((c) >= 'A' && (c) <= 'Z') || \
	 ((c) >= '0' && (c) <= '9') ||  \
	  (c) == '+' || (c) == '-' || (c) == '.')

bool git_net_str_is_url(const char *str)
{
	const char *c;

	for (c = str; *c; c++) {
		if (*c == ':' && *(c+1) == '/' && *(c+2) == '/')
			return true;

		if (!is_valid_scheme_char(*c))
			break;
	}

	return false;
}

static const char *default_port_for_scheme(const char *scheme)
{
	if (strcmp(scheme, "http") == 0)
		return DEFAULT_PORT_HTTP;
	else if (strcmp(scheme, "https") == 0)
		return DEFAULT_PORT_HTTPS;
	else if (strcmp(scheme, "git") == 0)
		return DEFAULT_PORT_GIT;
	else if (strcmp(scheme, "ssh") == 0 ||
	         strcmp(scheme, "ssh+git") == 0 ||
		 strcmp(scheme, "git+ssh") == 0)
		return DEFAULT_PORT_SSH;

	return NULL;
}

static bool is_ssh_scheme(const char *scheme, size_t scheme_len)
{
	if (!scheme_len)
		return false;

	return strncasecmp(scheme, "ssh", scheme_len) == 0 ||
	       strncasecmp(scheme, "ssh+git", scheme_len) == 0 ||
	       strncasecmp(scheme, "git+ssh", scheme_len) == 0;
}

int git_net_url_dup(git_net_url *out, git_net_url *in)
{
	if (in->scheme) {
		out->scheme = git__strdup(in->scheme);
		GIT_ERROR_CHECK_ALLOC(out->scheme);
	}

	if (in->host) {
		out->host = git__strdup(in->host);
		GIT_ERROR_CHECK_ALLOC(out->host);
	}

	if (in->port) {
		out->port = git__strdup(in->port);
		GIT_ERROR_CHECK_ALLOC(out->port);
	}

	if (in->path) {
		out->path = git__strdup(in->path);
		GIT_ERROR_CHECK_ALLOC(out->path);
	}

	if (in->query) {
		out->query = git__strdup(in->query);
		GIT_ERROR_CHECK_ALLOC(out->query);
	}

	if (in->username) {
		out->username = git__strdup(in->username);
		GIT_ERROR_CHECK_ALLOC(out->username);
	}

	if (in->password) {
		out->password = git__strdup(in->password);
		GIT_ERROR_CHECK_ALLOC(out->password);
	}

	return 0;
}

static int url_invalid(const char *message)
{
	git_error_set(GIT_ERROR_NET, "invalid url: %s", message);
	return GIT_EINVALIDSPEC;
}

static int url_parse_authority(
	git_net_url_parser *parser,
	const char *authority,
	size_t len)
{
	const char *c, *hostport_end, *host_end = NULL,
	           *userpass_end, *user_end = NULL;

	enum {
		HOSTPORT, HOST, IPV6, HOST_END, USERPASS, USER
	} state = HOSTPORT;

	if (len == 0)
		return 0;

	/*
	 * walk the authority backwards so that we can parse google code's
	 * ssh urls that are not rfc compliant and allow @ in the username
	 */
	for (hostport_end = authority + len, c = hostport_end - 1;
	     c >= authority && !user_end;
	     c--) {
		switch (state) {
		case HOSTPORT:
			if (*c == ':') {
				parser->port = c + 1;
				parser->port_len = hostport_end - parser->port;
				host_end = c;
				state = HOST;
				break;
			}

			/*
			 * if we've only seen digits then we don't know
			 * if we're parsing just a host or a host and port.
			 * if we see a non-digit, then we're in a host,
			 * otherwise, fall through to possibly match the
			 * "@" (user/host separator).
			 */

			if (*c < '0' || *c > '9') {
				host_end = hostport_end;
				state = HOST;
			}

			/* fall through */

		case HOST:
			if (*c == ']' && host_end == c + 1) {
				host_end = c;
				state = IPV6;
			}

			else if (*c == '@') {
				parser->host = c + 1;
				parser->host_len = host_end ?
					host_end - parser->host :
					hostport_end - parser->host;
				userpass_end = c;
				state = USERPASS;
			}

			else if (*c == '[' || *c == ']' || *c == ':') {
				return url_invalid("malformed hostname");
			}

			break;

		case IPV6:
			if (*c == '[') {
				parser->host = c + 1;
				parser->host_len = host_end - parser->host;
				state = HOST_END;
			}

			else if ((*c < '0' || *c > '9') &&
			         (*c < 'a' || *c > 'f') &&
			         (*c < 'A' || *c > 'F') &&
			         (*c != ':')) {
				return url_invalid("malformed hostname");
			}

			break;

		case HOST_END:
			if (*c == '@') {
				userpass_end = c;
				state = USERPASS;
				break;
			}

			return url_invalid("malformed hostname");

		case USERPASS:
			if (*c == '@' &&
			    !is_ssh_scheme(parser->scheme, parser->scheme_len))
				return url_invalid("malformed hostname");

			if (*c == ':') {
				parser->password = c + 1;
				parser->password_len = userpass_end - parser->password;
				user_end = c;
				state = USER;
				break;
			}

			break;

		default:
			GIT_ASSERT(!"unhandled state");
		}
	}

	switch (state) {
		case HOSTPORT:
			parser->host = authority;
			parser->host_len = (hostport_end - parser->host);
			break;
		case HOST:
			parser->host = authority;
			parser->host_len = (host_end - parser->host);
			break;
		case IPV6:
			return url_invalid("malformed hostname");
		case HOST_END:
			break;
		case USERPASS:
			parser->user = authority;
			parser->user_len = (userpass_end - parser->user);
			break;
		case USER:
			parser->user = authority;
			parser->user_len = (user_end - parser->user);
			break;
		default:
			GIT_ASSERT(!"unhandled state");
	}

	return 0;
}

static int url_parse_path(
	git_net_url_parser *parser,
	const char *path,
	size_t len)
{
	const char *c, *end;

	enum { PATH, QUERY, FRAGMENT } state = PATH;

	parser->path = path;
	end = path + len;

	for (c = path; c < end; c++) {
		switch (state) {
		case PATH:
			switch (*c) {
			case '?':
				parser->path_len = (c - parser->path);
				parser->query = c + 1;
				state = QUERY;
				break;
			case '#':
				parser->path_len = (c - parser->path);
				parser->fragment = c + 1;
				state = FRAGMENT;
				break;
			}
			break;

		case QUERY:
			if (*c == '#') {
				parser->query_len = (c - parser->query);
				parser->fragment = c + 1;
				state = FRAGMENT;
			}
			break;

		case FRAGMENT:
			break;

		default:
			GIT_ASSERT(!"unhandled state");
		}
	}

	switch (state) {
	case PATH:
		parser->path_len = (c - parser->path);
		break;
	case QUERY:
		parser->query_len = (c - parser->query);
		break;
	case FRAGMENT:
		parser->fragment_len = (c - parser->fragment);
		break;
	}

	return 0;
}

static int url_parse_finalize(git_net_url *url, git_net_url_parser *parser)
{
	git_str scheme = GIT_STR_INIT, user = GIT_STR_INIT,
	        password = GIT_STR_INIT, host = GIT_STR_INIT,
	        port = GIT_STR_INIT, path = GIT_STR_INIT,
	        query = GIT_STR_INIT, fragment = GIT_STR_INIT;
	const char *default_port;
	int port_specified = 0;
	int error = 0;

	if (parser->scheme_len) {
		if ((error = git_str_put(&scheme, parser->scheme, parser->scheme_len)) < 0)
			goto done;

		git__strntolower(scheme.ptr, scheme.size);
	}

	if (parser->user_len &&
	    (error = git_str_decode_percent(&user, parser->user, parser->user_len)) < 0)
		goto done;

	if (parser->password_len &&
	    (error = git_str_decode_percent(&password, parser->password, parser->password_len)) < 0)
		goto done;

	if (parser->host_len &&
	    (error = git_str_decode_percent(&host, parser->host, parser->host_len)) < 0)
		goto done;

	if (parser->port_len) {
		port_specified = 1;
		error = git_str_put(&port, parser->port, parser->port_len);
	} else if (parser->scheme_len &&
	           (default_port = default_port_for_scheme(scheme.ptr)) != NULL) {
		error = git_str_puts(&port, default_port);
	}

	if (error < 0)
		goto done;

	if (parser->path_len)
		error = git_str_put(&path, parser->path, parser->path_len);
	else if (parser->hierarchical)
		error = git_str_puts(&path, "/");

	if (error < 0)
		goto done;

	if (parser->query_len &&
	    (error = git_str_decode_percent(&query, parser->query, parser->query_len)) < 0)
		goto done;

	if (parser->fragment_len &&
	    (error = git_str_decode_percent(&fragment, parser->fragment, parser->fragment_len)) < 0)
		goto done;

	url->scheme = git_str_detach(&scheme);
	url->host = git_str_detach(&host);
	url->port = git_str_detach(&port);
	url->path = git_str_detach(&path);
	url->query = git_str_detach(&query);
	url->fragment = git_str_detach(&fragment);
	url->username = git_str_detach(&user);
	url->password = git_str_detach(&password);
	url->port_specified = port_specified;

	error = 0;

done:
	git_str_dispose(&scheme);
	git_str_dispose(&user);
	git_str_dispose(&password);
	git_str_dispose(&host);
	git_str_dispose(&port);
	git_str_dispose(&path);
	git_str_dispose(&query);
	git_str_dispose(&fragment);

	return error;
}

int git_net_url_parse(git_net_url *url, const char *given)
{
	git_net_url_parser parser = GIT_NET_URL_PARSER_INIT;
	const char *c, *authority, *path;
	size_t authority_len = 0, path_len = 0;
	int error = 0;

	enum {
		SCHEME_START, SCHEME,
		AUTHORITY_START, AUTHORITY,
		PATH_START, PATH
	} state = SCHEME_START;

	memset(url, 0, sizeof(git_net_url));

	for (c = given; *c; c++) {
		switch (state) {
		case SCHEME_START:
			parser.scheme = c;
			state = SCHEME;

			/* fall through */

		case SCHEME:
			if (*c == ':') {
				parser.scheme_len = (c - parser.scheme);

				if (parser.scheme_len &&
				    *(c+1) == '/' && *(c+2) == '/') {
					c += 2;
					parser.hierarchical = 1;
					state = AUTHORITY_START;
				} else {
					state = PATH_START;
				}
			} else if (!is_valid_scheme_char(*c)) {
				/*
				 * an illegal scheme character means that we
				 * were just given a relative path
				 */
				path = given;
				state = PATH;
				break;
			}
			break;

		case AUTHORITY_START:
			authority = c;
			state = AUTHORITY;

			/* fall through */
		case AUTHORITY:
			if (*c != '/')
				break;

			authority_len = (c - authority);

			/* fall through */
		case PATH_START:
			path = c;
			state = PATH;
			break;

		case PATH:
			break;

		default:
			GIT_ASSERT(!"unhandled state");
		}
	}

	switch (state) {
	case SCHEME:
		/*
		 * if we never saw a ':' then we were given a relative
		 * path, not a bare scheme
		 */
		path = given;
		path_len = (c - path);
		break;
	case AUTHORITY_START:
		break;
	case AUTHORITY:
		authority_len = (c - authority);
		break;
	case PATH_START:
		break;
	case PATH:
		path_len = (c - path);
		break;
	default:
		GIT_ASSERT(!"unhandled state");
	}

	if (authority_len &&
	    (error = url_parse_authority(&parser, authority, authority_len)) < 0)
		goto done;

	if (path_len &&
	    (error = url_parse_path(&parser, path, path_len)) < 0)
		goto done;

	error = url_parse_finalize(url, &parser);

done:
	return error;
}

int git_net_url_parse_http(
	git_net_url *url,
	const char *given)
{
	git_net_url_parser parser = GIT_NET_URL_PARSER_INIT;
	const char *c, *authority, *path = NULL;
	size_t authority_len = 0, path_len = 0;
	int error;

	/* Hopefully this is a proper URL with a scheme. */
	if (git_net_str_is_url(given))
		return git_net_url_parse(url, given);

	memset(url, 0, sizeof(git_net_url));

	/* Without a scheme, we are in the host (authority) section. */
	for (c = authority = given; *c; c++) {
		if (!path && *c == '/') {
			authority_len = (c - authority);
			path = c;
		}
	}

	if (path)
		path_len = (c - path);
	else
		authority_len = (c - authority);

	parser.scheme = "http";
	parser.scheme_len = 4;
	parser.hierarchical = 1;

	if (authority_len &&
	    (error = url_parse_authority(&parser, authority, authority_len)) < 0)
		return error;

	if (path_len &&
	    (error = url_parse_path(&parser, path, path_len)) < 0)
		return error;

	return url_parse_finalize(url, &parser);
}

static int scp_invalid(const char *message)
{
	git_error_set(GIT_ERROR_NET, "invalid scp-style path: %s", message);
	return GIT_EINVALIDSPEC;
}

static bool is_ipv6(const char *str)
{
	const char *c;
	size_t colons = 0;

	if (*str++ != '[')
		return false;

	for (c = str; *c; c++) {
		if (*c  == ':')
			colons++;

		if (*c == ']')
			return (colons > 1);

		if (*c != ':' &&
		    (*c < '0' || *c > '9') &&
		    (*c < 'a' || *c > 'f') &&
		    (*c < 'A' || *c > 'F'))
			return false;
	}

	return false;
}

static bool has_at(const char *str)
{
	const char *c;

	for (c = str; *c; c++) {
		if (*c == '@')
			return true;

		if (*c == ':')
			break;
	}

	return false;
}

int git_net_url_parse_scp(git_net_url *url, const char *given)
{
	const char *default_port = default_port_for_scheme("ssh");
	const char *c, *user, *host, *port = NULL, *path = NULL;
	size_t user_len = 0, host_len = 0, port_len = 0;
	unsigned short bracket = 0;

	enum {
		NONE,
		USER,
		HOST_START, HOST, HOST_END,
		IPV6, IPV6_END,
		PORT_START, PORT, PORT_END,
		PATH_START
	} state = NONE;

	memset(url, 0, sizeof(git_net_url));

	for (c = given; *c && !path; c++) {
		switch (state) {
		case NONE:
			switch (*c) {
			case '@':
				return scp_invalid("unexpected '@'");
			case ':':
				return scp_invalid("unexpected ':'");
			case '[':
				if (is_ipv6(c)) {
					state = IPV6;
					host = c;
				} else if (bracket++ > 1) {
					return scp_invalid("unexpected '['");
				}
				break;
			default:
				if (has_at(c)) {
					state = USER;
					user = c;
				} else {
					state = HOST;
					host = c;
				}
				break;
			}
			break;

		case USER:
			if (*c == '@') {
				user_len = (c - user);
				state = HOST_START;
			}
			break;

		case HOST_START:
			state = (*c == '[') ? IPV6 : HOST;
			host = c;
			break;

		case HOST:
			if (*c == ':') {
				host_len = (c - host);
				state = bracket ? PORT_START : PATH_START;
			} else if (*c == ']') {
				if (bracket-- == 0)
					return scp_invalid("unexpected ']'");

				host_len = (c - host);
				state = HOST_END;
			}
			break;

		case HOST_END:
			if (*c != ':')
				return scp_invalid("unexpected character after hostname");
			state = PATH_START;
			break;

		case IPV6:
			if (*c == ']')
				state = IPV6_END;
			break;

		case IPV6_END:
			if (*c != ':')
				return scp_invalid("unexpected character after ipv6 address");

			host_len = (c - host);
			state = bracket ? PORT_START : PATH_START;
			break;

		case PORT_START:
			port = c;
			state = PORT;
			break;

		case PORT:
			if (*c == ']') {
				if (bracket-- == 0)
					return scp_invalid("unexpected ']'");

				port_len = c - port;
				state = PORT_END;
			}
			break;

		case PORT_END:
			if (*c != ':')
				return scp_invalid("unexpected character after ipv6 address");

			state = PATH_START;
			break;

		case PATH_START:
			path = c;
			break;

		default:
			GIT_ASSERT(!"unhandled state");
		}
	}

	if (!path)
		return scp_invalid("path is required");

	GIT_ERROR_CHECK_ALLOC(url->scheme = git__strdup("ssh"));

	if (user_len)
		GIT_ERROR_CHECK_ALLOC(url->username = git__strndup(user, user_len));

	GIT_ASSERT(host_len);
	GIT_ERROR_CHECK_ALLOC(url->host = git__strndup(host, host_len));

	if (port_len) {
		url->port_specified = 1;
		GIT_ERROR_CHECK_ALLOC(url->port = git__strndup(port, port_len));
	} else {
		GIT_ERROR_CHECK_ALLOC(url->port = git__strdup(default_port));
	}

	GIT_ASSERT(path);
	GIT_ERROR_CHECK_ALLOC(url->path = git__strdup(path));

	return 0;
}

int git_net_url_parse_standard_or_scp(git_net_url *url, const char *given)
{
	return git_net_str_is_url(given) ?
	       git_net_url_parse(url, given) :
	       git_net_url_parse_scp(url, given);
}

int git_net_url_joinpath(
	git_net_url *out,
	git_net_url *one,
	const char *two)
{
	git_str path = GIT_STR_INIT;
	const char *query;
	size_t one_len, two_len;

	git_net_url_dispose(out);

	if ((query = strchr(two, '?')) != NULL) {
		two_len = query - two;

		if (*(++query) != '\0') {
			out->query = git__strdup(query);
			GIT_ERROR_CHECK_ALLOC(out->query);
		}
	} else {
		two_len = strlen(two);
	}

	/* Strip all trailing `/`s from the first path */
	one_len = one->path ? strlen(one->path) : 0;
	while (one_len && one->path[one_len - 1] == '/')
		one_len--;

	/* Strip all leading `/`s from the second path */
	while (*two == '/') {
		two++;
		two_len--;
	}

	git_str_put(&path, one->path, one_len);
	git_str_putc(&path, '/');
	git_str_put(&path, two, two_len);

	if (git_str_oom(&path))
		return -1;

	out->path = git_str_detach(&path);

	if (one->scheme) {
		out->scheme = git__strdup(one->scheme);
		GIT_ERROR_CHECK_ALLOC(out->scheme);
	}

	if (one->host) {
		out->host = git__strdup(one->host);
		GIT_ERROR_CHECK_ALLOC(out->host);
	}

	if (one->port) {
		out->port = git__strdup(one->port);
		GIT_ERROR_CHECK_ALLOC(out->port);
	}

	if (one->username) {
		out->username = git__strdup(one->username);
		GIT_ERROR_CHECK_ALLOC(out->username);
	}

	if (one->password) {
		out->password = git__strdup(one->password);
		GIT_ERROR_CHECK_ALLOC(out->password);
	}

	return 0;
}

/*
 * Some servers strip the query parameters from the Location header
 * when sending a redirect. Others leave it in place.
 * Check for both, starting with the stripped case first,
 * since it appears to be more common.
 */
static void remove_service_suffix(
	git_net_url *url,
	const char *service_suffix)
{
	const char *service_query = strchr(service_suffix, '?');
	size_t full_suffix_len = strlen(service_suffix);
	size_t suffix_len = service_query ?
		(size_t)(service_query - service_suffix) : full_suffix_len;
	size_t path_len = strlen(url->path);
	ssize_t truncate = -1;

	/*
	 * Check for a redirect without query parameters,
	 * like "/newloc/info/refs"'
	 */
	if (suffix_len && path_len >= suffix_len) {
		size_t suffix_offset = path_len - suffix_len;

		if (git__strncmp(url->path + suffix_offset, service_suffix, suffix_len) == 0 &&
		    (!service_query || git__strcmp(url->query, service_query + 1) == 0)) {
			truncate = suffix_offset;
		}
	}

	/*
	 * If we haven't already found where to truncate to remove the
	 * suffix, check for a redirect with query parameters, like
	 * "/newloc/info/refs?service=git-upload-pack"
	 */
	if (truncate < 0 && git__suffixcmp(url->path, service_suffix) == 0)
		truncate = path_len - full_suffix_len;

	/* Ensure we leave a minimum of '/' as the path */
	if (truncate == 0)
		truncate++;

	if (truncate > 0) {
		url->path[truncate] = '\0';

		git__free(url->query);
		url->query = NULL;
	}
}

int git_net_url_apply_redirect(
	git_net_url *url,
	const char *redirect_location,
	bool allow_offsite,
	const char *service_suffix)
{
	git_net_url tmp = GIT_NET_URL_INIT;
	int error = 0;

	GIT_ASSERT(url);
	GIT_ASSERT(redirect_location);

	if (redirect_location[0] == '/') {
		git__free(url->path);

		if ((url->path = git__strdup(redirect_location)) == NULL) {
			error = -1;
			goto done;
		}
	} else {
		git_net_url *original = url;

		if ((error = git_net_url_parse(&tmp, redirect_location)) < 0)
			goto done;

		/* Validate that this is a legal redirection */

		if (original->scheme &&
		    strcmp(original->scheme, tmp.scheme) != 0 &&
		    strcmp(tmp.scheme, "https") != 0) {
			git_error_set(GIT_ERROR_NET, "cannot redirect from '%s' to '%s'",
				original->scheme, tmp.scheme);

			error = -1;
			goto done;
		}

		if (original->host &&
		    !allow_offsite &&
		    git__strcasecmp(original->host, tmp.host) != 0) {
			git_error_set(GIT_ERROR_NET, "cannot redirect from '%s' to '%s'",
				original->host, tmp.host);

			error = -1;
			goto done;
		}

		git_net_url_swap(url, &tmp);
	}

	/* Remove the service suffix if it was given to us */
	if (service_suffix)
		remove_service_suffix(url, service_suffix);

done:
	git_net_url_dispose(&tmp);
	return error;
}

bool git_net_url_valid(git_net_url *url)
{
	return (url->host && url->port && url->path);
}

bool git_net_url_is_default_port(git_net_url *url)
{
	const char *default_port;

	if (url->scheme && (default_port = default_port_for_scheme(url->scheme)) != NULL)
		return (strcmp(url->port, default_port) == 0);
	else
		return false;
}

bool git_net_url_is_ipv6(git_net_url *url)
{
	return (strchr(url->host, ':') != NULL);
}

void git_net_url_swap(git_net_url *a, git_net_url *b)
{
	git_net_url tmp = GIT_NET_URL_INIT;

	memcpy(&tmp, a, sizeof(git_net_url));
	memcpy(a, b, sizeof(git_net_url));
	memcpy(b, &tmp, sizeof(git_net_url));
}

int git_net_url_fmt(git_str *buf, git_net_url *url)
{
	GIT_ASSERT_ARG(url);
	GIT_ASSERT_ARG(url->scheme);
	GIT_ASSERT_ARG(url->host);

	git_str_puts(buf, url->scheme);
	git_str_puts(buf, "://");

	if (url->username) {
		git_str_puts(buf, url->username);

		if (url->password) {
			git_str_puts(buf, ":");
			git_str_puts(buf, url->password);
		}

		git_str_putc(buf, '@');
	}

	git_str_puts(buf, url->host);

	if (url->port && !git_net_url_is_default_port(url)) {
		git_str_putc(buf, ':');
		git_str_puts(buf, url->port);
	}

	git_str_puts(buf, url->path ? url->path : "/");

	if (url->query) {
		git_str_putc(buf, '?');
		git_str_puts(buf, url->query);
	}

	return git_str_oom(buf) ? -1 : 0;
}

int git_net_url_fmt_path(git_str *buf, git_net_url *url)
{
	git_str_puts(buf, url->path ? url->path : "/");

	if (url->query) {
		git_str_putc(buf, '?');
		git_str_puts(buf, url->query);
	}

	return git_str_oom(buf) ? -1 : 0;
}

static bool matches_pattern(
	git_net_url *url,
	const char *pattern,
	size_t pattern_len)
{
	const char *domain, *port = NULL, *colon;
	size_t host_len, domain_len, port_len = 0, wildcard = 0;

	GIT_UNUSED(url);
	GIT_UNUSED(pattern);

	if (!pattern_len)
		return false;
	else if (pattern_len == 1 && pattern[0] == '*')
		return true;
	else if (pattern_len > 1 && pattern[0] == '*' && pattern[1] == '.')
		wildcard = 2;
	else if (pattern[0] == '.')
		wildcard = 1;

	domain = pattern + wildcard;
	domain_len = pattern_len - wildcard;

	if ((colon = memchr(domain, ':', domain_len)) != NULL) {
		domain_len = colon - domain;
		port = colon + 1;
		port_len = pattern_len - wildcard - domain_len - 1;
	}

	/* A pattern's port *must* match if it's specified */
	if (port_len && git__strlcmp(url->port, port, port_len) != 0)
		return false;

	/* No wildcard?  Host must match exactly. */
	if (!wildcard)
		return !git__strlcmp(url->host, domain, domain_len);

	/* Wildcard: ensure there's (at least) a suffix match */
	if ((host_len = strlen(url->host)) < domain_len ||
	    memcmp(url->host + (host_len - domain_len), domain, domain_len))
		return false;

	/* The pattern is *.domain and the host is simply domain */
	if (host_len == domain_len)
		return true;

	/* The pattern is *.domain and the host is foo.domain */
	return (url->host[host_len - domain_len - 1] == '.');
}

bool git_net_url_matches_pattern(git_net_url *url, const char *pattern)
{
	return matches_pattern(url, pattern, strlen(pattern));
}

bool git_net_url_matches_pattern_list(
	git_net_url *url,
	const char *pattern_list)
{
	const char *pattern, *pattern_end, *sep;

	for (pattern = pattern_list;
	     pattern && *pattern;
	     pattern = sep ? sep + 1 : NULL) {
		sep = strchr(pattern, ',');
		pattern_end = sep ? sep : strchr(pattern, '\0');

		if (matches_pattern(url, pattern, (pattern_end - pattern)))
			return true;
	}

	return false;
}

void git_net_url_dispose(git_net_url *url)
{
	if (url->username)
		git__memzero(url->username, strlen(url->username));

	if (url->password)
		git__memzero(url->password, strlen(url->password));

	git__free(url->scheme); url->scheme = NULL;
	git__free(url->host); url->host = NULL;
	git__free(url->port); url->port = NULL;
	git__free(url->path); url->path = NULL;
	git__free(url->query); url->query = NULL;
	git__free(url->fragment); url->fragment = NULL;
	git__free(url->username); url->username = NULL;
	git__free(url->password); url->password = NULL;
}
