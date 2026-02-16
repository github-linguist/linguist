/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_net_h__
#define INCLUDE_net_h__

#include "git2_util.h"

/*
 * Hostname handling
 */

/*
 * See if a given hostname matches a certificate name pattern, according
 * to RFC2818 rules (which specifies HTTP over TLS). Mainly, an asterisk
 * matches anything, but is limited to a single url component.
 */
extern bool git_net_hostname_matches_cert(
	const char *hostname,
	const char *pattern);

/*
 * URL handling
 */

typedef struct git_net_url {
	char *scheme;
	char *host;
	char *port;
	char *path;
	char *query;
	char *fragment;
	char *username;
	char *password;

	unsigned int port_specified;
} git_net_url;

#define GIT_NET_URL_INIT { NULL }

/** Is a given string a url? */
extern bool git_net_str_is_url(const char *str);

/** Duplicate a URL */
extern int git_net_url_dup(git_net_url *out, git_net_url *in);

/** Parses a string containing a URL into a structure. */
extern int git_net_url_parse(git_net_url *url, const char *str);

/** Parses a string containing an SCP style path into a URL structure. */
extern int git_net_url_parse_scp(git_net_url *url, const char *str);

/**
 * Parses a string containing a standard URL or an SCP style path into
 * a URL structure.
 */
extern int git_net_url_parse_standard_or_scp(git_net_url *url, const char *str);

/**
 * Parses a string containing an HTTP endpoint that may not be a
 * well-formed URL. For example, "localhost" or "localhost:port".
 */
extern int git_net_url_parse_http(
	git_net_url *url,
	const char *str);

/** Appends a path and/or query string to the given URL */
extern int git_net_url_joinpath(
	git_net_url *out,
	git_net_url *in,
	const char *path);

/** Ensures that a URL is minimally valid (contains a host, port and path) */
extern bool git_net_url_valid(git_net_url *url);

/** Returns true if the URL is on the default port. */
extern bool git_net_url_is_default_port(git_net_url *url);

/** Returns true if the host portion of the URL is an ipv6 address. */
extern bool git_net_url_is_ipv6(git_net_url *url);

/* Applies a redirect to the URL with a git-aware service suffix. */
extern int git_net_url_apply_redirect(
	git_net_url *url,
	const char *redirect_location,
	bool allow_offsite,
	const char *service_suffix);

/** Swaps the contents of one URL for another.  */
extern void git_net_url_swap(git_net_url *a, git_net_url *b);

/** Places the URL into the given buffer. */
extern int git_net_url_fmt(git_str *out, git_net_url *url);

/** Place the path and query string into the given buffer. */
extern int git_net_url_fmt_path(git_str *buf, git_net_url *url);

/** Determines if the url matches given pattern or pattern list */
extern bool git_net_url_matches_pattern(
	git_net_url *url,
	const char *pattern);
extern bool git_net_url_matches_pattern_list(
	git_net_url *url,
	const char *pattern_list);

/** Disposes the contents of the structure. */
extern void git_net_url_dispose(git_net_url *url);

#endif
