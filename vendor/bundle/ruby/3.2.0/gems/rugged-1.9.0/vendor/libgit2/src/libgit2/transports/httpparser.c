/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "httpparser.h"

#include <string.h>

#if defined(GIT_HTTPPARSER_HTTPPARSER)

#include "http_parser.h"

static int on_message_begin(http_parser *p)
{
	git_http_parser *parser = (git_http_parser *)p;
	return parser->settings.on_message_begin(parser);
}

static int on_url(http_parser *p, const char *str, size_t len)
{
	git_http_parser *parser = (git_http_parser *)p;
	return parser->settings.on_url(parser, str, len);
}

static int on_header_field(http_parser *p, const char *str, size_t len)
{
	git_http_parser *parser = (git_http_parser *)p;
	return parser->settings.on_header_field(parser, str, len);
}

static int on_header_value(http_parser *p, const char *str, size_t len)
{
	git_http_parser *parser = (git_http_parser *)p;
	return parser->settings.on_header_value(parser, str, len);
}

static int on_headers_complete(http_parser *p)
{
	git_http_parser *parser = (git_http_parser *)p;
	return parser->settings.on_headers_complete(parser);
}

static int on_body(http_parser *p, const char *buf, size_t len)
{
	git_http_parser *parser = (git_http_parser *)p;
	return parser->settings.on_body(parser, buf, len);
}

static int on_message_complete(http_parser *p)
{
	git_http_parser *parser = (git_http_parser *)p;
	return parser->settings.on_message_complete(parser);
}

void git_http_parser_init(
	git_http_parser *parser,
	git_http_parser_t type,
	git_http_parser_settings *settings)
{
	http_parser_init(&parser->parser, (enum http_parser_type)type);
	memcpy(&parser->settings, settings, sizeof(git_http_parser_settings));
}

size_t git_http_parser_execute(
	git_http_parser *parser,
	const char *data,
	size_t len)
{
	struct http_parser_settings settings_proxy;

	memset(&settings_proxy, 0, sizeof(struct http_parser_settings));

	settings_proxy.on_message_begin = parser->settings.on_message_begin ? on_message_begin : NULL;
	settings_proxy.on_url = parser->settings.on_url ? on_url : NULL;
	settings_proxy.on_header_field = parser->settings.on_header_field ? on_header_field : NULL;
	settings_proxy.on_header_value = parser->settings.on_header_value ? on_header_value : NULL;
	settings_proxy.on_headers_complete = parser->settings.on_headers_complete ? on_headers_complete : NULL;
	settings_proxy.on_body = parser->settings.on_body ? on_body : NULL;
	settings_proxy.on_message_complete = parser->settings.on_message_complete ? on_message_complete : NULL;

	return http_parser_execute(&parser->parser, &settings_proxy, data, len);
}

#elif defined(GIT_HTTPPARSER_LLHTTP) || defined(GIT_HTTPPARSER_BUILTIN)

# include <llhttp.h>

size_t git_http_parser_execute(
	git_http_parser *parser,
	const char* data,
	size_t len)
{
	llhttp_errno_t error;
	size_t parsed_len;

	/*
	 * Unlike http_parser, which returns the number of parsed
	 * bytes in the _execute() call, llhttp returns an error
	 * code.
	 */

	if (data == NULL || len == 0)
		error = llhttp_finish(parser);
	else
		error = llhttp_execute(parser, data, len);

	parsed_len = len;

	/*
	 * Adjust number of parsed bytes in case of error.
	 */
	if (error != HPE_OK) {
		parsed_len = llhttp_get_error_pos(parser) - data;

		/* This isn't a real pause, just a way to stop parsing early. */
		if (error == HPE_PAUSED_UPGRADE)
			llhttp_resume_after_upgrade(parser);
	}

	return parsed_len;
}

#else
# error unknown http-parser
#endif
