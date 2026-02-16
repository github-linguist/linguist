/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_transports_httpparser_h__
#define INCLUDE_transports_httpparser_h__

#include "git2_util.h"

#if defined(GIT_HTTPPARSER_HTTPPARSER)

# include <http_parser.h>

typedef enum {
	GIT_HTTP_PARSER_OK     = HPE_OK,
	GIT_HTTP_PARSER_PAUSED = HPE_PAUSED,
} git_http_parser_error_t;

typedef enum {
	GIT_HTTP_PARSER_REQUEST  = HTTP_REQUEST,
	GIT_HTTP_PARSER_RESPONSE = HTTP_RESPONSE,
} git_http_parser_t;

typedef struct git_http_parser git_http_parser;

typedef struct {
	int (*on_message_begin)(git_http_parser *);
	int (*on_url)(git_http_parser *, const char *, size_t);
	int (*on_header_field)(git_http_parser *, const char *, size_t);
	int (*on_header_value)(git_http_parser *, const char *, size_t);
	int (*on_headers_complete)(git_http_parser *);
	int (*on_body)(git_http_parser *, const char *, size_t);
	int (*on_message_complete)(git_http_parser *);
} git_http_parser_settings;

struct git_http_parser {
	http_parser parser;
	git_http_parser_settings settings;
	void *data;
};

void git_http_parser_init(
	git_http_parser *parser,
	git_http_parser_t type,
	git_http_parser_settings *settings);

size_t git_http_parser_execute(
	git_http_parser *parser,
	const char *data,
	size_t len);

# define git_http_parser_status_code(parser) parser->parser.status_code
# define git_http_parser_keep_alive(parser) http_should_keep_alive(&parser->parser)
# define git_http_parser_pause(parser) (http_parser_pause(&parser->parser, 1), 0)
# define git_http_parser_resume(parser) http_parser_pause(&parser->parser, 0)
# define git_http_parser_remain_after_pause(parser) 1
# define git_http_parser_errno(parser) parser->parser.http_errno
# define git_http_parser_errmsg(parser, errno) http_errno_description(errno)

#elif defined(GIT_HTTPPARSER_LLHTTP) || defined(GIT_HTTPPARSER_BUILTIN)

# include <llhttp.h>

typedef enum {
	GIT_HTTP_PARSER_OK     = HPE_OK,
	GIT_HTTP_PARSER_PAUSED = HPE_PAUSED,
} git_http_parser_error_t;

typedef enum {
	GIT_HTTP_PARSER_REQUEST  = HTTP_REQUEST,
	GIT_HTTP_PARSER_RESPONSE = HTTP_RESPONSE,
} git_http_parser_t;

typedef llhttp_t git_http_parser;
typedef llhttp_settings_t git_http_parser_settings;

# define git_http_parser_init(parser, direction, settings) llhttp_init(parser, (llhttp_type_t)direction, settings)

size_t git_http_parser_execute(
	git_http_parser *parser,
	const char *data,
	size_t len);

# define git_http_parser_status_code(parser) parser->status_code
# define git_http_parser_keep_alive(parser) llhttp_should_keep_alive(parser)
# define git_http_parser_pause(parser) (llhttp_pause(parser), GIT_HTTP_PARSER_PAUSED)
# define git_http_parser_resume(parser) llhttp_resume(parser)
# define git_http_parser_remain_after_pause(parser) 0
# define git_http_parser_errno(parser) parser->error
# define git_http_parser_errmsg(parser, errno) llhttp_get_error_reason(parser)

#else
# error unknown http-parser
#endif

#endif
