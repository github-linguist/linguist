/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_config_parse_h__
#define INCLUDE_config_parse_h__

#include "common.h"

#include "array.h"
#include "futils.h"
#include "oid.h"
#include "parse.h"

extern const char *git_config_escapes;
extern const char *git_config_escaped;

typedef struct {
	const char *path;
	git_parse_ctx ctx;
} git_config_parser;

#define GIT_CONFIG_PARSER_INIT { NULL, GIT_PARSE_CTX_INIT }

typedef int (*git_config_parser_section_cb)(
	git_config_parser *parser,
	const char *current_section,
	const char *line,
	size_t line_len,
	void *payload);

typedef int (*git_config_parser_variable_cb)(
	git_config_parser *parser,
	const char *current_section,
	const char *var_name,
	const char *var_value,
	const char *line,
	size_t line_len,
	void *payload);

typedef int (*git_config_parser_comment_cb)(
	git_config_parser *parser,
	const char *line,
	size_t line_len,
	void *payload);

typedef int (*git_config_parser_eof_cb)(
	git_config_parser *parser,
	const char *current_section,
	void *payload);

int git_config_parser_init(git_config_parser *out, const char *path, const char *data, size_t datalen);
void git_config_parser_dispose(git_config_parser *parser);

int git_config_parse(
	git_config_parser *parser,
	git_config_parser_section_cb on_section,
	git_config_parser_variable_cb on_variable,
	git_config_parser_comment_cb on_comment,
	git_config_parser_eof_cb on_eof,
	void *payload);

#endif
