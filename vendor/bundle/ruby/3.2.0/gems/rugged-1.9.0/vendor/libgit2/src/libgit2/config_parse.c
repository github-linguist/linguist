/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config_parse.h"

#include <ctype.h>

const char *git_config_escapes = "ntb\"\\";
const char *git_config_escaped = "\n\t\b\"\\";

static void set_parse_error(git_config_parser *reader, int col, const char *error_str)
{
	if (col)
		git_error_set(GIT_ERROR_CONFIG,
		              "failed to parse config file: %s (in %s:%"PRIuZ", column %d)",
		              error_str, reader->path, reader->ctx.line_num, col);
	else
		git_error_set(GIT_ERROR_CONFIG,
		              "failed to parse config file: %s (in %s:%"PRIuZ")",
		              error_str, reader->path, reader->ctx.line_num);
}


GIT_INLINE(int) config_keychar(char c)
{
	return git__isalnum(c) || c == '-';
}

static int strip_comments(char *line, int in_quotes)
{
	int quote_count = in_quotes, backslash_count = 0;
	char *ptr;

	for (ptr = line; *ptr; ++ptr) {
		if (ptr[0] == '"' && ((ptr > line && ptr[-1] != '\\') || ptr == line))
			quote_count++;

		if ((ptr[0] == ';' || ptr[0] == '#') &&
			(quote_count % 2) == 0 &&
			(backslash_count % 2) == 0) {
			ptr[0] = '\0';
			break;
		}

		if (ptr[0] == '\\')
			backslash_count++;
		else
			backslash_count = 0;
	}

	/* skip any space at the end */
	while (ptr > line && git__isspace(ptr[-1])) {
		ptr--;
	}
	ptr[0] = '\0';

	return quote_count;
}


static int parse_subsection_header(git_config_parser *reader, const char *line, size_t pos, const char *base_name, char **section_name)
{
	int c, rpos;
	const char *first_quote, *last_quote;
	const char *line_start = line;
	git_str buf = GIT_STR_INIT;
	size_t quoted_len, alloc_len, base_name_len = strlen(base_name);

	/* Skip any additional whitespace before our section name */
	while (git__isspace(line[pos]))
		pos++;

	/* We should be at the first quotation mark. */
	if (line[pos] != '"') {
		set_parse_error(reader, 0, "missing quotation marks in section header");
		goto end_error;
	}

	first_quote = &line[pos];
	last_quote = strrchr(line, '"');
	quoted_len = last_quote - first_quote;

	if ((last_quote - line) > INT_MAX) {
		set_parse_error(reader, 0, "invalid section header, line too long");
		goto end_error;
	}

	if (quoted_len == 0) {
		set_parse_error(reader, 0, "missing closing quotation mark in section header");
		goto end_error;
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, base_name_len, quoted_len);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 2);

	if (git_str_grow(&buf, alloc_len) < 0 ||
	    git_str_printf(&buf, "%s.", base_name) < 0)
		goto end_error;

	rpos = 0;

	line = first_quote;
	c = line[++rpos];

	/*
	 * At the end of each iteration, whatever is stored in c will be
	 * added to the string. In case of error, jump to out
	 */
	do {

		switch (c) {
		case 0:
			set_parse_error(reader, 0, "unexpected end-of-line in section header");
			goto end_error;

		case '"':
			goto end_parse;

		case '\\':
			c = line[++rpos];

			if (c == 0) {
				set_parse_error(reader, rpos, "unexpected end-of-line in section header");
				goto end_error;
			}

		default:
			break;
		}

		git_str_putc(&buf, (char)c);
		c = line[++rpos];
	} while (line + rpos < last_quote);

end_parse:
	if (git_str_oom(&buf))
		goto end_error;

	if (line[rpos] != '"' || line[rpos + 1] != ']') {
		set_parse_error(reader, rpos, "unexpected text after closing quotes");
		git_str_dispose(&buf);
		return -1;
	}

	*section_name = git_str_detach(&buf);
	return (int)(&line[rpos + 2] - line_start); /* rpos is at the closing quote */

end_error:
	git_str_dispose(&buf);

	return -1;
}

static int parse_section_header(git_config_parser *reader, char **section_out)
{
	char *name, *name_end;
	int name_length, pos;
	int result;
	char *line;
	char c;
	size_t line_len;

	git_parse_advance_ws(&reader->ctx);
	line = git__strndup(reader->ctx.line, reader->ctx.line_len);
	if (line == NULL)
		return -1;

	/* find the end of the variable's name */
	name_end = strrchr(line, ']');
	if (name_end == NULL) {
		git__free(line);
		set_parse_error(reader, 0, "missing ']' in section header");
		return -1;
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&line_len, (size_t)(name_end - line), 1);
	name = git__malloc(line_len);
	GIT_ERROR_CHECK_ALLOC(name);

	name_length = 0;
	pos = 0;

	/* Make sure we were given a section header */
	c = line[pos++];
	GIT_ASSERT(c == '[');

	c = line[pos++];

	do {
		if (git__isspace(c)){
			name[name_length] = '\0';
			result = parse_subsection_header(reader, line, pos, name, section_out);
			git__free(line);
			git__free(name);
			return result;
		}

		if (!config_keychar(c) && c != '.') {
			set_parse_error(reader, pos, "unexpected character in header");
			goto fail_parse;
		}

		name[name_length++] = (char)git__tolower(c);

	} while ((c = line[pos++]) != ']');

	if (line[pos - 1] != ']') {
		set_parse_error(reader, pos, "unexpected end of file");
		goto fail_parse;
	}

	git__free(line);

	name[name_length] = 0;
	*section_out = name;

	return pos;

fail_parse:
	git__free(line);
	git__free(name);
	return -1;
}

static int skip_bom(git_parse_ctx *parser)
{
	git_str buf = GIT_STR_INIT_CONST(parser->content, parser->content_len);
	git_str_bom_t bom;
	int bom_offset = git_str_detect_bom(&bom, &buf);

	if (bom == GIT_STR_BOM_UTF8)
		git_parse_advance_chars(parser, bom_offset);

	/* TODO: reference implementation is pretty stupid with BoM */

	return 0;
}

/*
	(* basic types *)
	digit = "0".."9"
	integer = digit { digit }
	alphabet = "a".."z" + "A" .. "Z"

	section_char = alphabet | "." | "-"
	extension_char = (* any character except newline *)
	any_char = (* any character *)
	variable_char = "alphabet" | "-"


	(* actual grammar *)
	config = { section }

	section = header { definition }

	header = "[" section [subsection | subsection_ext] "]"

	subsection = "." section
	subsection_ext = "\"" extension "\""

	section = section_char { section_char }
	extension = extension_char { extension_char }

	definition = variable_name ["=" variable_value] "\n"

	variable_name = variable_char { variable_char }
	variable_value = string | boolean | integer

	string = quoted_string | plain_string
	quoted_string = "\"" plain_string "\""
	plain_string = { any_char }

	boolean = boolean_true | boolean_false
	boolean_true = "yes" | "1" | "true" | "on"
	boolean_false = "no" | "0" | "false" | "off"
*/

/* '\"' -> '"' etc */
static int unescape_line(char **out, bool *is_multi, const char *ptr, int *quote_count)
{
	char *str, *fixed, *esc;
	size_t ptr_len = strlen(ptr), alloc_len;

	*is_multi = false;

	if (GIT_ADD_SIZET_OVERFLOW(&alloc_len, ptr_len, 1) ||
		(str = git__malloc(alloc_len)) == NULL) {
		return -1;
	}

	fixed = str;

	while (*ptr != '\0') {
		if (*ptr == '"') {
			if (quote_count)
				(*quote_count)++;
		} else if (*ptr != '\\') {
			*fixed++ = *ptr;
		} else {
			/* backslash, check the next char */
			ptr++;
			/* if we're at the end, it's a multiline, so keep the backslash */
			if (*ptr == '\0') {
				*is_multi = true;
				goto done;
			}
			if ((esc = strchr(git_config_escapes, *ptr)) != NULL) {
				*fixed++ = git_config_escaped[esc - git_config_escapes];
			} else {
				git__free(str);
				git_error_set(GIT_ERROR_CONFIG, "invalid escape at %s", ptr);
				return -1;
			}
		}
		ptr++;
	}

done:
	*fixed = '\0';
	*out = str;

	return 0;
}

static int parse_multiline_variable(git_config_parser *reader, git_str *value, int in_quotes, size_t *line_len)
{
	int quote_count;
	bool multiline = true;

	while (multiline) {
		char *line = NULL, *proc_line = NULL;
		int error;

		/* Check that the next line exists */
		git_parse_advance_line(&reader->ctx);
		line = git__strndup(reader->ctx.line, reader->ctx.line_len);
		GIT_ERROR_CHECK_ALLOC(line);
		if (GIT_ADD_SIZET_OVERFLOW(line_len, *line_len, reader->ctx.line_len)) {
			error = -1;
			goto out;
		}

		/*
		 * We've reached the end of the file, there is no continuation.
		 * (this is not an error).
		 */
		if (line[0] == '\0') {
			error = 0;
			goto out;
		}

		/* If it was just a comment, pretend it didn't exist */
		quote_count = strip_comments(line, in_quotes);
		if (line[0] == '\0')
			goto next;

		if ((error = unescape_line(&proc_line, &multiline,
					   line, &in_quotes)) < 0)
			goto out;

		/* Add this line to the multiline var */
		if ((error = git_str_puts(value, proc_line)) < 0)
			goto out;

next:
		git__free(line);
		git__free(proc_line);
		in_quotes = quote_count;
		continue;

out:
		git__free(line);
		git__free(proc_line);
		return error;
	}

	return 0;
}

GIT_INLINE(bool) is_namechar(char c)
{
	return git__isalnum(c) || c == '-';
}

static int parse_name(
	char **name, const char **value, git_config_parser *reader, const char *line)
{
	const char *name_end = line, *value_start;

	*name = NULL;
	*value = NULL;

	while (*name_end && is_namechar(*name_end))
		name_end++;

	if (line == name_end) {
		set_parse_error(reader, 0, "invalid configuration key");
		return -1;
	}

	value_start = name_end;

	while (*value_start && git__isspace(*value_start))
		value_start++;

	if (*value_start == '=') {
		*value = value_start + 1;
	} else if (*value_start) {
		set_parse_error(reader, 0, "invalid configuration key");
		return -1;
	}

	if ((*name = git__strndup(line, name_end - line)) == NULL)
		return -1;

	return 0;
}

static int parse_variable(git_config_parser *reader, char **var_name, char **var_value, size_t *line_len)
{
	const char *value_start = NULL;
	char *line = NULL, *name = NULL, *value = NULL;
	int quote_count, error;
	bool multiline;

	*var_name = NULL;
	*var_value = NULL;

	git_parse_advance_ws(&reader->ctx);
	line = git__strndup(reader->ctx.line, reader->ctx.line_len);
	GIT_ERROR_CHECK_ALLOC(line);

	quote_count = strip_comments(line, 0);

	if ((error = parse_name(&name, &value_start, reader, line)) < 0)
		goto out;

	/*
	 * Now, let's try to parse the value
	 */
	if (value_start != NULL) {
		while (git__isspace(value_start[0]))
			value_start++;

		if ((error = unescape_line(&value, &multiline, value_start, NULL)) < 0)
			goto out;

		if (multiline) {
			git_str multi_value = GIT_STR_INIT;
			git_str_attach(&multi_value, value, 0);
			value = NULL;

			if (parse_multiline_variable(reader, &multi_value, quote_count % 2, line_len) < 0 ||
			    git_str_oom(&multi_value)) {
				error = -1;
				git_str_dispose(&multi_value);
				goto out;
			}

			value = git_str_detach(&multi_value);
		}
	}

	*var_name = name;
	*var_value = value;
	name = NULL;
	value = NULL;

out:
	git__free(name);
	git__free(value);
	git__free(line);
	return error;
}

int git_config_parser_init(git_config_parser *out, const char *path, const char *data, size_t datalen)
{
	out->path = path;
	return git_parse_ctx_init(&out->ctx, data, datalen);
}

void git_config_parser_dispose(git_config_parser *parser)
{
	git_parse_ctx_clear(&parser->ctx);
}

int git_config_parse(
	git_config_parser *parser,
	git_config_parser_section_cb on_section,
	git_config_parser_variable_cb on_variable,
	git_config_parser_comment_cb on_comment,
	git_config_parser_eof_cb on_eof,
	void *payload)
{
	git_parse_ctx *ctx;
	char *current_section = NULL, *var_name = NULL, *var_value = NULL;
	int result = 0;

	ctx = &parser->ctx;

	skip_bom(ctx);

	for (; ctx->remain_len > 0; git_parse_advance_line(ctx)) {
		const char *line_start;
		size_t line_len;
		char c;

	restart:
		line_start = ctx->line;
		line_len = ctx->line_len;

		/*
		 * Get either first non-whitespace character or, if that does
		 * not exist, the first whitespace character. This is required
		 * to preserve whitespaces when writing back the file.
		 */
		if (git_parse_peek(&c, ctx, GIT_PARSE_PEEK_SKIP_WHITESPACE) < 0 &&
		    git_parse_peek(&c, ctx, 0) < 0)
			continue;

		switch (c) {
		case '[': /* section header, new section begins */
			git__free(current_section);
			current_section = NULL;

			result = parse_section_header(parser, &current_section);
			if (result < 0)
				break;

			git_parse_advance_chars(ctx, result);

			if (on_section)
				result = on_section(parser, current_section, line_start, line_len, payload);
			/*
			 * After we've parsed the section header we may not be
			 * done with the line. If there's still data in there,
			 * run the next loop with the rest of the current line
			 * instead of moving forward.
			 */

			if (!git_parse_peek(&c, ctx, GIT_PARSE_PEEK_SKIP_WHITESPACE))
				goto restart;

			break;

		case '\n': /* comment or whitespace-only */
		case '\r':
		case ' ':
		case '\t':
		case ';':
		case '#':
			if (on_comment) {
				result = on_comment(parser, line_start, line_len, payload);
			}
			break;

		default: /* assume variable declaration */
			if ((result = parse_variable(parser, &var_name, &var_value, &line_len)) == 0 && on_variable) {
				result = on_variable(parser, current_section, var_name, var_value, line_start, line_len, payload);
				git__free(var_name);
				git__free(var_value);
			}

			break;
		}

		if (result < 0)
			goto out;
	}

	if (on_eof)
		result = on_eof(parser, current_section, payload);

out:
	git__free(current_section);
	return result;
}
