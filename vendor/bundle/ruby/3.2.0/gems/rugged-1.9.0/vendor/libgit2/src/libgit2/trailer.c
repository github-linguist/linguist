/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "array.h"
#include "common.h"
#include "git2/message.h"

#include <stddef.h>
#include <string.h>
#include <ctype.h>

#define COMMENT_LINE_CHAR '#'
#define TRAILER_SEPARATORS ":"

static const char *const git_generated_prefixes[] = {
	"Signed-off-by: ",
	"(cherry picked from commit ",
	NULL
};

static int is_blank_line(const char *str)
{
	const char *s = str;
	while (*s && *s != '\n' && git__isspace(*s))
		s++;
	return !*s || *s == '\n';
}

static const char *next_line(const char *str)
{
	const char *nl = strchr(str, '\n');

	if (nl) {
		return nl + 1;
	} else {
		/* return pointer to the NUL terminator: */
		return str + strlen(str);
	}
}

/*
 * Return the position of the start of the last line. If len is 0, return 0.
 */
static bool last_line(size_t *out, const char *buf, size_t len)
{
	size_t i;

	*out = 0;

	if (len == 0)
		return false;
	if (len == 1)
		return true;

	/*
	 * Skip the last character (in addition to the null terminator),
	 * because if the last character is a newline, it is considered as part
	 * of the last line anyway.
	 */
	i = len - 2;

	for (; i > 0; i--) {
		if (buf[i] == '\n') {
			*out = i + 1;
			return true;
		}
	}
	return true;
}

/*
 * If the given line is of the form
 * "<token><optional whitespace><separator>..." or "<separator>...", sets out
 * to the location of the separator and returns true.  Otherwise, returns
 * false.  The optional whitespace is allowed there primarily to allow things
 * like "Bug #43" where <token> is "Bug" and <separator> is "#".
 *
 * The separator-starts-line case (in which this function returns true and
 * sets out to 0) is distinguished from the non-well-formed-line case (in
 * which this function returns false) because some callers of this function
 * need such a distinction.
 */
static bool find_separator(size_t *out, const char *line, const char *separators)
{
	int whitespace_found = 0;
	const char *c;
	for (c = line; *c; c++) {
		if (strchr(separators, *c)) {
			*out = c - line;
			return true;
		}

		if (!whitespace_found && (git__isalnum(*c) || *c == '-'))
			continue;
		if (c != line && (*c == ' ' || *c == '\t')) {
			whitespace_found = 1;
			continue;
		}
		break;
	}
	return false;
}

/*
 * Inspect the given string and determine the true "end" of the log message, in
 * order to find where to put a new Signed-off-by: line.  Ignored are
 * trailing comment lines and blank lines.  To support "git commit -s
 * --amend" on an existing commit, we also ignore "Conflicts:".  To
 * support "git commit -v", we truncate at cut lines.
 *
 * Returns the number of bytes from the tail to ignore, to be fed as
 * the second parameter to append_signoff().
 */
static size_t ignore_non_trailer(const char *buf, size_t len)
{
	size_t boc = 0, bol = 0;
	int in_old_conflicts_block = 0;
	size_t cutoff = len;

	while (bol < cutoff) {
		const char *next_line = memchr(buf + bol, '\n', len - bol);

		if (!next_line)
			next_line = buf + len;
		else
			next_line++;

		if (buf[bol] == COMMENT_LINE_CHAR || buf[bol] == '\n') {
			/* is this the first of the run of comments? */
			if (!boc)
				boc = bol;
			/* otherwise, it is just continuing */
		} else if (git__prefixcmp(buf + bol, "Conflicts:\n") == 0) {
			in_old_conflicts_block = 1;
			if (!boc)
				boc = bol;
		} else if (in_old_conflicts_block && buf[bol] == '\t') {
			; /* a pathname in the conflicts block */
		} else if (boc) {
			/* the previous was not trailing comment */
			boc = 0;
			in_old_conflicts_block = 0;
		}
		bol = next_line - buf;
	}
	return boc ? len - boc : len - cutoff;
}

/*
 * Return the position of the start of the patch or the length of str if there
 * is no patch in the message.
 */
static size_t find_patch_start(const char *str)
{
	const char *s;

	for (s = str; *s; s = next_line(s)) {
		if (git__prefixcmp(s, "---") == 0 && git__isspace(s[3]))
			return s - str;
	}

	return s - str;
}

/*
 * Return the position of the first trailer line or len if there are no
 * trailers.
 */
static size_t find_trailer_start(const char *buf, size_t len)
{
	const char *s;
	size_t end_of_title, l;
	int only_spaces = 1;
	int recognized_prefix = 0, trailer_lines = 0, non_trailer_lines = 0;
	/*
	 * Number of possible continuation lines encountered. This will be
	 * reset to 0 if we encounter a trailer (since those lines are to be
	 * considered continuations of that trailer), and added to
	 * non_trailer_lines if we encounter a non-trailer (since those lines
	 * are to be considered non-trailers).
	 */
	int possible_continuation_lines = 0;

	/* The first paragraph is the title and cannot be trailers */
	for (s = buf; s < buf + len; s = next_line(s)) {
		if (s[0] == COMMENT_LINE_CHAR)
			continue;
		if (is_blank_line(s))
			break;
	}
	end_of_title = s - buf;

	/*
	 * Get the start of the trailers by looking starting from the end for a
	 * blank line before a set of non-blank lines that (i) are all
	 * trailers, or (ii) contains at least one Git-generated trailer and
	 * consists of at least 25% trailers.
	 */
	l = len;
	while (last_line(&l, buf, l) && l >= end_of_title) {
		const char *bol = buf + l;
		const char *const *p;
		size_t separator_pos = 0;

		if (bol[0] == COMMENT_LINE_CHAR) {
			non_trailer_lines += possible_continuation_lines;
			possible_continuation_lines = 0;
			continue;
		}
		if (is_blank_line(bol)) {
			if (only_spaces)
				continue;
			non_trailer_lines += possible_continuation_lines;
			if (recognized_prefix &&
			    trailer_lines * 3 >= non_trailer_lines)
				return next_line(bol) - buf;
			else if (trailer_lines && !non_trailer_lines)
				return next_line(bol) - buf;
			return len;
		}
		only_spaces = 0;

		for (p = git_generated_prefixes; *p; p++) {
			if (git__prefixcmp(bol, *p) == 0) {
				trailer_lines++;
				possible_continuation_lines = 0;
				recognized_prefix = 1;
				goto continue_outer_loop;
			}
		}

		find_separator(&separator_pos, bol, TRAILER_SEPARATORS);
		if (separator_pos >= 1 && !git__isspace(bol[0])) {
			trailer_lines++;
			possible_continuation_lines = 0;
			if (recognized_prefix)
				continue;
		} else if (git__isspace(bol[0]))
			possible_continuation_lines++;
		else {
			non_trailer_lines++;
			non_trailer_lines += possible_continuation_lines;
			possible_continuation_lines = 0;
		}
continue_outer_loop:
		;
	}

	return len;
}

/* Return the position of the end of the trailers. */
static size_t find_trailer_end(const char *buf, size_t len)
{
	return len - ignore_non_trailer(buf, len);
}

static char *extract_trailer_block(const char *message, size_t *len)
{
	size_t patch_start = find_patch_start(message);
	size_t trailer_end = find_trailer_end(message, patch_start);
	size_t trailer_start = find_trailer_start(message, trailer_end);

	size_t trailer_len = trailer_end - trailer_start;

	char *buffer = git__malloc(trailer_len + 1);
	if (buffer == NULL)
		return NULL;

	memcpy(buffer, message + trailer_start, trailer_len);
	buffer[trailer_len] = 0;

	*len = trailer_len;

	return buffer;
}

enum trailer_state {
	S_START = 0,
	S_KEY = 1,
	S_KEY_WS = 2,
	S_SEP_WS = 3,
	S_VALUE = 4,
	S_VALUE_NL = 5,
	S_VALUE_END = 6,
	S_IGNORE = 7
};

#define NEXT(st) { state = (st); ptr++; continue; }
#define GOTO(st) { state = (st); continue; }

typedef git_array_t(git_message_trailer) git_array_trailer_t;

int git_message_trailers(git_message_trailer_array *trailer_arr, const char *message)
{
	enum trailer_state state = S_START;
	int rc = 0;
	char *ptr;
	char *key = NULL;
	char *value = NULL;
	git_array_trailer_t arr = GIT_ARRAY_INIT;

	size_t trailer_len;
	char *trailer = extract_trailer_block(message, &trailer_len);
	if (trailer == NULL)
		return -1;

	for (ptr = trailer;;) {
		switch (state) {
			case S_START: {
				if (*ptr == 0) {
					goto ret;
				}

				key = ptr;
				GOTO(S_KEY);
			}
			case S_KEY: {
				if (*ptr == 0) {
					goto ret;
				}

				if (git__isalnum(*ptr) || *ptr == '-') {
					/* legal key character */
					NEXT(S_KEY);
				}

				if (*ptr == ' ' || *ptr == '\t') {
					/* optional whitespace before separator */
					*ptr = 0;
					NEXT(S_KEY_WS);
				}

				if (strchr(TRAILER_SEPARATORS, *ptr)) {
					*ptr = 0;
					NEXT(S_SEP_WS);
				}

				/* illegal character */
				GOTO(S_IGNORE);
			}
			case S_KEY_WS: {
				if (*ptr == 0) {
					goto ret;
				}

				if (*ptr == ' ' || *ptr == '\t') {
					NEXT(S_KEY_WS);
				}

				if (strchr(TRAILER_SEPARATORS, *ptr)) {
					NEXT(S_SEP_WS);
				}

				/* illegal character */
				GOTO(S_IGNORE);
			}
			case S_SEP_WS: {
				if (*ptr == 0) {
					goto ret;
				}

				if (*ptr == ' ' || *ptr == '\t') {
					NEXT(S_SEP_WS);
				}

				value = ptr;
				NEXT(S_VALUE);
			}
			case S_VALUE: {
				if (*ptr == 0) {
					GOTO(S_VALUE_END);
				}

				if (*ptr == '\n') {
					NEXT(S_VALUE_NL);
				}

				NEXT(S_VALUE);
			}
			case S_VALUE_NL: {
				if (*ptr == ' ') {
					/* continuation; */
					NEXT(S_VALUE);
				}

				ptr[-1] = 0;
				GOTO(S_VALUE_END);
			}
			case S_VALUE_END: {
				git_message_trailer *t = git_array_alloc(arr);

				t->key = key;
				t->value = value;

				key = NULL;
				value = NULL;

				GOTO(S_START);
			}
			case S_IGNORE: {
				if (*ptr == 0) {
					goto ret;
				}

				if (*ptr == '\n') {
					NEXT(S_START);
				}

				NEXT(S_IGNORE);
			}
		}
	}

ret:
	trailer_arr->_trailer_block = trailer;
	trailer_arr->trailers = arr.ptr;
	trailer_arr->count = arr.size;

	return rc;
}

void git_message_trailer_array_free(git_message_trailer_array *arr)
{
	git__free(arr->_trailer_block);
	git__free(arr->trailers);
}
