/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_str_h__
#define INCLUDE_str_h__

#include "git2_util.h"

struct git_str {
	char *ptr;
	size_t asize;
	size_t size;
};

typedef enum {
	GIT_STR_BOM_NONE = 0,
	GIT_STR_BOM_UTF8 = 1,
	GIT_STR_BOM_UTF16_LE = 2,
	GIT_STR_BOM_UTF16_BE = 3,
	GIT_STR_BOM_UTF32_LE = 4,
	GIT_STR_BOM_UTF32_BE = 5
} git_str_bom_t;

typedef struct {
	git_str_bom_t bom; /* BOM found at head of text */
	unsigned int nul, cr, lf, crlf; /* NUL, CR, LF and CRLF counts */
	unsigned int printable, nonprintable; /* These are just approximations! */
} git_str_text_stats;

extern char git_str__initstr[];
extern char git_str__oom[];

/* Use to initialize string buffer structure when git_str is on stack */
#define GIT_STR_INIT { git_str__initstr, 0, 0 }

/**
 * Static initializer for git_str from static string buffer
 */
#define GIT_STR_INIT_CONST(str, len) { (char *)(str), 0, (size_t)(len) }

GIT_INLINE(bool) git_str_is_allocated(const git_str *str)
{
	return (str->ptr != NULL && str->asize > 0);
}

/**
 * Initialize a git_str structure.
 *
 * For the cases where GIT_STR_INIT cannot be used to do static
 * initialization.
 */
extern int git_str_init(git_str *str, size_t initial_size);

extern void git_str_dispose(git_str *str);

/**
 * Resize the string buffer allocation to make more space.
 *
 * This will attempt to grow the string buffer to accommodate the target
 * size.  The bstring buffer's `ptr` will be replaced with a newly
 * allocated block of data.  Be careful so that memory allocated by the
 * caller is not lost.  As a special variant, if you pass `target_size` as
 * 0 and the memory is not allocated by libgit2, this will allocate a new
 * buffer of size `size` and copy the external data into it.
 *
 * Currently, this will never shrink a buffer, only expand it.
 *
 * If the allocation fails, this will return an error and the buffer will be
 * marked as invalid for future operations, invaliding the contents.
 *
 * @param str The buffer to be resized; may or may not be allocated yet
 * @param target_size The desired available size
 * @return 0 on success, -1 on allocation failure
 */
int git_str_grow(git_str *str, size_t target_size);

/**
 * Resize the buffer allocation to make more space.
 *
 * This will attempt to grow the string buffer to accommodate the
 * additional size.  It is similar to `git_str_grow`, but performs the
 * new size calculation, checking for overflow.
 *
 * Like `git_str_grow`, if this is a user-supplied string buffer,
 * this will allocate a new string uffer.
 */
extern int git_str_grow_by(git_str *str, size_t additional_size);

/**
 * Attempt to grow the buffer to hold at least `target_size` bytes.
 *
 * If the allocation fails, this will return an error.  If `mark_oom` is
 * true, this will mark the string buffer as invalid for future
 * operations; if false, existing string buffer content will be preserved,
 * but calling code must handle that string buffer was not expanded.  If
 * `preserve_external` is true, then any existing data pointed to be
 * `ptr` even if `asize` is zero will be copied into the newly allocated
 * string buffer.
 */
extern int git_str_try_grow(
	git_str *str, size_t target_size, bool mark_oom);

extern void git_str_swap(git_str *str_a, git_str *str_b);
extern char *git_str_detach(git_str *str);
extern int git_str_attach(git_str *str, char *ptr, size_t asize);

/* Populates a `git_str` where the contents are not "owned" by the string
 * buffer, and calls to `git_str_dispose` will not free the given str.
 */
extern void git_str_attach_notowned(
	git_str *str, const char *ptr, size_t size);

/**
 * Test if there have been any reallocation failures with this git_str.
 *
 * Any function that writes to a git_str can fail due to memory allocation
 * issues.  If one fails, the git_str will be marked with an OOM error and
 * further calls to modify the string buffer will fail.  Check
 * git_str_oom() at the end of your sequence and it will be true if you
 * ran out of memory at any point with that string buffer.
 *
 * @return false if no error, true if allocation error
 */
GIT_INLINE(bool) git_str_oom(const git_str *str)
{
	return (str->ptr == git_str__oom);
}

/*
 * Functions below that return int value error codes will return 0 on
 * success or -1 on failure (which generally means an allocation failed).
 * Using a git_str where the allocation has failed with result in -1 from
 * all further calls using that string buffer.  As a result, you can
 * ignore the return code of these functions and call them in a series
 * then just call git_str_oom at the end.
 */

int git_str_set(git_str *str, const void *data, size_t datalen);

int git_str_sets(git_str *str, const char *string);
int git_str_putc(git_str *str, char c);
int git_str_putcn(git_str *str, char c, size_t len);
int git_str_put(git_str *str, const char *data, size_t len);
int git_str_puts(git_str *str, const char *string);
int git_str_printf(git_str *str, const char *format, ...) GIT_FORMAT_PRINTF(2, 3);
int git_str_vprintf(git_str *str, const char *format, va_list ap);
void git_str_clear(git_str *str);
void git_str_consume_bytes(git_str *str, size_t len);
void git_str_consume(git_str *str, const char *end);
void git_str_truncate(git_str *str, size_t len);
void git_str_shorten(git_str *str, size_t amount);
void git_str_truncate_at_char(git_str *path, char separator);
void git_str_rtruncate_at_char(git_str *path, char separator);

/** General join with separator */
int git_str_join_n(git_str *str, char separator, int len, ...);
/** Fast join of two strings - first may legally point into `str` data */
int git_str_join(git_str *str, char separator, const char *str_a, const char *str_b);
/** Fast join of three strings - cannot reference `str` data */
int git_str_join3(git_str *str, char separator, const char *str_a, const char *str_b, const char *str_c);

/**
 * Join two strings as paths, inserting a slash between as needed.
 * @return 0 on success, -1 on failure
 */
GIT_INLINE(int) git_str_joinpath(git_str *str, const char *a, const char *b)
{
	return git_str_join(str, '/', a, b);
}

GIT_INLINE(const char *) git_str_cstr(const git_str *str)
{
	return str->ptr;
}

GIT_INLINE(size_t) git_str_len(const git_str *str)
{
	return str->size;
}

int git_str_copy_cstr(char *data, size_t datasize, const git_str *str);

#define git_str_PUTS(str, cstr) git_str_put(str, cstr, sizeof(cstr) - 1)

GIT_INLINE(ssize_t) git_str_rfind_next(const git_str *str, char ch)
{
	ssize_t idx = (ssize_t)str->size - 1;
	while (idx >= 0 && str->ptr[idx] == ch) idx--;
	while (idx >= 0 && str->ptr[idx] != ch) idx--;
	return idx;
}

GIT_INLINE(ssize_t) git_str_rfind(const git_str *str, char ch)
{
	ssize_t idx = (ssize_t)str->size - 1;
	while (idx >= 0 && str->ptr[idx] != ch) idx--;
	return idx;
}

GIT_INLINE(ssize_t) git_str_find(const git_str *str, char ch)
{
	void *found = memchr(str->ptr, ch, str->size);
	return found ? (ssize_t)((const char *)found - str->ptr) : -1;
}

/* Remove whitespace from the end of the string buffer */
void git_str_rtrim(git_str *str);

int git_str_cmp(const git_str *a, const git_str *b);

/* Quote and unquote a string buffer as specified in
 * http://marc.info/?l=git&m=112927316408690&w=2
 */
int git_str_quote(git_str *str);
int git_str_unquote(git_str *str);

/* Write data as a hex string */
int git_str_encode_hexstr(git_str *str, const char *data, size_t len);

/* Write data as base64 encoded in string buffer */
int git_str_encode_base64(git_str *str, const char *data, size_t len);
/* Decode the given bas64 and write the result to the string buffer */
int git_str_decode_base64(git_str *str, const char *base64, size_t len);

/* Write data as "base85" encoded in string buffer */
int git_str_encode_base85(git_str *str, const char *data, size_t len);
/* Decode the given "base85" and write the result to the string buffer */
int git_str_decode_base85(git_str *str, const char *base64, size_t len, size_t output_len);

/*
 * Decode the given percent-encoded string and write the result to the
 * string buffer.
 */
int git_str_decode_percent(git_str *str, const char *encoded, size_t len);

/*
 * Insert, remove or replace a portion of the string buffer.
 *
 * @param str The string buffer to work with
 *
 * @param where The location in the string buffer where the transformation
 * should be applied.
 *
 * @param nb_to_remove The number of chars to be removed. 0 to not
 * remove any character in the string buffer.
 *
 * @param data A pointer to the data which should be inserted.
 *
 * @param nb_to_insert The number of chars to be inserted. 0 to not
 * insert any character from the string buffer.
 *
 * @return 0 or an error code.
 */
int git_str_splice(
	git_str *str,
	size_t where,
	size_t nb_to_remove,
	const char *data,
	size_t nb_to_insert);

/**
 * Append string to string buffer, prefixing each character from
 * `esc_chars` with `esc_with` string.
 *
 * @param str String buffer to append data to
 * @param string String to escape and append
 * @param esc_chars Characters to be escaped
 * @param esc_with String to insert in from of each found character
 * @return 0 on success, <0 on failure (probably allocation problem)
 */
extern int git_str_puts_escaped(
	git_str *str,
	const char *string,
	const char *esc_chars,
	const char *esc_with);

/**
 * Append string escaping characters that are regex special
 */
GIT_INLINE(int) git_str_puts_escape_regex(git_str *str, const char *string)
{
	return git_str_puts_escaped(str, string, "^.[]$()|*+?{}\\", "\\");
}

/**
 * Unescape all characters in a string buffer in place
 *
 * I.e. remove backslashes
 */
extern void git_str_unescape(git_str *str);

/**
 * Replace all \r\n with \n.
 *
 * @return 0 on success, -1 on memory error
 */
extern int git_str_crlf_to_lf(git_str *tgt, const git_str *src);

/**
 * Replace all \n with \r\n. Does not modify existing \r\n.
 *
 * @return 0 on success, -1 on memory error
 */
extern int git_str_lf_to_crlf(git_str *tgt, const git_str *src);

/**
 * Fill string buffer with the common prefix of a array of strings
 *
 * String buffer will be set to empty if there is no common prefix
 */
extern int git_str_common_prefix(git_str *buf, char *const *const strings, size_t count);

/**
 * Check if a string buffer begins with a UTF BOM
 *
 * @param bom Set to the type of BOM detected or GIT_BOM_NONE
 * @param str String buffer in which to check the first bytes for a BOM
 * @return Number of bytes of BOM data (or 0 if no BOM found)
 */
extern int git_str_detect_bom(git_str_bom_t *bom, const git_str *str);

/**
 * Gather stats for a piece of text
 *
 * Fill the `stats` structure with counts of unreadable characters, carriage
 * returns, etc, so it can be used in heuristics.  This automatically skips
 * a trailing EOF (\032 character).  Also it will look for a BOM at the
 * start of the text and can be told to skip that as well.
 *
 * @param stats Structure to be filled in
 * @param str Text to process
 * @param skip_bom Exclude leading BOM from stats if true
 * @return Does the string buffer heuristically look like binary data
 */
extern bool git_str_gather_text_stats(
	git_str_text_stats *stats, const git_str *str, bool skip_bom);

/**
* Check quickly if string buffer looks like it contains binary data
*
* @param str string buffer to check
* @return 1 if string buffer looks like non-text data
*/
int git_str_is_binary(const git_str *str);

/**
* Check quickly if buffer contains a NUL byte
*
* @param str string buffer to check
* @return 1 if string buffer contains a NUL byte
*/
int git_str_contains_nul(const git_str *str);

#endif
