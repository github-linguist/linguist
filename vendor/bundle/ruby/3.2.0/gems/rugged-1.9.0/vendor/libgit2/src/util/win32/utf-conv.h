/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_utf_conv_h__
#define INCLUDE_win32_utf_conv_h__

#include "git2_util.h"

#include <wchar.h>

#ifndef WC_ERR_INVALID_CHARS
# define WC_ERR_INVALID_CHARS	0x80
#endif

/**
 * Converts a NUL-terminated UTF-8 string to wide characters. This is a
 * convenience function for `git_utf8_to_16_with_len`.
 *
 * @param dest The buffer to receive the wide string.
 * @param dest_size The size of the buffer, in characters.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_to_16(wchar_t *dest, size_t dest_size, const char *src);

/**
 * Converts a UTF-8 string to wide characters.
 *
 * @param dest The buffer to receive the wide string.
 * @param dest_size The size of the buffer, in characters.
 * @param src The UTF-8 string to convert.
 * @param src_len The length of the string to convert.
 * @return The length of the wide string, in characters
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_to_16_with_len(
	wchar_t *dest,
	size_t dest_size,
	const char *src,
	int src_len);

/**
 * Converts a NUL-terminated wide string to UTF-8. This is a convenience
 * function for `git_utf8_from_16_with_len`.
 *
 * @param dest The buffer to receive the UTF-8 string.
 * @param dest_size The size of the buffer, in bytes.
 * @param src The wide string to convert.
 * @param src_len The length of the string to convert.
 * @return The length of the UTF-8 string, in bytes
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_from_16(char *dest, size_t dest_size, const wchar_t *src);

/**
 * Converts a wide string to UTF-8.
 *
 * @param dest The buffer to receive the UTF-8 string.
 * @param dest_size The size of the buffer, in bytes.
 * @param src The wide string to convert.
 * @param src_len The length of the string to convert.
 * @return The length of the UTF-8 string, in bytes
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_from_16_with_len(char *dest, size_t dest_size, const wchar_t *src, int src_len);

/**
 * Converts a UTF-8 string to wide characters. Memory is allocated to hold
 * the converted string. The caller is responsible for freeing the string
 * with git__free.
 *
 * @param dest Receives a pointer to the wide string.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_to_16_alloc(wchar_t **dest, const char *src);

/**
 * Converts a UTF-8 string to wide characters. Memory is allocated to hold
 * the converted string. The caller is responsible for freeing the string
 * with git__free.
 *
 * @param dest Receives a pointer to the wide string.
 * @param src The UTF-8 string to convert.
 * @param src_len The length of the string.
 * @return The length of the wide string, in characters
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_to_16_alloc_with_len(
	wchar_t **dest,
	const char *src,
	int src_len);

/**
 * Converts a wide string to UTF-8. Memory is allocated to hold the
 * converted string. The caller is responsible for freeing the string
 * with git__free.
 *
 * @param dest Receives a pointer to the UTF-8 string.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_from_16_alloc(char **dest, const wchar_t *src);

/**
 * Converts a wide string to UTF-8. Memory is allocated to hold the
 * converted string. The caller is responsible for freeing the string
 * with git__free.
 *
 * @param dest Receives a pointer to the UTF-8 string.
 * @param src The wide string to convert.
 * @param src_len The length of the wide string.
 * @return The length of the UTF-8 string, in bytes
 *         (not counting the NULL terminator), or < 0 for failure
 */
int git_utf8_from_16_alloc_with_len(
	char **dest,
	const wchar_t *src,
	int src_len);

#endif
