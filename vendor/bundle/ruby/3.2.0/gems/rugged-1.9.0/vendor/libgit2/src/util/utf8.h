/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_utf8_h__
#define INCLUDE_utf8_h__

#include "git2_util.h"

/*
 * Iterate through an UTF-8 string, yielding one codepoint at a time.
 *
 * @param out pointer where to store the current codepoint
 * @param str current position in the string
 * @param str_len size left in the string
 * @return length in bytes of the read codepoint; -1 if the codepoint was invalid
 */
extern int git_utf8_iterate(uint32_t *out, const char *str, size_t str_len);

/**
 * Returns the number of characters in the given string.
 *
 * This function will count invalid codepoints; if any given byte is
 * not part of a valid UTF-8 codepoint, then it will be counted toward
 * the length in characters.
 *
 * In other words:
 *   0x24 (U+0024 "$") has length 1
 *   0xc2 0xa2 (U+00A2 "¢") has length 1
 *   0x24 0xc2 0xa2 (U+0024 U+00A2 "$¢") has length 2
 *   0xf0 0x90 0x8d 0x88 (U+10348 "𐍈") has length 1
 *   0x24 0xc0 0xc1 0x34 (U+0024 <invalid> <invalid> "4) has length 4
 *
 * @param str string to scan
 * @param str_len size of the string
 * @return length in characters of the string
 */
extern size_t git_utf8_char_length(const char *str, size_t str_len);

/**
 * Iterate through an UTF-8 string and stops after finding any invalid UTF-8
 * codepoints.
 *
 * @param str string to scan
 * @param str_len size of the string
 * @return length in bytes of the string that contains valid data
 */
extern size_t git_utf8_valid_buf_length(const char *str, size_t str_len);

#endif
