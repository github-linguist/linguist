/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_path_w32_h__
#define INCLUDE_win32_path_w32_h__

#include "git2_util.h"

/**
 * Create a Win32 path (in UCS-2 format) from a UTF-8 string.  If the given
 * path is relative, then it will be turned into an absolute path by having
 * the current working directory prepended.
 *
 * @param dest The buffer to receive the wide string.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
extern int git_win32_path_from_utf8(git_win32_path dest, const char *src);

/**
 * Create a Win32 path (in UCS-2 format) from a UTF-8 string.  If the given
 * path is relative, then it will not be turned into an absolute path.
 *
 * @param dest The buffer to receive the wide string.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
extern int git_win32_path_relative_from_utf8(git_win32_path dest, const char *src);

/**
 * Canonicalize a Win32 UCS-2 path so that it is suitable for delivery to the
 * Win32 APIs: remove multiple directory separators, squashing to a single one,
 * strip trailing directory separators, ensure directory separators are all
 * canonical (always backslashes, never forward slashes) and process any
 * directory entries of '.' or '..'.
 *
 * Note that this is intended to be used on absolute Windows paths, those
 * that start with `C:\`, `\\server\share`, `\\?\`, etc.
 *
 * This processes the buffer in place.
 *
 * @param path The buffer to process
 * @return The new length of the buffer, in wchar_t's (not counting the NULL terminator)
 */
extern int git_win32_path_canonicalize(git_win32_path path);

/**
 * Create an internal format (posix-style) UTF-8 path from a Win32 UCS-2 path.
 *
 * @param dest The buffer to receive the UTF-8 string.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes (not counting the NULL terminator), or < 0 for failure
 */
extern int git_win32_path_to_utf8(git_win32_utf8_path dest, const wchar_t *src);

/**
 * Get the short name for the terminal path component in the given path.
 * For example, given "C:\Foo\Bar\Asdf.txt", this will return the short name
 * for the file "Asdf.txt".
 *
 * @param path The given path in UTF-8
 * @return The name of the shortname for the given path
 */
extern char *git_win32_path_8dot3_name(const char *path);

extern int git_win32_path_readlink_w(git_win32_path dest, const git_win32_path path);

/**
 * Removes any trailing backslashes from a path, except in the case of a drive
 * letter path (C:\, D:\, etc.). This function cannot fail.
 *
 * @param path The path which should be trimmed.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32_path_trim_end(wchar_t *str, size_t len);

/**
 * Removes any of the following namespace prefixes from a path,
 * if found: "\??\", "\\?\", "\\?\UNC\". This function cannot fail.
 *
 * @param path The path which should be converted.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32_path_remove_namespace(wchar_t *str, size_t len);

int git_win32_path_find_executable(git_win32_path fullpath, wchar_t* exe);

#endif
