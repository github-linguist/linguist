/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_dir_h__
#define INCLUDE_win32_dir_h__

#include "git2_util.h"

#include "w32_util.h"

struct git__dirent {
	int d_ino;
	git_win32_utf8_path d_name;
};

typedef struct {
	HANDLE h;
	WIN32_FIND_DATAW f;
	struct git__dirent entry;
	int first;
	char dir[GIT_FLEX_ARRAY];
} git__DIR;

extern git__DIR *git__opendir(const char *);
extern struct git__dirent *git__readdir(git__DIR *);
extern int git__readdir_ext(
	git__DIR *, struct git__dirent *, struct git__dirent **, int *);
extern void git__rewinddir(git__DIR *);
extern int git__closedir(git__DIR *);

# ifndef GIT__WIN32_NO_WRAP_DIR
#	define dirent git__dirent
#	define DIR git__DIR
#	define opendir	git__opendir
#	define readdir	git__readdir
#   define readdir_r(d,e,r) git__readdir_ext((d),(e),(r),NULL)
#	define rewinddir git__rewinddir
#	define closedir git__closedir
# endif

#endif
