/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_win32_compat_h__
#define INCLUDE_win32_win32_compat_h__

#include <stdint.h>
#include <time.h>
#include <wchar.h>
#include <sys/stat.h>
#include <sys/types.h>

typedef long suseconds_t;

struct p_timeval {
	time_t tv_sec;
	suseconds_t tv_usec;
};

struct p_timespec {
	time_t tv_sec;
	long tv_nsec;
};

#define timespec p_timespec

struct p_stat {
	_dev_t st_dev;
	_ino_t st_ino;
	mode_t st_mode;
	short st_nlink;
	short st_uid;
	short st_gid;
	_dev_t st_rdev;
	__int64 st_size;
	struct timespec st_atim;
	struct timespec st_mtim;
	struct timespec st_ctim;
#define st_atime st_atim.tv_sec
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec
#define st_atime_nsec st_atim.tv_nsec
#define st_mtime_nsec st_mtim.tv_nsec
#define st_ctime_nsec st_ctim.tv_nsec
};

#define stat p_stat

#endif
