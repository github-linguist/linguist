/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_map_h__
#define INCLUDE_map_h__

#include "git2_util.h"


/* p_mmap() prot values */
#define GIT_PROT_NONE 0x0
#define GIT_PROT_READ 0x1
#define GIT_PROT_WRITE 0x2
#define GIT_PROT_EXEC 0x4

/* git__mmmap() flags values */
#define GIT_MAP_FILE	0
#define GIT_MAP_SHARED 1
#define GIT_MAP_PRIVATE 2
#define GIT_MAP_TYPE	0xf
#define GIT_MAP_FIXED	0x10

#ifdef __amigaos4__
#define MAP_FAILED 0
#endif

typedef struct { /* memory mapped buffer	*/
	void *data; /* data bytes			*/
	size_t len; /* data length			*/
#ifdef GIT_WIN32
	HANDLE fmh; /* file mapping handle */
#endif
} git_map;

#define GIT_MMAP_VALIDATE(out, len, prot, flags) do { \
	GIT_ASSERT(out != NULL && len > 0); \
	GIT_ASSERT((prot & GIT_PROT_WRITE) || (prot & GIT_PROT_READ)); \
	GIT_ASSERT((flags & GIT_MAP_FIXED) == 0); } while (0)

extern int p_mmap(git_map *out, size_t len, int prot, int flags, int fd, off64_t offset);
extern int p_munmap(git_map *map);

#endif
