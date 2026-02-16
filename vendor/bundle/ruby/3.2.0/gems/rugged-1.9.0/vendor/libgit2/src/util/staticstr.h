/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_stackstr_h__
#define INCLUDE_stackstr_h__

#include "git2_util.h"

typedef struct {
	/* Length of / number of bytes used by `data`. */
	size_t len;

	/* Size of the allocated `data` buffer. */
	size_t size;

	/* The actual string buffer data. */
	char data[GIT_FLEX_ARRAY];
} git_staticstr;

#define git_staticstr_with_size(__size) \
	struct { \
		size_t len; \
		size_t size; \
		char data[__size]; \
	}

#define git_staticstr_init(__str, __size) \
	do { \
		(__str)->len = 0; \
		(__str)->size = __size; \
		(__str)->data[0] = '\0'; \
	} while(0)

#define git_staticstr_offset(__str) \
	((__str)->data + (__str)->len)

#define git_staticstr_remain(__str) \
	((__str)->len > (__str)->size ? 0 : ((__str)->size - (__str)->len))

#define git_staticstr_increase(__str, __len) \
	do { ((__str)->len += __len); } while(0)

#define git_staticstr_consume_bytes(__str, __len) \
	do { git_staticstr_consume(__str, (__str)->data + __len); } while(0)

#define git_staticstr_consume(__str, __end) \
	do { \
		if (__end > (__str)->data && \
		    __end <= (__str)->data + (__str)->len) { \
			size_t __consumed = __end - (__str)->data; \
			memmove((__str)->data, __end, (__str)->len - __consumed); \
			(__str)->len -= __consumed; \
			(__str)->data[(__str)->len] = '\0'; \
		} \
	} while(0)

#define git_staticstr_clear(__str) \
	do { \
		(__str)->len = 0; \
		(__str)->data[0] = 0; \
	} while(0)

#endif
