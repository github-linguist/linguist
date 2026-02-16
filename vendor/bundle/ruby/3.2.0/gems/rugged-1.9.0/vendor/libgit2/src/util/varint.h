/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_varint_h__
#define INCLUDE_varint_h__

#include "git2_util.h"

#include <stdint.h>

extern int git_encode_varint(unsigned char *, size_t, uintmax_t);
extern uintmax_t git_decode_varint(const unsigned char *, size_t *);

#endif
