/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_oidarray_h__
#define INCLUDE_oidarray_h__

#include "common.h"

#include "git2/oidarray.h"
#include "array.h"

typedef git_array_t(git_oid) git_array_oid_t;

extern void git_oidarray__reverse(git_oidarray *arr);
extern void git_oidarray__from_array(git_oidarray *out, const git_array_oid_t *array);
extern void git_oidarray__to_array(git_array_oid_t *out, const git_oidarray *array);

int git_oidarray__add(git_array_oid_t *arr, git_oid *id);
bool git_oidarray__remove(git_array_oid_t *arr, git_oid *id);

#endif
