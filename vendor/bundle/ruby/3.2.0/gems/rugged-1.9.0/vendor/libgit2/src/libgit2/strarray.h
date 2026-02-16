/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_strarray_h__
#define INCLUDE_strarray_h__

#include "common.h"
#include "git2/strarray.h"

/**
 * Copy a string array object from source to target.
 *
 * Note: target is overwritten and hence should be empty, otherwise its
 * contents are leaked.  Call git_strarray_free() if necessary.
 *
 * @param tgt target
 * @param src source
 * @return 0 on success, < 0 on allocation failure
 */
extern int git_strarray_copy(git_strarray *tgt, const git_strarray *src);

#endif
