/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_hashmap_oid_h__
#define INCLUDE_hashmap_oid_h__

#include "hashmap.h"

GIT_INLINE(uint32_t) git_hashmap_oid_hashcode(const git_oid *oid)
{
	uint32_t hash;
	memcpy(&hash, oid->id, sizeof(uint32_t));
	return hash;
}

#define GIT_HASHMAP_OID_STRUCT(name, val_t) \
	GIT_HASHMAP_STRUCT(name, const git_oid *, val_t)
#define GIT_HASHMAP_OID_PROTOTYPES(name, val_t) \
	GIT_HASHMAP_PROTOTYPES(name, const git_oid *, val_t)
#define GIT_HASHMAP_OID_FUNCTIONS(name, scope, val_t) \
	GIT_HASHMAP_FUNCTIONS(name, scope, const git_oid *, val_t, git_hashmap_oid_hashcode, git_oid_equal)

#define GIT_HASHMAP_OID_SETUP(name, val_t) \
	GIT_HASHMAP_OID_STRUCT(name, val_t) \
	GIT_HASHMAP_OID_FUNCTIONS(name, GIT_HASHMAP_INLINE, val_t)

#endif
