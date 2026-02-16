/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"

#if !defined(GIT_THREADS)

#define TLSDATA_MAX 16

typedef struct {
	void *value;
	void (GIT_SYSTEM_CALL *destroy_fn)(void *);
} tlsdata_value;

static tlsdata_value tlsdata_values[TLSDATA_MAX];
static int tlsdata_cnt = 0;

int git_tlsdata_init(git_tlsdata_key *key, void (GIT_SYSTEM_CALL *destroy_fn)(void *))
{
	if (tlsdata_cnt >= TLSDATA_MAX)
		return -1;

	tlsdata_values[tlsdata_cnt].value = NULL;
	tlsdata_values[tlsdata_cnt].destroy_fn = destroy_fn;

	*key = tlsdata_cnt;
	tlsdata_cnt++;

	return 0;
}

int git_tlsdata_set(git_tlsdata_key key, void *value)
{
	if (key < 0 || key > tlsdata_cnt)
		return -1;

	tlsdata_values[key].value = value;
	return 0;
}

void *git_tlsdata_get(git_tlsdata_key key)
{
	if (key < 0 || key > tlsdata_cnt)
		return NULL;

	return tlsdata_values[key].value;
}

int git_tlsdata_dispose(git_tlsdata_key key)
{
	void *value;
	void (*destroy_fn)(void *) = NULL;

	if (key < 0 || key > tlsdata_cnt)
		return -1;

	value = tlsdata_values[key].value;
	destroy_fn = tlsdata_values[key].destroy_fn;

	tlsdata_values[key].value = NULL;
	tlsdata_values[key].destroy_fn = NULL;

	if (value && destroy_fn)
		destroy_fn(value);

	return 0;
}

#elif defined(GIT_WIN32)

int git_tlsdata_init(git_tlsdata_key *key, void (GIT_SYSTEM_CALL *destroy_fn)(void *))
{
	DWORD fls_index = FlsAlloc(destroy_fn);

	if (fls_index == FLS_OUT_OF_INDEXES)
		return -1;

	*key = fls_index;
	return 0;
}

int git_tlsdata_set(git_tlsdata_key key, void *value)
{
	if (!FlsSetValue(key, value))
		return -1;

	return 0;
}

void *git_tlsdata_get(git_tlsdata_key key)
{
	return FlsGetValue(key);
}

int git_tlsdata_dispose(git_tlsdata_key key)
{
	if (!FlsFree(key))
		return -1;

	return 0;
}

#elif defined(_POSIX_THREADS)

int git_tlsdata_init(git_tlsdata_key *key, void (GIT_SYSTEM_CALL *destroy_fn)(void *))
{
	if (pthread_key_create(key, destroy_fn) != 0)
		return -1;

	return 0;
}

int git_tlsdata_set(git_tlsdata_key key, void *value)
{
	if (pthread_setspecific(key, value) != 0)
		return -1;

	return 0;
}

void *git_tlsdata_get(git_tlsdata_key key)
{
	return pthread_getspecific(key);
}

int git_tlsdata_dispose(git_tlsdata_key key)
{
	if (pthread_key_delete(key) != 0)
		return -1;

	return 0;
}

#else
# error unknown threading model
#endif
