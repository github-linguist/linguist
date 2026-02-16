/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_steams_schannel_h__
#define INCLUDE_steams_schannel_h__

#include "common.h"

#include "git2/sys/stream.h"

#ifdef GIT_SCHANNEL

extern int git_schannel_stream_new(
	git_stream **out,
	const char *host,
	const char *port);

extern int git_schannel_stream_wrap(
	git_stream **out,
	git_stream *in,
	const char *host);

#endif

#endif
