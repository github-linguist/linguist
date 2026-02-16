/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "trace.h"

#include "str.h"
#include "runtime.h"
#include "git2/trace.h"

struct git_trace_data git_trace__data = {0};

int git_trace_set(git_trace_level_t level, git_trace_cb callback)
{
	GIT_ASSERT_ARG(level == 0 || callback != NULL);

	git_trace__data.level = level;
	git_trace__data.callback = callback;
	GIT_MEMORY_BARRIER;

	return 0;
}
