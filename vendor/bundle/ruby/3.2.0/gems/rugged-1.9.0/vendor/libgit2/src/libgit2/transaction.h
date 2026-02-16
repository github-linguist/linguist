/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_transaction_h__
#define INCLUDE_transaction_h__

#include "common.h"

int git_transaction_config_new(
	git_transaction **out,
	git_config *cfg,
	void *data);

#endif
