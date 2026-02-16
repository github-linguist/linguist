/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_transports_libssh2_h__
#define INCLUDE_transports_libssh2_h__

#include "common.h"

#include "git2.h"
#include "git2/transport.h"
#include "git2/sys/transport.h"

int git_transport_ssh_libssh2_global_init(void);

int git_smart_subtransport_ssh_libssh2(
	git_smart_subtransport **out,
	git_transport *owner,
	void *param);

int git_smart_subtransport_ssh_libssh2_set_paths(
	git_smart_subtransport *subtransport,
	const char *cmd_uploadpack,
	const char *cmd_receivepack);

#endif
