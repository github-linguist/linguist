/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "ssh_exec.h"
#include "ssh_libssh2.h"

#include "transports/smart.h"

int git_smart_subtransport_ssh(
	git_smart_subtransport **out,
	git_transport *owner,
	void *param)
{
#ifdef GIT_SSH_LIBSSH2
	return git_smart_subtransport_ssh_libssh2(out, owner, param);
#elif GIT_SSH_EXEC
	return git_smart_subtransport_ssh_exec(out, owner, param);
#else
	GIT_UNUSED(out);
	GIT_UNUSED(owner);
	GIT_UNUSED(param);

	git_error_set(GIT_ERROR_INVALID, "cannot create SSH transport; library was built without SSH support");
	return -1;
#endif
}

static int transport_set_paths(git_transport *t, git_strarray *paths)
{
	transport_smart *smart = (transport_smart *)t;

#ifdef GIT_SSH_LIBSSH2
	return git_smart_subtransport_ssh_libssh2_set_paths(
		(git_smart_subtransport *)smart->wrapped,
		paths->strings[0],
		paths->strings[1]);
#elif GIT_SSH_EXEC
	return git_smart_subtransport_ssh_exec_set_paths(
		(git_smart_subtransport *)smart->wrapped,
		paths->strings[0],
		paths->strings[1]);
#else
	GIT_UNUSED(t);
	GIT_UNUSED(smart);
	GIT_UNUSED(paths);

	GIT_ASSERT(!"cannot create SSH library; library was built without SSH support");
	return -1;
#endif
}

int git_transport_ssh_with_paths(
	git_transport **out,
	git_remote *owner,
	void *payload)
{
	git_strarray *paths = (git_strarray *) payload;
	git_transport *transport;
	int error;

	git_smart_subtransport_definition ssh_definition = {
		git_smart_subtransport_ssh,
		0, /* no RPC */
		NULL
	};

	if (paths->count != 2) {
		git_error_set(GIT_ERROR_SSH, "invalid ssh paths, must be two strings");
		return GIT_EINVALIDSPEC;
	}

	if ((error = git_transport_smart(&transport, owner, &ssh_definition)) < 0)
		return error;

	if ((error = transport_set_paths(transport, paths)) < 0)
		return error;

	*out = transport;
	return 0;
}

