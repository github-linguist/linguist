/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_note_h__
#define INCLUDE_note_h__

#include "common.h"

#include "git2/oid.h"
#include "git2/types.h"

#define GIT_NOTES_DEFAULT_REF "refs/notes/commits"

#define GIT_NOTES_DEFAULT_MSG_ADD \
	"Notes added by 'git_note_create' from libgit2"

#define GIT_NOTES_DEFAULT_MSG_RM \
	"Notes removed by 'git_note_remove' from libgit2"

struct git_note {
	git_oid id;

	git_signature *author;
	git_signature *committer;

	char *message;
};

#endif
