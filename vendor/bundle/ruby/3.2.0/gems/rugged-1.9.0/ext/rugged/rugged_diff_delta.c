/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_cRuggedDiff;
VALUE rb_cRuggedDiffDelta;

VALUE rb_git_delta_file_fromC(const git_diff_file *file)
{
	VALUE rb_file;

	if (!file)
		return Qnil;

	rb_file = rb_hash_new();

	rb_hash_aset(rb_file, CSTR2SYM("oid"), rugged_create_oid(&file->id));
	rb_hash_aset(rb_file, CSTR2SYM("path"), file->path ? rb_str_new2(file->path) : Qnil);
	rb_hash_aset(rb_file, CSTR2SYM("size"), INT2FIX(file->size));
	rb_hash_aset(rb_file, CSTR2SYM("flags"), UINT2NUM(file->flags));
	rb_hash_aset(rb_file, CSTR2SYM("mode"), UINT2NUM(file->mode));

	return rb_file;
}

static VALUE rb_git_delta_status_fromC(git_delta_t status)
{
	switch(status) {
		case GIT_DELTA_UNMODIFIED:
			return CSTR2SYM("unmodified");
		case GIT_DELTA_ADDED:
			return CSTR2SYM("added");
		case GIT_DELTA_DELETED:
			return CSTR2SYM("deleted");
		case GIT_DELTA_MODIFIED:
			return CSTR2SYM("modified");
		case GIT_DELTA_RENAMED:
			return CSTR2SYM("renamed");
		case GIT_DELTA_COPIED:
			return CSTR2SYM("copied");
		case GIT_DELTA_IGNORED:
			return CSTR2SYM("ignored");
		case GIT_DELTA_UNTRACKED:
			return CSTR2SYM("untracked");
		case GIT_DELTA_TYPECHANGE:
			return CSTR2SYM("typechange");
		default:
			return CSTR2SYM("unknown");
	}
}

static VALUE rb_git_delta_status_char_fromC(git_delta_t status)
{
	char status_char[2];

	status_char[0] = git_diff_status_char(status);
	status_char[1] = '\0';

	return CSTR2SYM(status_char);
}

VALUE rugged_diff_delta_new(VALUE owner, const git_diff_delta *delta)
{
	VALUE rb_delta = rb_class_new_instance(0, NULL, rb_cRuggedDiffDelta);

	rugged_set_owner(rb_delta, owner);
	rb_iv_set(rb_delta, "@old_file", rb_git_delta_file_fromC(&delta->old_file));
	rb_iv_set(rb_delta, "@new_file", rb_git_delta_file_fromC(&delta->new_file));
	rb_iv_set(rb_delta, "@similarity", INT2FIX(delta->similarity));
	rb_iv_set(rb_delta, "@status", rb_git_delta_status_fromC(delta->status));
	rb_iv_set(rb_delta, "@status_char", rb_git_delta_status_char_fromC(delta->status));
	rb_iv_set(rb_delta, "@binary",
		(!(delta->flags & GIT_DIFF_FLAG_NOT_BINARY) &&
		 (delta->flags & GIT_DIFF_FLAG_BINARY)) ? Qtrue : Qfalse
	);

	return rb_delta;
}

void Init_rugged_diff_delta(void)
{
	rb_cRuggedDiffDelta = rb_define_class_under(rb_cRuggedDiff, "Delta", rb_cObject);
}
