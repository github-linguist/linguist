/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedObject;
extern VALUE rb_cRuggedReference;
VALUE rb_cRuggedBranch;

static inline VALUE rugged_branch_new(VALUE owner, git_reference *ref)
{
	return rugged_ref_new(rb_cRuggedBranch, owner, ref);
}

/*
 *  call-seq:
 *    branch.head? -> true or false
 *
 *  Returns +true+ if the branch is pointed at by +HEAD+, +false+ otherwise.
 */
static VALUE rb_git_branch_head_p(VALUE self)
{
	git_reference *branch;
	Data_Get_Struct(self, git_reference, branch);
	return git_branch_is_head(branch) ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    branch.name -> string
 *
 *  Returns the name of +branch+.
 *
 *  See Rugged::Reference#canonical_name if you need the fully qualified
 *  name of the underlying reference.
 */
static VALUE rb_git_branch_name(VALUE self)
{
	git_reference *branch;
	const char *branch_name;
	Data_Get_Struct(self, git_reference, branch);

	rugged_exception_check(git_branch_name(&branch_name, branch));

	return rb_str_new_utf8(branch_name);
}

static VALUE rb_git_branch__remote_name(VALUE rb_repo, const char *canonical_name)
{
	git_repository *repo;
	git_buf remote_name = { NULL };
	int error;
	VALUE result = Qnil;

	Data_Get_Struct(rb_repo, git_repository, repo);

	if ((error = git_branch_remote_name(&remote_name, repo, canonical_name)) == GIT_OK)
		result = rb_enc_str_new(remote_name.ptr, remote_name.size, rb_utf8_encoding());

	git_buf_dispose(&remote_name);
	rugged_exception_check(error);

	return result;
}

/*
 *  call-seq:
 *    branch.remote_name -> string
 *
 *  Get the name of the remote the branch belongs to.
 *
 *  If +branch+ is a remote branch, the name of the remote it belongs to is
 *  returned. If +branch+ is a tracking branch, the name of the remote
 *  of the tracked branch is returned.
 *
 *  Otherwise, +nil+ is returned.
 */
static VALUE rb_git_branch_remote_name(VALUE self)
{
	git_reference *branch, *remote_ref;
	int error = 0;

	Data_Get_Struct(self, git_reference, branch);

	if (git_reference_is_remote(branch)) {
		remote_ref = branch;
	} else {
		error = git_branch_upstream(&remote_ref, branch);

		if (error == GIT_ENOTFOUND)
			return Qnil;

		rugged_exception_check(error);
	}

	return rb_git_branch__remote_name(
			rugged_owner(self),
			git_reference_name(remote_ref));
}

/*
 *  call-seq:
 *    branch.upstream -> branch
 *
 *  Returns the remote tracking branch, or +nil+ if the branch is
 *  remote or has no tracking branch.
 */
static VALUE rb_git_branch_upstream(VALUE self)
{
	git_reference *branch, *upstream_branch;
	int error;

	Data_Get_Struct(self, git_reference, branch);

	if (git_reference_is_remote(branch))
		return Qnil;

	error = git_branch_upstream(&upstream_branch, branch);

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	return rugged_branch_new(rugged_owner(self), upstream_branch);
}

/*
 *  call-seq:
 *    branch.upstream = branch
 *
 *  Set the upstream configuration for a given local branch.
 *
 *  Takes a local or remote Rugged::Branch instance or a Rugged::Reference
 *  pointing to a branch.
 */
static VALUE rb_git_branch_set_upstream(VALUE self, VALUE rb_branch)
{
	git_reference *branch, *target_branch;
	const char *target_branch_name;

	Data_Get_Struct(self, git_reference, branch);
	if (!NIL_P(rb_branch)) {
		if (!rb_obj_is_kind_of(rb_branch, rb_cRuggedReference))
			rb_raise(rb_eTypeError, "Expecting a Rugged::Reference instance");

		Data_Get_Struct(rb_branch, git_reference, target_branch);

		rugged_exception_check(
			git_branch_name(&target_branch_name, target_branch)
		);
	} else {
		target_branch_name = NULL;
	}

	rugged_exception_check(
		git_branch_set_upstream(branch, target_branch_name)
	);

	return rb_branch;
}

void Init_rugged_branch(void)
{
	rb_cRuggedBranch = rb_define_class_under(rb_mRugged, "Branch", rb_cRuggedReference);

	rb_define_method(rb_cRuggedBranch, "head?", rb_git_branch_head_p, 0);
	rb_define_method(rb_cRuggedBranch, "name", rb_git_branch_name, 0);
	rb_define_method(rb_cRuggedBranch, "remote_name", rb_git_branch_remote_name, 0);
	rb_define_method(rb_cRuggedBranch, "upstream", rb_git_branch_upstream, 0);
	rb_define_method(rb_cRuggedBranch, "upstream=", rb_git_branch_set_upstream, 1);
}
