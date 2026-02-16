/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedBranch;

VALUE rb_cRuggedBranchCollection;

static inline VALUE rugged_branch_new(VALUE owner, git_reference *ref)
{
	return rugged_ref_new(rb_cRuggedBranch, owner, ref);
}

// Helper method to normalize branch lookups.
static inline int rugged_branch_lookup(git_reference **branch, git_repository *repo, VALUE rb_name_or_branch)
{
	if (rb_obj_is_kind_of(rb_name_or_branch, rb_cRuggedBranch)) {
		rb_name_or_branch = rb_funcall(rb_name_or_branch, rb_intern("canonical_name"), 0);

		if (TYPE(rb_name_or_branch) != T_STRING)
			rb_raise(rb_eTypeError, "Expected #canonical_name to return a String");

		return git_reference_lookup(branch, repo, StringValueCStr(rb_name_or_branch));
	} else if (TYPE(rb_name_or_branch) == T_STRING) {
		char *branch_name = StringValueCStr(rb_name_or_branch), *ref_name;
		int error;

		if (strncmp(branch_name, "refs/heads/", strlen("refs/heads/")) == 0 ||
		    strncmp(branch_name, "refs/remotes/", strlen("refs/remotes/")) == 0)
			return git_reference_lookup(branch, repo, branch_name);

		if ((error = git_branch_lookup(branch, repo, branch_name, GIT_BRANCH_LOCAL)) == GIT_OK ||
		    error != GIT_ENOTFOUND)
			return error;

		if ((error = git_branch_lookup(branch, repo, branch_name, GIT_BRANCH_REMOTE)) == GIT_OK ||
		    error != GIT_ENOTFOUND)
			return error;

		ref_name = xmalloc((strlen(branch_name) + strlen("refs/") + 1)  * sizeof(char));
		strcpy(ref_name, "refs/");
		strcat(ref_name, branch_name);

		error = git_reference_lookup(branch, repo, ref_name);
		xfree(ref_name);

		return error;
	} else {
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Branch instance");
	}
}

/*
 *  call-seq:
 *    BranchCollection.new(repo) -> refs
 *
 *  Creates and returns a new collection of branches for the given +repo+.
 */
static VALUE rb_git_branch_collection_initialize(VALUE self, VALUE repo)
{
	rugged_set_owner(self, repo);
	return self;
}

static git_branch_t parse_branch_type(VALUE rb_filter)
{
	ID id_filter;

	Check_Type(rb_filter, T_SYMBOL);
	id_filter = SYM2ID(rb_filter);

	if (id_filter == rb_intern("local")) {
		return GIT_BRANCH_LOCAL;
	} else if (id_filter == rb_intern("remote")) {
		return GIT_BRANCH_REMOTE;
	} else {
		rb_raise(rb_eTypeError,
			"Invalid branch filter. Expected `:remote`, `:local` or `nil`");
	}
}

/*
 *  call-seq:
 *    branches.create(name, target, options = {}) -> branch
 *
 *  Create a new branch with the given +name+, pointing to the +target+.
 *
 *  +name+ needs to be a branch name, not an absolute reference path
 *  (e.g. +development+ instead of +refs/heads/development+).
 *
 *  +target+ needs to be an existing commit in the given repository.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :force ::
 *    Overwrites the branch with the given +name+, if it already exists,
 *    instead of raising an exception.
 *
 *  If a branch with the given +name+ already exists and +:force+ is not +true+,
 *  an exception will be raised.
 *
 *  Returns a Rugged::Branch for the newly created branch.
 */
static VALUE rb_git_branch_collection_create(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_repo = rugged_owner(self), rb_name, rb_target, rb_options;
	git_repository *repo;
	git_reference *branch;
	git_commit *target;
	int error, force = 0;

	rb_scan_args(argc, argv, "20:", &rb_name, &rb_target, &rb_options);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_name, T_STRING);
	Check_Type(rb_target, T_STRING);

	if (!NIL_P(rb_options)) {
		force = RTEST(rb_hash_aref(rb_options, CSTR2SYM("force")));
	}

	target = (git_commit *)rugged_object_get(repo, rb_target, GIT_OBJ_COMMIT);

	error = git_branch_create(&branch, repo, StringValueCStr(rb_name), target, force);

	git_commit_free(target);

	rugged_exception_check(error);

	return rugged_branch_new(rb_repo, branch);
}

/*
 *  call-seq:
 *    branches[name] -> branch
 *
 *  Return the branch with the given +name+.
 *
 *  Branches can be looked up by their relative (+development+) or absolute
 *  (+refs/heads/development+) branch name.
 *
 *  If a local branch and a remote branch both share the same short name
 *  (e.g. +refs/heads/origin/master+ and +refs/remotes/origin/master+),
 *  passing +origin/master+ as the +name+ will return the local branch.
 *  You can explicitly request the local branch by passing
 *  +heads/origin/master+, or the remote branch through +remotes/origin/master+.
 *
 *  Returns the looked up branch, or +nil+ if the branch doesn't exist.
 */
static VALUE rb_git_branch_collection_aref(VALUE self, VALUE rb_name) {
	git_reference *branch;
	git_repository *repo;

	VALUE rb_repo = rugged_owner(self);
	int error;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_name, T_STRING);

	error = rugged_branch_lookup(&branch, repo, rb_name);
	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);
	return rugged_branch_new(rb_repo, branch);
}

static VALUE each_branch(int argc, VALUE *argv, VALUE self, int branch_names_only)
{
	VALUE rb_repo, rb_filter;
	git_repository *repo;
	git_branch_iterator *iter;
	int error, exception = 0;
	git_branch_t filter = (GIT_BRANCH_LOCAL | GIT_BRANCH_REMOTE), branch_type;

	RETURN_ENUMERATOR(self, argc, argv);
	rb_scan_args(argc, argv, "01", &rb_filter);

	rb_repo = rugged_owner(self);
	rugged_check_repo(rb_repo);

	if (!NIL_P(rb_filter))
		filter = parse_branch_type(rb_filter);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_branch_iterator_new(&iter, repo, filter);
	rugged_exception_check(error);

	if (branch_names_only) {
		git_reference *branch;
		while (!exception && (error = git_branch_next(&branch, &branch_type, iter)) == GIT_OK) {
			rb_protect(rb_yield, rb_str_new_utf8(git_reference_shorthand(branch)), &exception);
		}
	} else {
		git_reference *branch;
		while (!exception && (error = git_branch_next(&branch, &branch_type, iter)) == GIT_OK) {
			rb_protect(rb_yield, rugged_branch_new(rb_repo, branch), &exception);
		}
	}

	git_branch_iterator_free(iter);

	if (exception)
		rb_jump_tag(exception);

	if (error != GIT_ITEROVER)
		rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    branches.each([filter]) { |branch| block }
 *    branches.each([filter]) -> enumerator
 *
 *  Iterate through the branches in the collection. Iteration can be
 *  optionally filtered to yield only +:local+ or +:remote+ branches.
 *
 *  The given block will be called once with a +Rugged::Branch+ object
 *  for each branch in the repository. If no block is given, an enumerator
 *  will be returned.
 */
static VALUE rb_git_branch_collection_each(int argc, VALUE *argv, VALUE self)
{
	return each_branch(argc, argv, self, 0);
}

/*
 *  call-seq:
 *    branches.each_name([filter]) { |branch_name| block }
 *    branches.each_name([filter]) -> enumerator
 *
 *  Iterate through the names of the branches in the collection. Iteration can be
 *  optionally filtered to yield only +:local+ or +:remote+ branches.
 *
 *  The given block will be called once with the name of each branch as a +String+.
 *  If no block is given, an enumerator will be returned.
 */
static VALUE rb_git_branch_collection_each_name(int argc, VALUE *argv, VALUE self)
{
	return each_branch(argc, argv, self, 1);
}

/*
 *  call-seq:
 *    branches.delete(branch) -> nil
 *    branches.delete(name) -> nil
 *
 *  Delete the specified branch.
 *
 *  If a Rugged::Branch object was passed, the object will become
 *  invalidated and won't be able to be used for any other operations.
 */
static VALUE rb_git_branch_collection_delete(VALUE self, VALUE rb_name_or_branch)
{
	git_reference *branch;
	git_repository *repo;

	VALUE rb_repo = rugged_owner(self);
	int error;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = rugged_branch_lookup(&branch, repo, rb_name_or_branch);
	rugged_exception_check(error);

	error = git_branch_delete(branch);
	git_reference_free(branch);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    branch.move(old_name, new_name, options = {}) -> new_branch
 *    branch.move(branch, new_name, options = {}) -> new_branch
 *    branch.rename(old_name, new_name, options = {}) -> new_branch
 *    branch.rename(branch, new_name, options = {}) -> new_branch
 *
 *  Rename a branch to +new_name+.
 *
 *  +new_name+ needs to be a branch name, not an absolute reference path
 *  (e.g. +development+ instead of +refs/heads/development+).
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :force ::
 *    Overwrites the branch with the given +name+, if it already exists,
 *    instead of raising an exception.
 *
 *  If a branch with the given +new_name+ already exists and +:force+ is not +true+,
 *  an exception will be raised.
 *
 *  A new Rugged::Branch object for the renamed branch will be returned.
 *
 */
static VALUE rb_git_branch_collection_move(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_repo = rugged_owner(self), rb_name_or_branch, rb_new_branch_name, rb_options;
	git_reference *old_branch = NULL, *new_branch = NULL;
	git_repository *repo;
	int error, force = 0;

	rb_scan_args(argc, argv, "20:", &rb_name_or_branch, &rb_new_branch_name, &rb_options);
	Check_Type(rb_new_branch_name, T_STRING);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = rugged_branch_lookup(&old_branch, repo, rb_name_or_branch);
	rugged_exception_check(error);

	if (!NIL_P(rb_options)) {
		force = RTEST(rb_hash_aref(rb_options, CSTR2SYM("force")));
	}

	error = git_branch_move(&new_branch, old_branch, StringValueCStr(rb_new_branch_name), force);

	git_reference_free(old_branch);

	rugged_exception_check(error);

	return rugged_branch_new(rugged_owner(self), new_branch);
}

/*
 *  call-seq:
 *    branches.exist?(name) -> true or false
 *    branches.exists?(name) -> true or false
 *
 *  Check if a branch exists with the given +name+.
 */
static VALUE rb_git_branch_collection_exist_p(VALUE self, VALUE rb_name)
{
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;
	git_reference *branch;
	int error;

	Check_Type(rb_name, T_STRING);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = rugged_branch_lookup(&branch, repo, rb_name);
	git_reference_free(branch);

	if (error == GIT_ENOTFOUND)
		return Qfalse;
	else
		rugged_exception_check(error);

	return Qtrue;
}

void Init_rugged_branch_collection(void)
{
	rb_cRuggedBranchCollection = rb_define_class_under(rb_mRugged, "BranchCollection", rb_cObject);
	rb_include_module(rb_cRuggedBranchCollection, rb_mEnumerable);

	rb_define_method(rb_cRuggedBranchCollection, "initialize", rb_git_branch_collection_initialize, 1);

	rb_define_method(rb_cRuggedBranchCollection, "[]",         rb_git_branch_collection_aref, 1);
	rb_define_method(rb_cRuggedBranchCollection, "create",     rb_git_branch_collection_create, -1);

	rb_define_method(rb_cRuggedBranchCollection, "each",       rb_git_branch_collection_each, -1);
	rb_define_method(rb_cRuggedBranchCollection, "each_name",  rb_git_branch_collection_each_name, -1);

	rb_define_method(rb_cRuggedBranchCollection, "exist?",     rb_git_branch_collection_exist_p, 1);
	rb_define_method(rb_cRuggedBranchCollection, "exists?",    rb_git_branch_collection_exist_p, 1);

	rb_define_method(rb_cRuggedBranchCollection, "move",       rb_git_branch_collection_move, -1);
	rb_define_method(rb_cRuggedBranchCollection, "rename",     rb_git_branch_collection_move, -1);
	rb_define_method(rb_cRuggedBranchCollection, "delete",     rb_git_branch_collection_delete, 1);
}
