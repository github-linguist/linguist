/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedIndex;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedCommit;
extern VALUE rb_cRuggedReference;

VALUE rb_cRuggedRebase;

extern const rb_data_type_t rugged_object_type;

static VALUE rebase_operation_type(git_rebase_operation *operation);

static void parse_rebase_options(git_rebase_options *ret, VALUE rb_options)
{
	VALUE val;

	if (NIL_P(rb_options))
		return;

	val = rb_hash_aref(rb_options, CSTR2SYM("quiet"));
	ret->quiet = RTEST(val);

	val = rb_hash_aref(rb_options, CSTR2SYM("inmemory"));
	ret->inmemory = RTEST(val);

	val = rb_hash_aref(rb_options, CSTR2SYM("rewrite_notes_ref"));
	if (!NIL_P(val)) {
		Check_Type(val, T_STRING);
		ret->rewrite_notes_ref = StringValueCStr(val);
	}

	rugged_parse_checkout_options(&ret->checkout_options, rb_options);
	rugged_parse_merge_options(&ret->merge_options, rb_options);
}

void rb_git_rebase__free(git_rebase *rebase)
{
	git_rebase_free(rebase);
}

VALUE rugged_rebase_new(VALUE klass, VALUE owner, git_rebase *rebase)
{
	VALUE rb_rebase = Data_Wrap_Struct(klass, NULL, &rb_git_rebase__free, rebase);
	rugged_set_owner(rb_rebase, owner);
	return rb_rebase;
}

struct get_annotated_commit_args {
	git_annotated_commit **annotated_commit;
	VALUE rb_repo;
	VALUE rb_value;
};

static void get_annotated_commit(git_annotated_commit **annotated_commit, VALUE rb_repo, VALUE rb_value)
{
	git_repository *repo;
	int error;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if (rb_obj_is_kind_of(rb_value, rb_cRuggedCommit)) {
		const git_commit * commit;
		const git_oid * oid;

		TypedData_Get_Struct(rb_value, git_commit, &rugged_object_type, commit);

		oid = git_commit_id(commit);
		error = git_annotated_commit_lookup(annotated_commit, repo, oid);
	} else if (rb_obj_is_kind_of(rb_value, rb_cRuggedReference)) {
		const git_reference * ref;

		Data_Get_Struct(rb_value, git_reference, ref);

		error = git_annotated_commit_from_ref(annotated_commit, repo, ref);
	} else if (TYPE(rb_value) == T_STRING) {
		error = git_annotated_commit_from_revspec(annotated_commit, repo, StringValueCStr(rb_value));
	} else {
		rb_raise(rb_eTypeError, "Expecting a Rugged::Reference, Rugged::Commit or String instance");
	}

	rugged_exception_check(error);
}

static void get_annotated_commit_wrapper(struct get_annotated_commit_args *args)
{
	get_annotated_commit(args->annotated_commit, args->rb_repo, args->rb_value);
}

static int rugged_get_annotated_commit(
	git_annotated_commit ** annotated_commit, VALUE rb_repo, VALUE rb_value)
{
	struct get_annotated_commit_args args;
	int exception;

	args.annotated_commit = annotated_commit;
	args.rb_repo = rb_repo;
	args.rb_value = rb_value;

	rb_protect((VALUE (*)(VALUE))get_annotated_commit_wrapper, (VALUE)&args, &exception);

	return exception;
}

/*
 *  call-seq:
 *    Rebase.new(repo, branch, upstream[, onto][, options]) -> new_rebase
 *
 *  Initialize a new rebase operation. This will put +repo+ in a
 *  rebase state.
 *
 *  +branch+ is the branch to be rebased, and +upstream+ is the branch
 *  which is to be the new base. If +onto+ is specified, this will be
 *  the one base, and +upstream+ is used to determine which commits
 *  from +branch+ to use for the rebase.
 *
 *  You can pass merge and checkout options for the options hash, plus
 *  a few rebase-specific ones:
 *
 *  :quiet ::
 *    If true, a flag will be set to ask tools to activate quiet
 *    mode. This does not do anything for libgit2/rugged but can be
 *    used to interact with other implementations.
 *
 *  :inmemory ::
 *    Do not put the repository in a rebase state but perform all the
 *    operations in-memory. In case of conflicts, the rebase operation
 *    Hash returned by #next will contain the index which can be used to
 *    resolve conflicts.
 *
 *  :rewrite_notes_ref ::
 *    Name of the notes reference used to rewrite notes for rebased
 *    commits when finishing the rebase. If +nil+, it will be taken
 *    from the configuration.
 */
static VALUE rb_git_rebase_new(int argc, VALUE* argv, VALUE klass)
{
	int error = 0, exception = 0;
	git_rebase *rebase = NULL;
	git_repository *repo;
	git_annotated_commit *branch = NULL, *upstream = NULL, *onto = NULL;
	VALUE rb_repo, rb_branch, rb_upstream, rb_onto, rb_options;
	git_rebase_options options = GIT_REBASE_OPTIONS_INIT;

	rb_scan_args(argc, argv, "31:", &rb_repo, &rb_branch, &rb_upstream, &rb_onto, &rb_options);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if ((exception = rugged_get_annotated_commit(&branch, rb_repo, rb_branch)))
		goto cleanup;

	if ((exception = rugged_get_annotated_commit(&upstream, rb_repo, rb_upstream)))
		goto cleanup;

	if (!NIL_P(rb_onto)) {
		if ((exception = rugged_get_annotated_commit(&onto, rb_repo, rb_onto)))
			goto cleanup;
	}

	parse_rebase_options(&options, rb_options);

	error = git_rebase_init(&rebase, repo, branch, upstream, onto, &options);

cleanup:
	git_annotated_commit_free(branch);
	git_annotated_commit_free(upstream);
	git_annotated_commit_free(onto);

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);

	return rugged_rebase_new(klass, rb_repo, rebase);
}

/*
 *  call-seq:
 *    Rebase.next() -> operation or nil
 *
 *  Perform the next step in the rebase. The returned operation is a
 *  Hash with its details or nil if there are no more operations to
 *  perform. The Hash contains some of the following entries:
 *
 *  :type ::
 *    The type of operation being done. Can be one of +:pick+,
 *    +:reword+, +:edit+, +:squash+, +:fixup+ or +:exec+.
 *
 *  :id ::
 *    The id of the commit being cherry-picked. Exists for all but
 *    +:exec+ operations.
 *
 *  :exec ::
 *    If the operatin is +:exec+ this is what the user asked to be
 *    executed.
 */
static VALUE rb_git_rebase_next(VALUE self)
{
	int error;
	git_rebase *rebase;
	git_rebase_operation *operation;
	VALUE hash, val;

	Data_Get_Struct(self, git_rebase, rebase);
	error = git_rebase_next(&operation, rebase);
	if (error == GIT_ITEROVER)
		return Qnil;

	rugged_exception_check(error);

	/* Create the operation hash out of the relevant details */
	hash = rb_hash_new();

	val = rebase_operation_type(operation);
	rb_hash_aset(hash, CSTR2SYM("type"), val);

	if (operation->type != GIT_REBASE_OPERATION_EXEC) {
		val = rugged_create_oid(&operation->id);
		rb_hash_aset(hash, CSTR2SYM("id"), val);
	}

	if (operation->exec) {
		val = rb_str_new_utf8(operation->exec);
		rb_hash_aset(hash, CSTR2SYM("exec"), val);
	}

	return hash;
}
/*
 *  call-seq:
 *    rebase.inmemory_index -> index
 *
 *  Gets the index produced by the last operation, which is the result
 *  of +next+ and which will be committed by the next invocation of
 *  +commit+.  This is useful for resolving conflicts in an in-memory
 *  rebase before committing them.
 *
 *  This is only applicable for in-memory rebases; for rebases within
 *  a working directory, the changes were applied to the repository's
 *  index.
 */
static VALUE rb_git_rebase_inmemory_index(VALUE self)
{
	git_rebase *rebase;
	git_index *index;

	Data_Get_Struct(self, git_rebase, rebase);
	rugged_exception_check(git_rebase_inmemory_index(&index, rebase));

	return rugged_index_new(rb_cRuggedIndex, self, index);
}

/*
 *  call-seq:
 *    rebase.commit(author: nil, committer: committer, message: nil) -> oid or nil
 *
 *  Commit the current patch. Any conflicts must have been resolved.
 *
 *  If +author+ is +nil+, the existing author for the commit will be
 *  used. If +message+ is +nil+, the existing message will be used.
 *
 *  Returns a string containing the oid of the newly created commit,
 *  or +nil+ if there are no changes to be committed.
 */
static VALUE rb_git_rebase_commit(int argc, VALUE *argv, VALUE self)
{
	int error;
	git_oid id;
	git_rebase *rebase;
	git_signature *author = NULL, *committer;
	const char *message = NULL;
	VALUE rb_options, rb_author, rb_committer, rb_message;

	Data_Get_Struct(self, git_rebase, rebase);
	rb_scan_args(argc, argv, ":", &rb_options);

	Check_Type(rb_options, T_HASH);

	rb_author = rb_hash_aref(rb_options, CSTR2SYM("author"));
	rb_committer = rb_hash_aref(rb_options, CSTR2SYM("committer"));
	rb_message = rb_hash_aref(rb_options, CSTR2SYM("message"));

	if (!NIL_P(rb_message)) {
		Check_Type(rb_message, T_STRING);
		message = StringValueCStr(rb_message);
	}

	if (NIL_P(rb_committer))
		rb_raise(rb_eArgError, "Expected non-nil committer");
	else
		committer = rugged_signature_get(rb_committer, NULL);

	if (!NIL_P(rb_author))
		author = rugged_signature_get(rb_author, NULL);

	error = git_rebase_commit(&id, rebase, author, committer, NULL, message);
	git_signature_free(author);
	git_signature_free(committer);

	if (error == GIT_EAPPLIED) {
		giterr_clear();
		return Qnil;
	}

	rugged_exception_check(error);

	return rugged_create_oid(&id);
}

/*
 *  call-seq:
 *    rebase.abort -> nil
 *
 *  Abort the rebase currently in process, resetting the repository
 *  and working directory to their state before the rebase began.
 */
static VALUE rb_git_rebase_abort(VALUE self)
{
	git_rebase *rebase;

	Data_Get_Struct(self, git_rebase, rebase);
	rugged_exception_check(git_rebase_abort(rebase));

	return Qnil;
}

/*
 *  call-seq:
 *    rebase.finish -> nil
 *
 *  Finish the rebase currently in progress once all patches have been
 *  applied.
 */
static VALUE rb_git_rebase_finish(VALUE self, VALUE rb_sig)
{
	git_rebase *rebase;
	git_signature *sig;
	int error;

	Data_Get_Struct(self, git_rebase, rebase);
	sig = rugged_signature_get(rb_sig, NULL);
	error = git_rebase_finish(rebase, sig);
	git_signature_free(sig);

	rugged_exception_check(error);

	return Qnil;
}

static VALUE rebase_operation_type(git_rebase_operation *operation)
{
	VALUE rb_type;

	switch (operation->type) {
	case GIT_REBASE_OPERATION_PICK:
		rb_type = CSTR2SYM("pick");
		break;
	case GIT_REBASE_OPERATION_REWORD:
		rb_type = CSTR2SYM("reword");
		break;
	case GIT_REBASE_OPERATION_EDIT:
		rb_type = CSTR2SYM("edit");
		break;
	case GIT_REBASE_OPERATION_SQUASH:
		rb_type = CSTR2SYM("squash");
		break;
	case GIT_REBASE_OPERATION_FIXUP:
		rb_type = CSTR2SYM("fixup");
		break;
	case GIT_REBASE_OPERATION_EXEC:
		rb_type = CSTR2SYM("exec");
		break;
	default:
		rb_raise(rb_eTypeError, "Invalid rebase operation type found");
		break;
	}

	return rb_type;
}

void Init_rugged_rebase(void)
{
	rb_cRuggedRebase = rb_define_class_under(rb_mRugged, "Rebase", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedRebase);

	rb_define_singleton_method(rb_cRuggedRebase, "new", rb_git_rebase_new, -1);
	rb_define_method(rb_cRuggedRebase, "next",  rb_git_rebase_next,  0);
	rb_define_method(rb_cRuggedRebase, "inmemory_index",  rb_git_rebase_inmemory_index,  0);
	rb_define_method(rb_cRuggedRebase, "commit",  rb_git_rebase_commit,  -1);
	rb_define_method(rb_cRuggedRebase, "abort",  rb_git_rebase_abort,  0);
	rb_define_method(rb_cRuggedRebase, "finish",  rb_git_rebase_finish,  1);
}
