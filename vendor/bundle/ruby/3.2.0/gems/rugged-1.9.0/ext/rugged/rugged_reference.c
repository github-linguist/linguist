/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedRepo;

VALUE rb_cRuggedReference;

void rb_git_ref__free(git_reference *ref)
{
	git_reference_free(ref);
}

VALUE rugged_ref_new(VALUE klass, VALUE owner, git_reference *ref)
{
	VALUE rb_ref = Data_Wrap_Struct(klass, NULL, &rb_git_ref__free, ref);
	rugged_set_owner(rb_ref, owner);
	return rb_ref;
}


const char * rugged_refname_from_string_or_ref(VALUE rb_name_or_ref)
{
	if (rb_obj_is_kind_of(rb_name_or_ref, rb_cRuggedReference))
		rb_name_or_ref = rb_funcall(rb_name_or_ref, rb_intern("canonical_name"), 0);

	if (TYPE(rb_name_or_ref) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Reference instance");

	return StringValueCStr(rb_name_or_ref);
}

/*
 *  call-seq:
 *    Reference.valid_name?(ref_name) -> true or false
 *
 *  Check if a reference name is well-formed.
 *
 *  Valid reference names must follow one of two patterns:
 *
 *  1. Top-level names must contain only capital letters and underscores,
 *     and must begin and end with a letter. (e.g. "HEAD", "ORIG_HEAD").
 *  2. Names prefixed with "refs/" can be almost anything.  You must avoid
 *     the characters '~', '^', ':', '\\', '?', '[', and '*', and the
 *     sequences ".." and "@{" which have special meaning to revparse.
 *
 *  Returns true if the reference name is valid, false if not.
 */
static VALUE rb_git_ref_valid_name(VALUE klass, VALUE rb_name)
{
	Check_Type(rb_name, T_STRING);
	return git_reference_is_valid_name(StringValueCStr(rb_name)) == 1 ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    ref.peel -> oid
 *
 *  Peels tag objects to the sha that they point at. Replicates
 *  +git show-ref --dereference+.
 */
static VALUE rb_git_ref_peel(VALUE self)
{
	/* Leave room for \0 */
	git_reference *ref;
	git_object *object;
	char oid[GIT_OID_HEXSZ + 1];
	int error;

	Data_Get_Struct(self, git_reference, ref);

	error = git_reference_peel(&object, ref, GIT_OBJ_ANY);
	if (error == GIT_ENOTFOUND)
		return Qnil;
	else
		rugged_exception_check(error);

	if (git_reference_type(ref) == GIT_REF_OID &&
			!git_oid_cmp(git_object_id(object), git_reference_target(ref))) {
		git_object_free(object);
		return Qnil;
	} else {
		git_oid_tostr(oid, sizeof(oid), git_object_id(object));
		git_object_free(object);
		return rb_str_new_utf8(oid);
	}
}

/*
 *  call-seq:
 *    reference.target_id -> id
 *    reference.target_id -> ref_name
 *
 *  Return the target of +reference+.
 *
 *  If +reference+ is a symbolic reference, it returns the target
 *  reference object.
 *
 *  If +reference+ is a direct reference, it returns the target object.
 *
 *    ref1.type #=> :symbolic
 *    ref1.target #=> #<Rugged::Reference ...>
 *
 *    ref2.type #=> :direct
 *    ref2.target #=> #<Rugged::Commit ...>
 */
static VALUE rb_git_ref_target(VALUE self)
{
	git_reference *ref;

	Data_Get_Struct(self, git_reference, ref);

	if (git_reference_type(ref) == GIT_REF_OID) {
		git_object *target;

		rugged_exception_check(
			git_object_lookup(&target, git_reference_owner(ref), git_reference_target(ref), GIT_OBJ_ANY)
		);
		return rugged_object_new(rugged_owner(self), target);
	} else {
		git_reference *target;

		rugged_exception_check(
			git_reference_lookup(&target, git_reference_owner(ref), git_reference_symbolic_target(ref))
		);

		return rugged_ref_new(rb_cRuggedReference, rugged_owner(self), target);
	}
}

/*
 *  call-seq:
 *    reference.target_id -> id
 *    reference.target_id -> ref_name
 *
 *  Return the target identifier of +reference+.
 *
 *  If +reference+ is a symbolic reference, it returns the canonical
 *  name of the target reference.
 *
 *  If +reference+ is a direct reference, it returns the sha id of the target.
 *
 *    ref1.type #=> :symbolic
 *    ref1.target_id #=> "refs/heads/master"
 *
 *    ref2.type #=> :direct
 *    ref2.target_id #=> "de5ba987198bcf2518885f0fc1350e5172cded78"
 */
static VALUE rb_git_ref_target_id(VALUE self)
{
	git_reference *ref;
	Data_Get_Struct(self, git_reference, ref);

	if (git_reference_type(ref) == GIT_REF_OID) {
		return rugged_create_oid(git_reference_target(ref));
	} else {
		return rb_str_new_utf8(git_reference_symbolic_target(ref));
	}
}

/*
 *  call-seq:
 *    reference.type -> :symbolic or :direct
 *
 *  Return whether the reference is +:symbolic+ or +:direct+
 */
static VALUE rb_git_ref_type(VALUE self)
{
	git_reference *ref;
	Data_Get_Struct(self, git_reference, ref);

	switch (git_reference_type(ref)) {
		case GIT_REF_OID:
			return CSTR2SYM("direct");
		case GIT_REF_SYMBOLIC:
			return CSTR2SYM("symbolic");
		default:
			return Qnil;
	}
}

/*
 *  call-seq:
 *    reference.name -> name
 *    reference.canonical_name -> name
 *
 *  Returns the fully qualified name of the reference.
 *
 *  +name+ gets overwritten in subclasess like Rugged::Branch or Rugged::Tag
 *  to return "nicer" names for presentational purposes, while +canonical_name+
 *  is always supposed to return the fully qualified reference path.
 *
 *    reference.name #=> 'HEAD'
 */
static VALUE rb_git_ref_name(VALUE self)
{
	git_reference *ref;
	Data_Get_Struct(self, git_reference, ref);
	return rb_str_new_utf8(git_reference_name(ref));
}

/*
 *  call-seq:
 *    reference.resolve -> peeled_ref
 *
 *  Peel a symbolic reference to its target reference.
 *
 *    r1.type #=> :symbolic
 *    r1.name #=> 'HEAD'
 *    r1.target #=> 'refs/heads/master'
 *
 *    r2 = r1.resolve #=> #<Rugged::Reference:0x401b3948>
 *    r2.target #=> '9d09060c850defbc7711d08b57def0d14e742f4e'
 */
static VALUE rb_git_ref_resolve(VALUE self)
{
	git_reference *ref;
	git_reference *resolved;
	int error;

	Data_Get_Struct(self, git_reference, ref);

	error = git_reference_resolve(&resolved, ref);
	rugged_exception_check(error);

	return rugged_ref_new(rb_cRuggedReference, rugged_owner(self), resolved);
}

static VALUE reflog_entry_new(const git_reflog_entry *entry)
{
	VALUE rb_entry = rb_hash_new();
	const char *message;

	rb_hash_aset(rb_entry,
		CSTR2SYM("id_old"),
		rugged_create_oid(git_reflog_entry_id_old(entry))
	);

	rb_hash_aset(rb_entry,
		CSTR2SYM("id_new"),
		rugged_create_oid(git_reflog_entry_id_new(entry))
	);

	rb_hash_aset(rb_entry,
		CSTR2SYM("committer"),
		rugged_signature_new(git_reflog_entry_committer(entry), NULL)
	);

	if ((message = git_reflog_entry_message(entry)) != NULL) {
		rb_hash_aset(rb_entry, CSTR2SYM("message"), rb_str_new_utf8(message));
	}

	return rb_entry;
}

/*
 *  call-seq:
 *    reference.log -> [reflog_entry, ...]
 *
 *  Return an array with the log of all modifications to this reference
 *
 *  Each +reflog_entry+ is a hash with the following keys:
 *
 *  - +:id_old+: previous OID before the change
 *  - +:id_new+: OID after the change
 *  - +:committer+: author of the change
 *  - +:message+: message for the change
 *
 *  Example:
 *
 *    reference.log #=> [
 *    # {
 *    #  :id_old => nil,
 *    #  :id_new => '9d09060c850defbc7711d08b57def0d14e742f4e',
 *    #  :committer => {:name => 'Vicent Marti', :email => {'vicent@github.com'}},
 *    #  :message => 'created reference'
 *    # }, ... ]
 */
static VALUE rb_git_reflog(VALUE self)
{
	git_reflog *reflog;
	git_reference *ref;
	int error;
	VALUE rb_log;
	size_t i, ref_count;

	Data_Get_Struct(self, git_reference, ref);

	error = git_reflog_read(&reflog, git_reference_owner(ref), git_reference_name(ref));
	rugged_exception_check(error);

	ref_count = git_reflog_entrycount(reflog);
	rb_log = rb_ary_new2(ref_count);

	for (i = 0; i < ref_count; ++i) {
		const git_reflog_entry *entry =
			git_reflog_entry_byindex(reflog, ref_count - i - 1);

		rb_ary_push(rb_log, reflog_entry_new(entry));
	}

	git_reflog_free(reflog);
	return rb_log;
}

/*
 *  call-seq:
 *    reference.log? -> true or false
 *
 *  Return +true+ if the reference has a reflog, +false+ otherwise.
 */
static VALUE rb_git_has_reflog(VALUE self)
{
	git_reference *ref;
	git_repository *repo;

	Data_Get_Struct(self, git_reference, ref);
	repo = git_reference_owner(ref);

	return git_reference_has_log(repo, git_reference_name(ref)) ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    reference.branch? -> true or false
 *
 *  Returns +true+ if +reference+ is a local branch, false otherwise.
 */
static VALUE rb_git_ref_is_branch(VALUE self)
{
	git_reference *ref;
	Data_Get_Struct(self, git_reference, ref);
	return git_reference_is_branch(ref) ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    reference.remote? -> true or false
 *
 *  Returns +true+ if +reference+ is a remote branch, false otherwise.
 */
static VALUE rb_git_ref_is_remote(VALUE self)
{
	git_reference *ref;
	Data_Get_Struct(self, git_reference, ref);
	return git_reference_is_remote(ref) ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    reference.tag? -> true or false
 *
 *  Returns +true+ if +reference+ is a tag, false otherwise.
 */
static VALUE rb_git_ref_is_tag(VALUE self)
{
	git_reference *ref;
	Data_Get_Struct(self, git_reference, ref);
	return git_reference_is_tag(ref) ? Qtrue : Qfalse;
}

void Init_rugged_reference(void)
{
	rb_cRuggedReference = rb_define_class_under(rb_mRugged, "Reference", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedReference);

	rb_define_singleton_method(rb_cRuggedReference, "valid_name?", rb_git_ref_valid_name, 1);

	rb_define_method(rb_cRuggedReference, "target", rb_git_ref_target, 0);
	rb_define_method(rb_cRuggedReference, "target_id", rb_git_ref_target_id, 0);
	rb_define_method(rb_cRuggedReference, "peel", rb_git_ref_peel, 0);

	rb_define_method(rb_cRuggedReference, "type", rb_git_ref_type, 0);

	rb_define_method(rb_cRuggedReference, "name", rb_git_ref_name, 0);
	rb_define_method(rb_cRuggedReference, "canonical_name", rb_git_ref_name, 0);

	rb_define_method(rb_cRuggedReference, "resolve", rb_git_ref_resolve, 0);

	rb_define_method(rb_cRuggedReference, "branch?", rb_git_ref_is_branch, 0);
	rb_define_method(rb_cRuggedReference, "remote?", rb_git_ref_is_remote, 0);
	rb_define_method(rb_cRuggedReference, "tag?", rb_git_ref_is_tag, 0);

	rb_define_method(rb_cRuggedReference, "log", rb_git_reflog, 0);
	rb_define_method(rb_cRuggedReference, "log?", rb_git_has_reflog, 0);
}
