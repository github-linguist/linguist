/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedReference;

VALUE rb_cRuggedReferenceCollection;

/*
 *  call-seq:
 *    ReferenceCollection.new(repo) -> refs
 *
 *  Creates and returns a new collection of references for the given +repo+.
 */
static VALUE rb_git_reference_collection_initialize(VALUE self, VALUE repo)
{
	rugged_set_owner(self, repo);
	return self;
}

/*
 *  call-seq:
 *    references.create(name, oid, options = {}) -> new_ref
 *    references.create(name, target, options = {}) -> new_ref
 *
 *  Create a symbolic or direct reference on the collection's +repository+ with the given +name+.
 *
 *  If the second argument is a valid OID, the reference will be created as direct.
 *  Otherwise, it will be assumed the target is the name of another reference.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :force ::
 *    Overwrites the reference with the given +name+, if it already exists,
 *    instead of raising an exception.
 *
 *  If a reference with the given +name+ already exists and +:force+ is not +true+,
 *  an exception will be raised.
 */
static VALUE rb_git_reference_collection_create(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_repo = rugged_owner(self), rb_name, rb_target, rb_options;
	git_repository *repo;
	git_reference *ref;
	git_oid oid;
	char *log_message = NULL;
	int error, force = 0;

	rb_scan_args(argc, argv, "20:", &rb_name, &rb_target, &rb_options);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);
	Check_Type(rb_name, T_STRING);
	Check_Type(rb_target, T_STRING);

	if (!NIL_P(rb_options)) {
		VALUE rb_val = rb_hash_aref(rb_options, CSTR2SYM("message"));
		if (!NIL_P(rb_val))
			log_message = StringValueCStr(rb_val);

		force = RTEST(rb_hash_aref(rb_options, CSTR2SYM("force")));
	}

	if (git_oid_fromstr(&oid, StringValueCStr(rb_target)) == GIT_OK) {
		error = git_reference_create(
			&ref, repo, StringValueCStr(rb_name), &oid, force, log_message);
	} else {
		error = git_reference_symbolic_create(
			&ref, repo, StringValueCStr(rb_name), StringValueCStr(rb_target), force, log_message);
	}

	rugged_exception_check(error);

	return rugged_ref_new(rb_cRuggedReference, rb_repo, ref);
}

/*
 *  call-seq:
 *    references[name] -> new_ref
 *
 *  Lookup a reference in the collection with the given +name+.
 *
 *  Returns a new Rugged::Reference object.
 */
static VALUE rb_git_reference_collection_aref(VALUE self, VALUE rb_name) {
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;
	git_reference *ref;
	int error;

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_reference_lookup(&ref, repo, StringValueCStr(rb_name));

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	return rugged_ref_new(rb_cRuggedReference, rb_repo, ref);
}

static VALUE rb_git_reference_collection__each(int argc, VALUE *argv, VALUE self, int only_names)
{
	VALUE rb_glob, rb_repo = rugged_owner(self);
	git_repository *repo;
	git_reference_iterator *iter;
	int error, exception = 0;

	RETURN_ENUMERATOR(self, argc, argv);
	rb_scan_args(argc, argv, "01", &rb_glob);

	rugged_check_repo(rb_repo);

	Data_Get_Struct(rb_repo, git_repository, repo);

	if (!NIL_P(rb_glob)) {
		Check_Type(rb_glob, T_STRING);
		error = git_reference_iterator_glob_new(&iter, repo, StringValueCStr(rb_glob));
	} else {
		error = git_reference_iterator_new(&iter, repo);
	}

	rugged_exception_check(error);

	if (only_names) {
		const char *ref_name;
		while (!exception && (error = git_reference_next_name(&ref_name, iter)) == GIT_OK) {
			rb_protect(rb_yield, rb_str_new_utf8(ref_name), &exception);
		}
	} else {
		git_reference *ref;
		while (!exception && (error = git_reference_next(&ref, iter)) == GIT_OK) {
			rb_protect(rb_yield, rugged_ref_new(rb_cRuggedReference, rb_repo, ref), &exception);
		}
	}

	git_reference_iterator_free(iter);

	if (exception)
		rb_jump_tag(exception);

	if (error != GIT_ITEROVER)
		rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    references.each(glob = nil) { |ref| block } -> nil
 *    references.each(glob = nil) -> enumerator
 *
 *  Iterate through all the references in the collection's +repository+. Iteration
 *  can be optionally filtered to the ones matching the given
 *  +glob+, a standard Unix filename glob.
 *
 *  The given block will be called once with a Rugged::Reference
 *  instance for each reference.
 *
 *  If no block is given, an enumerator will be returned.
 */
static VALUE rb_git_reference_collection_each(int argc, VALUE *argv, VALUE self)
{
	return rb_git_reference_collection__each(argc, argv, self, 0);
}

/*
 *  call-seq:
 *    references.each_name(glob = nil) { |ref_name| block } -> nil
 *    references.each_name(glob = nil) -> enumerator
 *
 *  Iterate through all the reference names in the collection's +repository+. Iteration
 *  can be optionally filtered to the ones matching the given
 *  +glob+, a standard Unix filename glob.
 *
 *  The given block will be called once with the name of each reference.
 *
 *  If no block is given, an enumerator will be returned.
 */
static VALUE rb_git_reference_collection_each_name(int argc, VALUE *argv, VALUE self)
{
	return rb_git_reference_collection__each(argc, argv, self, 1);
}

/*
 *  call-seq:
 *    references.exist?(name) -> true or false
 *    references.exists?(name) -> true or false
 *
 *  Check if a given reference exists with the given +name+.
 */
static VALUE rb_git_reference_collection_exist_p(VALUE self, VALUE rb_name_or_ref)
{
	VALUE rb_repo = rugged_owner(self);
	git_repository *repo;
	git_reference *ref;
	int error;

	if (rb_obj_is_kind_of(rb_name_or_ref, rb_cRuggedReference))
		rb_name_or_ref = rb_funcall(rb_name_or_ref, rb_intern("canonical_name"), 0);

	if (TYPE(rb_name_or_ref) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Reference instance");

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_reference_lookup(&ref, repo, StringValueCStr(rb_name_or_ref));
	git_reference_free(ref);

	if (error == GIT_ENOTFOUND)
		return Qfalse;
	else
		rugged_exception_check(error);

	return Qtrue;
}

/*
 *  call-seq:
 *    references.rename(old_name, new_name, options = {}) -> new_ref
 *    references.rename(ref, new_name, options = {}) -> new_ref
 *
 *  Change the name of a reference. If +force+ is +true+, any previously
 *  existing references will be overwritten when renaming.
 *
 *  Return a new reference object with the new object
 *
 *    reference.name #=> 'refs/heads/master'
 *    new_ref = references.rename(ref, 'refs/heads/development') #=> <Reference>
 *    new_ref.name #=> 'refs/heads/development'
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :force ::
 *    Overwrites the reference with the given +name+, if it already exists,
 *    instead of raising an exception.
 *
 *  If a reference with the given +new_name+ already exists and +:force+ is not +true+,
 *  an exception will be raised.
 */
static VALUE rb_git_reference_collection_rename(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_new_name, rb_name_or_ref, rb_options;
	VALUE rb_repo = rugged_owner(self);
	git_reference *ref, *out = NULL;
	git_repository *repo;
	char *log_message = NULL;
	int error, force = 0;

	rb_scan_args(argc, argv, "20:", &rb_name_or_ref, &rb_new_name, &rb_options);
	Check_Type(rb_new_name, T_STRING);

	if (rb_obj_is_kind_of(rb_name_or_ref, rb_cRuggedReference))
		rb_name_or_ref = rb_funcall(rb_name_or_ref, rb_intern("canonical_name"), 0);

	if (TYPE(rb_name_or_ref) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Reference instance");

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if (!NIL_P(rb_options)) {
		VALUE rb_val = rb_hash_aref(rb_options, CSTR2SYM("message"));
		if (!NIL_P(rb_val))
			log_message = StringValueCStr(rb_val);

		force = RTEST(rb_hash_aref(rb_options, CSTR2SYM("force")));
	}

	if ((error = git_reference_lookup(&ref, repo, StringValueCStr(rb_name_or_ref))) == GIT_OK)
		error = git_reference_rename(&out, ref, StringValueCStr(rb_new_name), force, log_message);

	git_reference_free(ref);

	rugged_exception_check(error);

	return rugged_ref_new(rb_cRuggedReference, rugged_owner(self), out);
}

/*
 *  call-seq:
 *    references.update(ref, oid)             -> new_ref
 *    references.update(name, oid)            -> new_ref
 *    references.update(ref, other_ref)       -> new_ref
 *    references.update(name, other_ref_name) -> new_ref
 *
 *  Set the target of a reference. If +ref+ is a direct reference,
 *  the new target must be a +String+ representing a SHA1 OID.
 *
 *  If +reference+ is symbolic, the new target must be a +String+ with
 *  the name of another reference.
 *
 *  The original reference is unaltered; a new reference object is
 *  returned with the new target, and the changes are persisted to
 *  disk.
 *
 *    r1.type #=> :symbolic
 *    references.update(r1, "refs/heads/master") #=> <Reference>
 *
 *    r2.type #=> :direct
 *    references.update(r2, "de5ba987198bcf2518885f0fc1350e5172cded78") #=> <Reference>
 */
static VALUE rb_git_reference_collection_update(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_repo = rugged_owner(self), rb_name_or_ref, rb_target, rb_options;
	git_repository *repo = NULL;
	git_reference *ref = NULL, *out = NULL;
	char *log_message = NULL;
	int error;

	rb_scan_args(argc, argv, "20:", &rb_name_or_ref, &rb_target, &rb_options);

	if (rb_obj_is_kind_of(rb_name_or_ref, rb_cRuggedReference))
		rb_name_or_ref = rb_funcall(rb_name_or_ref, rb_intern("canonical_name"), 0);

	if (TYPE(rb_name_or_ref) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Reference instance");

	if (rb_obj_is_kind_of(rb_target, rb_cRuggedReference))
		rb_target = rb_funcall(rb_target, rb_intern("canonical_name"), 0);

	if (TYPE(rb_target) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Reference instance");

	if (!NIL_P(rb_options)) {
		VALUE rb_val = rb_hash_aref(rb_options, CSTR2SYM("message"));
		if (!NIL_P(rb_val))
			log_message = StringValueCStr(rb_val);
	}

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_reference_lookup(&ref, repo, StringValueCStr(rb_name_or_ref));
	rugged_exception_check(error);

	if (git_reference_type(ref) == GIT_REF_OID) {
		git_oid target;

		error = git_oid_fromstr(&target, StringValueCStr(rb_target));
		if (error) goto cleanup;

		error = git_reference_set_target(&out, ref, &target, log_message);
	} else {
		error = git_reference_symbolic_set_target(&out, ref, StringValueCStr(rb_target), log_message);
	}

cleanup:

	git_reference_free(ref);
	rugged_exception_check(error);

	return rugged_ref_new(rb_cRuggedReference, rb_repo, out);
}

/*
 *  call-seq:
 *    references.delete(ref) -> nil
 *    references.delete(name) -> nil
 *
 *  Delete specified reference.
 *
 *  If a Rugged::Reference object was passed, the object will become
 *  invalidated and won't be able to be used for any other operations.
 *
 *    repo.references.delete("HEAD")
 *    # Reference no longer exists on disk
 */
static VALUE rb_git_reference_collection_delete(VALUE self, VALUE rb_name_or_ref)
{
	VALUE rb_repo = rugged_owner(self);
	git_reference *ref;
	git_repository *repo;
	int error;

	if (rb_obj_is_kind_of(rb_name_or_ref, rb_cRuggedReference))
		rb_name_or_ref = rb_funcall(rb_name_or_ref, rb_intern("canonical_name"), 0);

	if (TYPE(rb_name_or_ref) != T_STRING)
		rb_raise(rb_eTypeError, "Expecting a String or Rugged::Reference instance");

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_reference_lookup(&ref, repo, StringValueCStr(rb_name_or_ref));
	rugged_exception_check(error);

	error = git_reference_delete(ref);
	git_reference_free(ref);
	rugged_exception_check(error);

	return Qnil;
}

void Init_rugged_reference_collection(void)
{
	rb_cRuggedReferenceCollection = rb_define_class_under(rb_mRugged, "ReferenceCollection", rb_cObject);
	rb_include_module(rb_cRuggedReferenceCollection, rb_mEnumerable);

	rb_define_method(rb_cRuggedReferenceCollection, "initialize", rb_git_reference_collection_initialize, 1);

	rb_define_method(rb_cRuggedReferenceCollection, "create",     rb_git_reference_collection_create, -1);
	rb_define_method(rb_cRuggedReferenceCollection, "[]",         rb_git_reference_collection_aref, 1);

	rb_define_method(rb_cRuggedReferenceCollection, "each",       rb_git_reference_collection_each, -1);
	rb_define_method(rb_cRuggedReferenceCollection, "each_name",  rb_git_reference_collection_each_name, -1);

	rb_define_method(rb_cRuggedReferenceCollection, "exist?",     rb_git_reference_collection_exist_p, 1);
	rb_define_method(rb_cRuggedReferenceCollection, "exists?",    rb_git_reference_collection_exist_p, 1);

	rb_define_method(rb_cRuggedReferenceCollection, "move",       rb_git_reference_collection_rename, -1);
	rb_define_method(rb_cRuggedReferenceCollection, "rename",     rb_git_reference_collection_rename, -1);
	rb_define_method(rb_cRuggedReferenceCollection, "update",     rb_git_reference_collection_update, -1);
	rb_define_method(rb_cRuggedReferenceCollection, "delete",      rb_git_reference_collection_delete, 1);
}
