/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedObject;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedReference;

VALUE rb_cRuggedTag;
VALUE rb_cRuggedTagAnnotation;

extern const rb_data_type_t rugged_object_type;

/*
 *  call-seq:
 *    annotation.target -> object
 *
 *  Return the +object+ pointed at by this tag annotation, as a <tt>Rugged::Object</tt>
 *  instance.
 *
 *    annotation.target #=> #<Rugged::Commit:0x108828918>
 */
static VALUE rb_git_tag_annotation_target(VALUE self)
{
	git_tag *tag;
	git_object *target;
	int error;
	VALUE owner;

	TypedData_Get_Struct(self, git_tag, &rugged_object_type, tag);
	owner = rugged_owner(self);

	error = git_tag_target(&target, tag);
	rugged_exception_check(error);

	return rugged_object_new(owner, target);
}

/*
 *  call-seq:
 *    annotation.target_oid -> oid
 *    annotation.target_id -> oid
 *
 *  Return the oid pointed at by this tag annotation, as a <tt>String</tt>
 *  instance.
 *
 *    annotation.target_id #=> "2cb831a8aea28b2c1b9c63385585b864e4d3bad1"
 */
static VALUE rb_git_tag_annotation_target_id(VALUE self)
{
	git_tag *tag;
	const git_oid *target_oid;

	TypedData_Get_Struct(self, git_tag, &rugged_object_type, tag);

	target_oid = git_tag_target_id(tag);

	return rugged_create_oid(target_oid);
}

/*
 *  call-seq:
 *    annotation.type -> t
 *
 *  Return a symbol representing the type of the objeced pointed at by
 *  this +annotation+. Possible values are +:blob+, +:commit+, +:tree+ and +:tag+.
 *
 *  This is always the same as the +type+ of the returned <tt>annotation.target</tt>
 *
 *    annotation.type #=> :commit
 *    annotation.target.type == annotation.type #=> true
 */
static VALUE rb_git_tag_annotation_target_type(VALUE self)
{
	git_tag *tag;
	TypedData_Get_Struct(self, git_tag, &rugged_object_type, tag);

	return rugged_otype_new(git_tag_target_type(tag));
}

/*
 *  call-seq:
 *    annotation.name -> name
 *
 *  Return a string with the name of this tag +annotation+.
 *
 *    annotation.name #=> "v0.16.0"
 */
static VALUE rb_git_tag_annotation_name(VALUE self)
{
	git_tag *tag;
	TypedData_Get_Struct(self, git_tag, &rugged_object_type, tag);

	return rb_str_new_utf8(git_tag_name(tag));
}

/*
 *  call-seq:
 *    annotation.tagger -> signature
 *
 *  Return the signature for the author of this tag +annotation+. The signature
 *  is returned as a +Hash+ containing +:name+, +:email+ of the author
 *  and +:time+ of the tagging.
 *
 *    annotation.tagger #=> {:email=>"tanoku@gmail.com", :time=>Tue Jan 24 05:42:45 UTC 2012, :name=>"Vicent Mart\303\255"}
 */
static VALUE rb_git_tag_annotation_tagger(VALUE self)
{
	git_tag *tag;
	const git_signature *tagger;

	TypedData_Get_Struct(self, git_tag, &rugged_object_type, tag);
	tagger = git_tag_tagger(tag);

	if (!tagger)
		return Qnil;

	return rugged_signature_new(tagger, NULL);
}

/*
 *  call-seq:
 *    annotation.message -> msg
 *
 *  Return the message of this tag +annotation+. This includes the full body of the
 *  message and any optional footers or signatures after it.
 *
 *    annotation.message #=> "Release v0.16.0, codename 'broken stuff'"
 */
static VALUE rb_git_tag_annotation_message(VALUE self)
{
	git_tag *tag;
	const char *message;

	TypedData_Get_Struct(self, git_tag, &rugged_object_type, tag);
	message = git_tag_message(tag);

	if (!message)
		return Qnil;

	return rb_str_new_utf8(message);
}

/*
 *  call-seq:
 *    tag.annotation -> annotation or nil
 */
static VALUE rb_git_tag_annotation(VALUE self)
{
	git_reference *ref, *resolved_ref;
	git_repository *repo;
	git_object *target;
	int error;
	VALUE rb_repo = rugged_owner(self);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(self, git_reference, ref);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_reference_resolve(&resolved_ref, ref);
	rugged_exception_check(error);

	error = git_object_lookup(&target, repo, git_reference_target(resolved_ref), GIT_OBJ_TAG);
	git_reference_free(resolved_ref);

	if (error == GIT_ENOTFOUND)
		return Qnil;

	return rugged_object_new(rb_repo, target);
}

/*
 *  call-seq:
 *    tag.target -> git_object
 */
static VALUE rb_git_tag_target(VALUE self)
{
	git_reference *ref, *resolved_ref;
	git_repository *repo;
	git_object *target;
	int error;
	VALUE rb_repo = rugged_owner(self);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(self, git_reference, ref);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_reference_resolve(&resolved_ref, ref);
	rugged_exception_check(error);

	error = git_object_lookup(&target, repo, git_reference_target(resolved_ref), GIT_OBJ_ANY);
	git_reference_free(resolved_ref);
	rugged_exception_check(error);

	if (git_object_type(target) == GIT_OBJ_TAG) {
		git_object *annotation_target;

		error = git_tag_target(&annotation_target, (git_tag *)target);
		git_object_free(target);
		rugged_exception_check(error);

		return rugged_object_new(rb_repo, annotation_target);
	} else {
		return rugged_object_new(rb_repo, target);
	}
}

static VALUE rb_git_tag_annotated_p(VALUE self)
{
	return RTEST(rb_git_tag_annotation(self)) ? Qtrue : Qfalse;
}

void Init_rugged_tag(void)
{
	rb_cRuggedTag = rb_define_class_under(rb_mRugged, "Tag", rb_cRuggedReference);

	rb_define_method(rb_cRuggedTag, "annotation", rb_git_tag_annotation, 0);
	rb_define_method(rb_cRuggedTag, "annotated?", rb_git_tag_annotated_p, 0);
	rb_define_method(rb_cRuggedTag, "target", rb_git_tag_target, 0);

	rb_cRuggedTagAnnotation = rb_define_class_under(rb_cRuggedTag, "Annotation", rb_cRuggedObject);
	rb_undef_alloc_func(rb_cRuggedTagAnnotation);

	rb_define_method(rb_cRuggedTagAnnotation, "message", rb_git_tag_annotation_message, 0);
	rb_define_method(rb_cRuggedTagAnnotation, "name", rb_git_tag_annotation_name, 0);
	rb_define_method(rb_cRuggedTagAnnotation, "target", rb_git_tag_annotation_target, 0);
	rb_define_method(rb_cRuggedTagAnnotation, "target_oid", rb_git_tag_annotation_target_id, 0);
	rb_define_method(rb_cRuggedTagAnnotation, "target_id", rb_git_tag_annotation_target_id, 0);
	rb_define_method(rb_cRuggedTagAnnotation, "target_type", rb_git_tag_annotation_target_type, 0);
	rb_define_method(rb_cRuggedTagAnnotation, "tagger", rb_git_tag_annotation_tagger, 0);
}
