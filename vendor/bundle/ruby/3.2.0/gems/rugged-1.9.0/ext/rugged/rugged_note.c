/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedObject;

extern const rb_data_type_t rugged_object_type;

static VALUE rugged_git_note_message(const git_note *note)
{
	const char *message;
	message = git_note_message(note);

	/*
	 * assume the note message is utf8 compatible, because that's
	 * the sensible thing to do.
	 */
	return rb_str_new_utf8(message);
}

static VALUE rugged_git_note_oid(const git_note* note)
{
	const git_oid *oid;
	oid = git_note_id(note);

	return rugged_create_oid(oid);
}

/*
 *  call-seq:
 *    obj.notes(notes_ref = 'refs/notes/commits') -> hash
 *
 *  Lookup a note for +obj+ from +notes_ref+:
 *  - +notes_ref+: (optional): canonical name of the reference to use, defaults to "refs/notes/commits"
 *
 *  Returns a new Hash object.
 *
 *    obj.notes #=> {:message=>"note text\n", :oid=>"94eca2de348d5f672faf56b0decafa5937e3235e"}
 */
static VALUE rb_git_note_lookup(int argc, VALUE *argv, VALUE self)
{
	git_repository *repo;
	const char *notes_ref = NULL;
	VALUE rb_notes_ref;
	VALUE rb_note_hash;
	VALUE owner;
	git_note *note;
	git_object *object;
	int error;

	rb_scan_args(argc, argv, "01", &rb_notes_ref);

	if (!NIL_P(rb_notes_ref)) {
		Check_Type(rb_notes_ref, T_STRING);
		notes_ref = StringValueCStr(rb_notes_ref);
	}

	TypedData_Get_Struct(self, git_object, &rugged_object_type, object);

	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	error = git_note_read(&note, repo, notes_ref, git_object_id(object));

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	rb_note_hash = rb_hash_new();
	rb_hash_aset(rb_note_hash, CSTR2SYM("message"), rugged_git_note_message(note));
	rb_hash_aset(rb_note_hash, CSTR2SYM("oid"), rugged_git_note_oid(note));

	git_note_free(note);

	return rb_note_hash;
}

/*
 *  call-seq:
 *    obj.create_note(data = {}) -> oid
 *
 *  Write a new +note+ to +object+, with the given +data+
 *  arguments, passed as a +Hash+:
 *
 *  - +:message+: the content of the note to add to the object
 *  - +:committer+: (optional) a hash with the signature for the committer
 *    defaults to the signature from the configuration
 *  - +:author+: (optional) a hash with the signature for the author
 *    defaults to the signature from the configuration
 *  - +:ref+: (optional): canonical name of the reference to use, defaults to "refs/notes/commits"
 *  - +:force+: (optional): overwrite existing note (disabled by default)
 *
 *  When the note is successfully written to disk, its +oid+ will be
 *  returned as a hex +String+.
 *
 *    author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}
 *
 *    obj.create_note(
 *      :author    => author,
 *      :committer => author,
 *      :message   => "Hello world\n\n",
 *      :ref       => 'refs/notes/builds'
 *    )
 */
static VALUE rb_git_note_create(VALUE self, VALUE rb_data)
{
	VALUE rb_ref, rb_message, rb_force;
	git_repository *repo = NULL;
	const char *notes_ref = NULL;

	VALUE owner;

	git_signature *author, *committer;
	git_oid note_oid;

	git_object *target = NULL;
	int error = 0;
	int force = 0;

	Check_Type(rb_data, T_HASH);

	TypedData_Get_Struct(self, git_object, &rugged_object_type, target);

	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	rb_ref = rb_hash_aref(rb_data, CSTR2SYM("ref"));

	rb_force = rb_hash_aref(rb_data, CSTR2SYM("force"));
	if (!NIL_P(rb_force))
		force = rugged_parse_bool(rb_force);

	if (!NIL_P(rb_ref)) {
		Check_Type(rb_ref, T_STRING);
		notes_ref = StringValueCStr(rb_ref);
	}

	rb_message = rb_hash_aref(rb_data, CSTR2SYM("message"));
	Check_Type(rb_message, T_STRING);

	committer = rugged_signature_get(
		rb_hash_aref(rb_data, CSTR2SYM("committer")), repo
	);

	author = rugged_signature_get(
		rb_hash_aref(rb_data, CSTR2SYM("author")), repo
	);

	error = git_note_create(
			&note_oid,
			repo,
			notes_ref,
			author,
			committer,
			git_object_id(target),
			StringValueCStr(rb_message),
			force);


	git_signature_free(author);
	git_signature_free(committer);

	rugged_exception_check(error);

	return rugged_create_oid(&note_oid);
}

/*
 *  call-seq:
 *    obj.remove_note(data = {}) -> boolean
 *
 *  Removes a +note+ from +object+, with the given +data+
 *  arguments, passed as a +Hash+:
 *
 *  - +:committer+ (optional): a hash with the signature for the committer,
 *    defaults to the signature from the configuration
 *  - +:author+ (optional): a hash with the signature for the author,
 *    defaults to the signature from the configuration
 *  - +:ref+: (optional): canonical name of the reference to use, defaults to "refs/notes/commits"
 *
 *  When the note is successfully removed +true+ will be
 *  returned as a +Boolean+.
 *
 *    author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}
 *
 *    obj.remove_note(
 *      :author    => author,
 *      :committer => author,
 *      :ref       => 'refs/notes/builds'
 *    )
 */
static VALUE rb_git_note_remove(int argc, VALUE *argv, VALUE self)
{
	int error = 0;
	const char *notes_ref = NULL;
	git_repository *repo = NULL;
	git_signature *author, *committer;
	git_object *target = NULL;
	VALUE rb_data;
	VALUE rb_ref = Qnil;
	VALUE rb_author = Qnil;
	VALUE rb_committer = Qnil;
	VALUE owner;

	TypedData_Get_Struct(self, git_object, &rugged_object_type, target);

	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	rb_scan_args(argc, argv, "01", &rb_data);

	if (!NIL_P(rb_data)) {
		Check_Type(rb_data, T_HASH);

		rb_ref = rb_hash_aref(rb_data, CSTR2SYM("ref"));
		if (!NIL_P(rb_ref)) {
			Check_Type(rb_ref, T_STRING);
			notes_ref = StringValueCStr(rb_ref);
		}

		rb_committer = rb_hash_aref(rb_data, CSTR2SYM("committer"));
		rb_author = rb_hash_aref(rb_data, CSTR2SYM("author"));
	}

	committer = rugged_signature_get(rb_committer, repo);
	author = rugged_signature_get(rb_author, repo);

	error = git_note_remove(
			repo,
			notes_ref,
			author,
			committer,
			git_object_id(target));

	git_signature_free(author);
	git_signature_free(committer);

	if (error == GIT_ENOTFOUND)
		return Qfalse;

	rugged_exception_check(error);

	return Qtrue;
}

static int cb_note__each(const git_oid *blob_id, const git_oid *annotated_object_id, void *data)
{
	VALUE rb_args = rb_ary_new2(2);
	struct rugged_cb_payload *payload = data;
	git_object *annotated_object;
	git_object *note_blob;

	git_repository *repo;

	Data_Get_Struct(payload->rb_data, git_repository, repo);

	rugged_exception_check(
		git_object_lookup(&annotated_object, repo, annotated_object_id, GIT_OBJ_ANY)
	);

	rugged_exception_check(
		git_object_lookup(&note_blob, repo, blob_id, GIT_OBJ_BLOB)
	);

	rb_ary_push(rb_args, rugged_object_new(payload->rb_data, note_blob));
	rb_ary_push(rb_args, rugged_object_new(payload->rb_data, annotated_object));

	rb_protect(rb_yield_splat, rb_args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

/*
 *  call-seq:
 *    repo.each_note(notes_ref = "refs/notes/commits") { |note_blob, annotated_object| block }
 *    repo.each_note(notes_ref = "refs/notes/commits") -> an_enumerator
 *
 *  Call the given block once for each note_blob/annotated_object pair in +repository+
 *  - +notes_ref+: (optional): canonical name of the reference to use defaults to "refs/notes/commits"
 *
 *  If no block is given, an +Enumerator+ is returned.
 *
 *    @repo.each_note do |note_blob, annotated_object|
 *      puts "#{note_blob.oid} => #{annotated_object.oid}"
 *    end
 */
static VALUE rb_git_note_each(int argc, VALUE *argv, VALUE self)
{
	git_repository *repo;
	const char *notes_ref = NULL;
	int error;
	struct rugged_cb_payload payload = { self, 0 };
	VALUE rb_notes_ref;

	RETURN_ENUMERATOR(self, argc, argv);
	rb_scan_args(argc, argv, "01", &rb_notes_ref);

	if (!NIL_P(rb_notes_ref)) {
		Check_Type(rb_notes_ref, T_STRING);
		notes_ref = StringValueCStr(rb_notes_ref);
	}

	Data_Get_Struct(self, git_repository, repo);

	error = git_note_foreach(repo, notes_ref, &cb_note__each, &payload);

	if (payload.exception)
		rb_jump_tag(payload.exception);
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    repo.notes_default_ref() -> string
 *
 *  Get the default notes reference for a +repository+:
 *
 *  Returns a new String object.
 *
 *    repo.default_notes_ref #=> "refs/notes/commits"
 */
static VALUE rb_git_note_default_ref_GET(VALUE self)
{
	git_repository *repo = NULL;
	git_buf ref_name = { 0 };
	VALUE rb_result;

	Data_Get_Struct(self, git_repository, repo);

	rugged_exception_check(
		git_note_default_ref(&ref_name, repo)
	);

	rb_result = rb_enc_str_new(ref_name.ptr, ref_name.size, rb_utf8_encoding());

	git_buf_dispose(&ref_name);

	return rb_result;
}

void Init_rugged_notes(void)
{
	rb_define_method(rb_cRuggedObject, "notes", rb_git_note_lookup, -1);
	rb_define_method(rb_cRuggedObject, "create_note", rb_git_note_create, 1);
	rb_define_method(rb_cRuggedObject, "remove_note", rb_git_note_remove, -1);

	rb_define_method(rb_cRuggedRepo, "each_note", rb_git_note_each, -1);
	rb_define_method(rb_cRuggedRepo, "default_notes_ref", rb_git_note_default_ref_GET, 0);
}
