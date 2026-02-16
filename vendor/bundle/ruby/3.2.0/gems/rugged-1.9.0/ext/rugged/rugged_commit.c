/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"
#include "git2/commit.h"
#include "git2/message.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedObject;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedSignature;
VALUE rb_cRuggedCommit;

extern const rb_data_type_t rugged_object_type;

/*
 *  call-seq:
 *    commit.message -> msg
 *
 *  Return the message of this commit. This includes the full body of the
 *  message, with the short description, detailed description, and any
 *  optional footers or signatures after it.
 *
 *  In Ruby 1.9+, the returned string will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 *    commit.message #=> "add a lot of RDoc docs\n\nthis includes docs for commit and blob"
 */
static VALUE rb_git_commit_message_GET(VALUE self)
{
	git_commit *commit;
	rb_encoding *encoding = rb_utf8_encoding();
	const char *encoding_name;
	const char *message;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	message = git_commit_message(commit);
	encoding_name = git_commit_message_encoding(commit);
	if (encoding_name != NULL)
		encoding = rb_enc_find(encoding_name);

	return rb_enc_str_new(message, strlen(message), encoding);
}

/*
 *  call-seq:
 *    commit.trailers -> [["Trailer-name", "trailer value"], ...]
 *
 *  Return an array of arrays, each of which is a key/value pair representing a
 *  commit message trailer. Both the keys and values will be strings. An array
 *  is used to preserve the order the trailers were found.
 *
 *  In Ruby 1.9+, the returned strings will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 */
static VALUE rb_git_commit_trailers_GET(VALUE self)
{
	git_commit *commit;
	const char *message;
	rb_encoding *encoding = rb_utf8_encoding();
	const char *encoding_name;
	git_message_trailer_array arr;
	VALUE trailers = rb_ary_new();
	int error;
	size_t i;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	encoding_name = git_commit_message_encoding(commit);
	if (encoding_name != NULL)
		encoding = rb_enc_find(encoding_name);

	message = git_commit_message(commit);

	error = git_message_trailers(&arr, message);
	rugged_exception_check(error);

	for(i = 0; i < arr.count; i++) {
		VALUE pair = rb_ary_new();
		const char *key = arr.trailers[i].key;
		const char *value = arr.trailers[i].value;

		// trailer key
		rb_ary_push(pair, rb_enc_str_new(key, strlen(key), encoding));

		// trailer value
		rb_ary_push(pair, rb_enc_str_new(value, strlen(value), encoding));

		// add it to the list
		rb_ary_push(trailers, pair);
	}

	git_message_trailer_array_free(&arr);

	return trailers;
}

/*
 *  call-seq:
 *    commit.summary -> summary
 *
 *  Return the short summary message of this commit.
 *
 *  In Ruby 1.9+, the returned string will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 *    commit.message #=> "add a lot of RDoc docs\n\nthis includes docs for commit and blob"
 *    commit.summary #=> "add a lot of RDoc docs"
 */
static VALUE rb_git_commit_summary_GET(VALUE self)
{
	git_commit *commit;
	rb_encoding *encoding = rb_utf8_encoding();
	const char *encoding_name;
	const char *summary;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	summary = git_commit_summary(commit);
	encoding_name = git_commit_message_encoding(commit);
	if (encoding_name != NULL)
		encoding = rb_enc_find(encoding_name);

	return rb_enc_str_new(summary, strlen(summary), encoding);
}

/*
 *  call-seq:
 *    commit.committer -> signature
 *
 *  Return the signature for the committer of this +commit+. The signature
 *  is returned as a +Hash+ containing +:name+, +:email+ of the author
 *  and +:time+ of the change.
 *
 *  The committer of a commit is the person who actually applied the changes
 *  of the commit; in most cases it's the same as the author.
 *
 *  In Ruby 1.9+, the returned string will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 *    commit.committer #=> {:email=>"tanoku@gmail.com", :time=>Tue Jan 24 05:42:45 UTC 2012, :name=>"Vicent Mart\303\255"}
 */
static VALUE rb_git_commit_committer_GET(VALUE self)
{
	git_commit *commit;
	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	return rugged_signature_new(
		git_commit_committer(commit),
		git_commit_message_encoding(commit));
}

/*
 *  call-seq:
 *    commit.author -> signature
 *
 *  Return the signature for the author of this +commit+. The signature
 *  is returned as a +Hash+ containing +:name+, +:email+ of the author
 *  and +:time+ of the change.
 *
 *  The author of the commit is the person who intially created the changes.
 *
 *  In Ruby 1.9+, the returned string will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 *    commit.author #=> {:email=>"tanoku@gmail.com", :time=>Tue Jan 24 05:42:45 UTC 2012, :name=>"Vicent Mart\303\255"}
 */
static VALUE rb_git_commit_author_GET(VALUE self)
{
	git_commit *commit;
	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	return rugged_signature_new(
		git_commit_author(commit),
		git_commit_message_encoding(commit));
}

/*
 *  call-seq:
 *    commit.epoch_time -> int
 *
 *  Return the time when this commit was made effective. This is the same value
 *  as the +:time+ attribute for +commit.committer+, but represented as an +Integer+
 *  value in seconds since the Epoch.
 *
 *    commit.time #=> 1327383765
 */
static VALUE rb_git_commit_epoch_time_GET(VALUE self)
{
	git_commit *commit;
	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	return ULONG2NUM(git_commit_time(commit));
}

/*
 *  call-seq:
 *    commit.tree -> tree
 *
 *  Return the tree pointed at by this +commit+. The tree is
 *  returned as a +Rugged::Tree+ object.
 *
 *    commit.tree #=> #<Rugged::Tree:0x10882c680>
 */
static VALUE rb_git_commit_tree_GET(VALUE self)
{
	git_commit *commit;
	git_tree *tree;
	VALUE owner;
	int error;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);
	owner = rugged_owner(self);

	error = git_commit_tree(&tree, commit);
	rugged_exception_check(error);

	return rugged_object_new(owner, (git_object *)tree);
}

/*
 *  call-seq:
 *    commit.tree_id -> oid
 *
 *  Return the tree oid pointed at by this +commit+. The tree is
 *  returned as a String object.
 *
 *    commit.tree_id #=> "f148106ca58764adc93ad4e2d6b1d168422b9796"
 */
static VALUE rb_git_commit_tree_id_GET(VALUE self)
{
	git_commit *commit;
	const git_oid *tree_id;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	tree_id = git_commit_tree_id(commit);

	return rugged_create_oid(tree_id);
}

/*
 *  call-seq:
 *    commit.parents -> [commit, ...]
 *
 *  Return the parent(s) of this commit as an array of +Rugged::Commit+
 *  objects. An array is always returned even when the commit has only
 *  one or zero parents.
 *
 *    commit.parents #=> => [#<Rugged::Commit:0x108828918>]
 *    root.parents #=> []
 */
static VALUE rb_git_commit_parents_GET(VALUE self)
{
	git_commit *commit;
	git_commit *parent;
	unsigned int n, parent_count;
	VALUE ret_arr, owner;
	int error;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);
	owner = rugged_owner(self);

	parent_count = git_commit_parentcount(commit);
	ret_arr = rb_ary_new2((long)parent_count);

	for (n = 0; n < parent_count; n++) {
		error = git_commit_parent(&parent, commit, n);
		rugged_exception_check(error);
		rb_ary_push(ret_arr, rugged_object_new(owner, (git_object *)parent));
	}

	return ret_arr;
}

/*
 *  call-seq:
 *    commit.parent_ids -> [oid, ...]
 *
 *  Return the parent oid(s) of this commit as an array of oid String
 *  objects. An array is always returned even when the commit has only
 *  one or zero parents.
 *
 *    commit.parent_ids #=> => ["2cb831a8aea28b2c1b9c63385585b864e4d3bad1", ...]
 *    root.parent_ids #=> []
 */
static VALUE rb_git_commit_parent_ids_GET(VALUE self)
{
	git_commit *commit;
	const git_oid *parent_id;
	unsigned int n, parent_count;
	VALUE ret_arr;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	parent_count = git_commit_parentcount(commit);
	ret_arr = rb_ary_new2((long)parent_count);

	for (n = 0; n < parent_count; n++) {
		parent_id = git_commit_parent_id(commit, n);
		if (parent_id) {
			rb_ary_push(ret_arr, rugged_create_oid(parent_id));
		}
	}

	return ret_arr;
}

/*
 *  call-seq:
 *    commit.amend(data = {}) -> oid
 *
 *  Amend a commit object, with the given +data+
 *  arguments, passed as a +Hash+:
 *
 *  - +:message+: a string with the full text for the commit's message
 *  - +:committer+ (optional): a hash with the signature for the committer,
 *    defaults to the signature from the configuration
 *  - +:author+ (optional): a hash with the signature for the author,
 *    defaults to the signature from the configuration
 *  - +:tree+: the tree for this amended commit, represented as a <tt>Rugged::Tree</tt>
 *    instance or an OID +String+.
 *  - +:update_ref+ (optional): a +String+ with the name of a reference in the
 *  repository which should be updated to point to this amended commit (e.g. "HEAD")
 *
 *  When the amended commit is successfully written to disk, its +oid+ will be
 *  returned as a hex +String+.
 *
 *    author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}
 *
 *    commit.amend(
 *      :author => author,
 *      :message => "Updated Hello world\n\n",
 *      :committer => author,
 *      :tree => some_tree) #=> "f148106ca58764adc93ad4e2d6b1d168422b9796"
 */
static VALUE rb_git_commit_amend(VALUE self, VALUE rb_data)
{
	VALUE rb_message, rb_tree, rb_ref, owner;
	int error = 0;
	git_commit *commit_to_amend;
	char *message = NULL;
	git_tree *tree = NULL;
	git_signature *author = NULL, *committer = NULL;
	git_oid commit_oid;
	git_repository *repo;
	const char *update_ref = NULL;

	Check_Type(rb_data, T_HASH);

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit_to_amend);

	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	rb_ref = rb_hash_aref(rb_data, CSTR2SYM("update_ref"));
	if (!NIL_P(rb_ref)) {
		Check_Type(rb_ref, T_STRING);
		update_ref = StringValueCStr(rb_ref);
	}

	rb_message = rb_hash_aref(rb_data, CSTR2SYM("message"));
	if (!NIL_P(rb_message)) {
		Check_Type(rb_message, T_STRING);
		message = StringValueCStr(rb_message);
	}

	rb_tree = rb_hash_aref(rb_data, CSTR2SYM("tree"));
	if (!NIL_P(rb_tree))
		tree = (git_tree *)rugged_object_get(repo, rb_tree, GIT_OBJ_TREE);

	if (!NIL_P(rb_hash_aref(rb_data, CSTR2SYM("committer")))) {
		committer = rugged_signature_get(
			rb_hash_aref(rb_data, CSTR2SYM("committer")), repo
		);
	}

	if (!NIL_P(rb_hash_aref(rb_data, CSTR2SYM("author")))) {
		author = rugged_signature_get(
			rb_hash_aref(rb_data, CSTR2SYM("author")), repo
		);
	}

	error = git_commit_amend(
		&commit_oid,
		commit_to_amend,
		update_ref,
		author,
		committer,
		NULL,
		message,
		tree);

	git_signature_free(author);
	git_signature_free(committer);

	git_object_free((git_object *)tree);

	rugged_exception_check(error);

	return rugged_create_oid(&commit_oid);
}

struct commit_data {
	VALUE rb_err_obj;

	const char *update_ref;
	const char *message;
	git_tree *tree;
	git_signature *author;
	git_signature *committer;
	int parent_count;
	const git_commit **parents;
};

/**
 * Parse the commit options into something we can re-use
 *
 * Note that parents may be set even when the function errors, so make
 * sure to free this data.
 */
static int parse_commit_options(struct commit_data *out, git_repository *repo, VALUE rb_data)
{
	VALUE rb_message, rb_tree, rb_parents, rb_ref;
	int error = 0, parent_count, i;

	rb_ref = rb_hash_aref(rb_data, CSTR2SYM("update_ref"));
	if (!NIL_P(rb_ref)) {
		Check_Type(rb_ref, T_STRING);
		out->update_ref = StringValueCStr(rb_ref);
	}

	rb_message = rb_hash_aref(rb_data, CSTR2SYM("message"));
	Check_Type(rb_message, T_STRING);
	out->message = StringValueCStr(rb_message);

	out->committer = rugged_signature_get(
		rb_hash_aref(rb_data, CSTR2SYM("committer")), repo
	);

	out->author = rugged_signature_get(
		rb_hash_aref(rb_data, CSTR2SYM("author")), repo
	);

	rb_parents = rb_hash_aref(rb_data, CSTR2SYM("parents"));
	Check_Type(rb_parents, T_ARRAY);

	rb_tree = rb_hash_aref(rb_data, CSTR2SYM("tree"));
	out->tree = (git_tree *)rugged_object_get(repo, rb_tree, GIT_OBJ_TREE);

	out->parents = xcalloc(RARRAY_LEN(rb_parents), sizeof(void *));
	parent_count = 0;

	for (i = 0; i < (int)RARRAY_LEN(rb_parents); ++i) {
		VALUE p = rb_ary_entry(rb_parents, i);
		git_commit *parent = NULL;
		git_commit *tmp = NULL;

		if (NIL_P(p))
			continue;

		if (TYPE(p) == T_STRING) {
			git_oid oid;

			error = git_oid_fromstr(&oid, StringValueCStr(p));
			if (error < GIT_OK)
				goto out;

			error = git_commit_lookup(&parent, repo, &oid);
			if (error < GIT_OK)
				goto out;
		} else if (rb_obj_is_kind_of(p, rb_cRuggedCommit)) {
			TypedData_Get_Struct(p, git_commit, &rugged_object_type, tmp);
			if ((error = git_object_dup((git_object **) &parent, (git_object *) tmp)) < 0)
				goto out;
		} else {
			out->rb_err_obj = rb_exc_new2(rb_eTypeError, "Invalid type for parent object");
			error = -1;
			goto out;
		}

		out->parents[parent_count] = parent;
		parent_count++;
	}

out:
	out->parent_count = parent_count;
	return error;
}

static void free_commit_options(struct commit_data *commit_data)
{
	int i;

	git_signature_free(commit_data->author);
	git_signature_free(commit_data->committer);

	git_object_free((git_object *)commit_data->tree);

	for (i = 0; i < commit_data->parent_count; ++i)
		git_object_free((git_object *) commit_data->parents[i]);
	xfree(commit_data->parents);
}

/*
 *  call-seq:
 *    Commit.create(repository, data = {}) -> oid
 *
 *  Write a new +Commit+ object to +repository+, with the given +data+
 *  arguments, passed as a +Hash+:
 *
 *  - +:message+: a string with the full text for the commit's message
 *  - +:committer+ (optional): a hash with the signature for the committer,
 *    defaults to the signature from the configuration
 *  - +:author+ (optional): a hash with the signature for the author,
 *    defaults to the signature from the configuration
 *  - +:parents+: an +Array+ with zero or more parents for this commit,
 *    represented as <tt>Rugged::Commit</tt> instances, or OID +String+.
 *  - +:tree+: the tree for this commit, represented as a <tt>Rugged::Tree</tt>
 *    instance or an OID +String+.
 *  - +:update_ref+ (optional): a +String+ with the name of a reference in the
 *    repository which should be updated to point to this commit (e.g. "HEAD")
 *
 *  When the commit is successfully written to disk, its +oid+ will be
 *  returned as a hex +String+.
 *
 *    author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}
 *
 *    Rugged::Commit.create(r,
 *      :author => author,
 *      :message => "Hello world\n\n",
 *      :committer => author,
 *      :parents => ["2cb831a8aea28b2c1b9c63385585b864e4d3bad1"],
 *      :tree => some_tree) #=> "f148106ca58764adc93ad4e2d6b1d168422b9796"
 */
static VALUE rb_git_commit_create(VALUE self, VALUE rb_repo, VALUE rb_data)
{
	int error = 0;
	struct commit_data commit_data = { Qnil };
	git_oid commit_oid;
	git_repository *repo;

	Check_Type(rb_data, T_HASH);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if ((error = parse_commit_options(&commit_data, repo, rb_data)) < 0)
		goto cleanup;

	error = git_commit_create(
		&commit_oid,
		repo,
		commit_data.update_ref,
		commit_data.author,
		commit_data.committer,
		NULL,
		commit_data.message,
		commit_data.tree,
		commit_data.parent_count,
		commit_data.parents);

cleanup:
	free_commit_options(&commit_data);
	if (!NIL_P(commit_data.rb_err_obj))
		rb_exc_raise(commit_data.rb_err_obj);

	rugged_exception_check(error);

	return rugged_create_oid(&commit_oid);
}

/*
 *  call-seq:
 *    commit.to_mbox(options = {}) -> str
 *
 *  Returns +commit+'s contents formatted to resemble UNIX mailbox format.
 *
 *  Does not (yet) support merge commits.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :patch_no ::
 *    Number for this patch in the series. Defaults to +1+.
 *
 *  :total_patches ::
 *    Total number of patches in the series. Defaults to +1+.
 *
 *  :exclude_subject_patch_marker ::
 *    If set to true, no "[PATCH]" marker will be
 *    added to the beginning of the subject line.
 *
 *  Additionally, you can also pass the same options as for Rugged::Tree#diff.
 */
static VALUE rb_git_commit_to_mbox(int argc, VALUE *argv, VALUE self)
{
	git_buf email_patch = { NULL };
	git_repository *repo;
	git_commit *commit;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff_format_email_flags_t flags = GIT_DIFF_FORMAT_EMAIL_NONE;

	VALUE rb_repo = rugged_owner(self), rb_email_patch = Qnil, rb_val, rb_options;

	int error;
	size_t patch_no = 1, total_patches = 1;

	rb_scan_args(argc, argv, ":", &rb_options);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	if (!NIL_P(rb_options)) {
		Check_Type(rb_options, T_HASH);

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("patch_no"));
		if (!NIL_P(rb_val))
			patch_no = NUM2INT(rb_val);

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("total_patches"));
		if (!NIL_P(rb_val))
			total_patches = NUM2INT(rb_val);

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_subject_patch_marker"))))
			flags |= GIT_DIFF_FORMAT_EMAIL_EXCLUDE_SUBJECT_PATCH_MARKER;

		rugged_parse_diff_options(&opts, rb_options);
	}

	error = git_diff_commit_as_email(
		&email_patch,
		repo,
		commit,
		patch_no,
		total_patches,
		flags,
		&opts);

	if (error) goto cleanup;

	rb_email_patch = rb_enc_str_new(email_patch.ptr, email_patch.size, rb_utf8_encoding());

	cleanup:

	xfree(opts.pathspec.strings);
	git_buf_dispose(&email_patch);
	rugged_exception_check(error);

	return rb_email_patch;
}

/*
 *  call-seq:
 *    commit.header_field(field_name) -> str
 *
 *  Returns +commit+'s header field value.
 */
static VALUE rb_git_commit_header_field(VALUE self, VALUE rb_field)
{
	git_buf header_field = { 0 };
	git_commit *commit = NULL;

	const char *encoding_name;
	rb_encoding *encoding = rb_utf8_encoding();
	VALUE rb_result;

	int error;

	Check_Type(rb_field, T_STRING);
	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	error = git_commit_header_field(&header_field, commit, StringValueCStr(rb_field));

	if (error < 0) {
		git_buf_dispose(&header_field);
		if (error == GIT_ENOTFOUND)
			return Qnil;
		rugged_exception_check(error);
	}

	encoding_name = git_commit_message_encoding(commit);
	if (encoding_name != NULL)
		encoding = rb_enc_find(encoding_name);

	rb_result = rb_enc_str_new(header_field.ptr, header_field.size, encoding);
	git_buf_dispose(&header_field);
	return rb_result;
}

/*
 *  call-seq:
 *    commit.header -> str
 *
 *  Returns +commit+'s entire raw header.
 */
static VALUE rb_git_commit_header(VALUE self)
{
	git_commit *commit;
	const char *raw_header;

	TypedData_Get_Struct(self, git_commit, &rugged_object_type, commit);

	raw_header = git_commit_raw_header(commit);
	return rb_str_new_utf8(raw_header);
}

/*
 *  call-seq:
 *    Commit.extract_signature(repo, commit, field_name) -> [str, str]
 *
 *  Returns +commit+'s signature in 'field' and the signed data
 *
 *  The signature is done over the contents of the commit without the
 *  signature block in the header, which is the data in the second
 *  element in the return array.
 */
static VALUE rb_git_commit_extract_signature(int argc, VALUE *argv, VALUE self)
{
	int error;
	VALUE ret;
	git_oid commit_id;
	const char *field;
	git_repository *repo;
	git_buf signature = {0}, signed_data = {0};
	VALUE rb_repo, rb_commit, rb_field = Qnil;

	rb_scan_args(argc, argv, "21", &rb_repo, &rb_commit, &rb_field);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_oid_fromstr(&commit_id, StringValueCStr(rb_commit));
	rugged_exception_check(error);

	field = NIL_P(rb_field) ? NULL : StringValueCStr(rb_field);
	error = git_commit_extract_signature(&signature, &signed_data, repo, &commit_id, field);
	if (error < 0) {
		git_buf_dispose(&signature);
		git_buf_dispose(&signed_data);
	}

	if (error == GIT_ENOTFOUND && giterr_last()->klass == GITERR_OBJECT ) {
		ret = Qnil;
	} else {
		rugged_exception_check(error);

		ret = rb_ary_new3(2, rb_str_new(signature.ptr, signature.size),
				     rb_str_new(signed_data.ptr, signed_data.size));
	}

	git_buf_dispose(&signature);
	git_buf_dispose(&signed_data);

	return ret;
}

/*
 *  call-seq:
 *    Commit.create_to_s(repository, data = {}) -> str
 *
 *  Create a string with the contents of the commit, created with the
 *  given +data+ arguments, passed as a +Hash+:
 *
 *  - +:message+: a string with the full text for the commit's message
 *  - +:committer+ (optional): a hash with the signature for the committer,
 *    defaults to the signature from the configuration
 *  - +:author+ (optional): a hash with the signature for the author,
 *    defaults to the signature from the configuration
 *  - +:parents+: an +Array+ with zero or more parents for this commit,
 *    represented as <tt>Rugged::Commit</tt> instances, or OID +String+.
 *  - +:tree+: the tree for this commit, represented as a <tt>Rugged::Tree</tt>
 *    instance or an OID +String+.
 *
 *    author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}
 *
 *    Rugged::Commit.create(r,
 *      :author => author,
 *      :message => "Hello world\n\n",
 *      :committer => author,
 *      :parents => ["2cb831a8aea28b2c1b9c63385585b864e4d3bad1"],
 *      :tree => some_tree) #=> "tree some_tree\nparent 2cb831...."
 */
static VALUE rb_git_commit_create_to_s(VALUE self, VALUE rb_repo, VALUE rb_data)
{
	int error = 0;
	struct commit_data commit_data = { Qnil };
	git_repository *repo;
	git_buf buf = { 0 };
	VALUE ret;

	Check_Type(rb_data, T_HASH);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if ((error = parse_commit_options(&commit_data, repo, rb_data)) < 0)
		goto cleanup;

	error = git_commit_create_buffer(
		&buf,
		repo,
		commit_data.author,
		commit_data.committer,
		NULL,
		commit_data.message,
		commit_data.tree,
		commit_data.parent_count,
		commit_data.parents);

cleanup:
	free_commit_options(&commit_data);
	if (!NIL_P(commit_data.rb_err_obj))
		rb_exc_raise(commit_data.rb_err_obj);

	rugged_exception_check(error);

	ret = rb_str_new_utf8(buf.ptr);
	git_buf_dispose(&buf);

	return ret;
}

/*
 *  call-seq:
 *    Commit.create_with_signature(repo, content, signature, field_name = "gpgsig") -> oid
 *
 *  Create a commit from the +content+ string and the +signature+,
 *  adding this data to the +field_name+ header field in the resulting
 *  commit.
 *
 *  Returns the new commit's object id.
 *
 */
static VALUE rb_git_commit_create_with_signature(int argc, VALUE *argv, VALUE self)
{
	int error;
	git_oid id;
	const char *field = NULL;
	git_repository *repo;
	VALUE rb_repo, rb_content, rb_signature, rb_field = Qnil;

	rb_scan_args(argc, argv, "31", &rb_repo, &rb_content, &rb_signature, &rb_field);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_content, T_STRING);
	Check_Type(rb_signature, T_STRING);

	if (!NIL_P(rb_field)) {
		Check_Type(rb_field, T_STRING);
		field = StringValueCStr(rb_field);
	}

	error = git_commit_create_with_signature(&id, repo, StringValueCStr(rb_content), StringValueCStr(rb_signature), field);
	rugged_exception_check(error);

	return rugged_create_oid(&id);
}

void Init_rugged_commit(void)
{
	rb_cRuggedCommit = rb_define_class_under(rb_mRugged, "Commit", rb_cRuggedObject);
	rb_undef_alloc_func(rb_cRuggedCommit);

	rb_define_singleton_method(rb_cRuggedCommit, "create", rb_git_commit_create, 2);
	rb_define_singleton_method(rb_cRuggedCommit, "create_to_s", rb_git_commit_create_to_s, 2);
	rb_define_singleton_method(rb_cRuggedCommit, "create_with_signature", rb_git_commit_create_with_signature, -1);
	rb_define_singleton_method(rb_cRuggedCommit, "extract_signature", rb_git_commit_extract_signature, -1);

	rb_define_method(rb_cRuggedCommit, "message", rb_git_commit_message_GET, 0);
	rb_define_method(rb_cRuggedCommit, "trailers", rb_git_commit_trailers_GET, 0);
	rb_define_method(rb_cRuggedCommit, "summary", rb_git_commit_summary_GET, 0);
	rb_define_method(rb_cRuggedCommit, "epoch_time", rb_git_commit_epoch_time_GET, 0);
	rb_define_method(rb_cRuggedCommit, "committer", rb_git_commit_committer_GET, 0);
	rb_define_method(rb_cRuggedCommit, "author", rb_git_commit_author_GET, 0);
	rb_define_method(rb_cRuggedCommit, "tree", rb_git_commit_tree_GET, 0);

	rb_define_method(rb_cRuggedCommit, "tree_id", rb_git_commit_tree_id_GET, 0);
	rb_define_method(rb_cRuggedCommit, "tree_oid", rb_git_commit_tree_id_GET, 0);

	rb_define_method(rb_cRuggedCommit, "parents", rb_git_commit_parents_GET, 0);
	rb_define_method(rb_cRuggedCommit, "parent_ids", rb_git_commit_parent_ids_GET, 0);
	rb_define_method(rb_cRuggedCommit, "parent_oids", rb_git_commit_parent_ids_GET, 0);

	rb_define_method(rb_cRuggedCommit, "amend", rb_git_commit_amend, 1);

	rb_define_method(rb_cRuggedCommit, "to_mbox", rb_git_commit_to_mbox, -1);

	rb_define_method(rb_cRuggedCommit, "header_field", rb_git_commit_header_field, 1);
	rb_define_method(rb_cRuggedCommit, "header", rb_git_commit_header, 0);
}
