/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"
#include <ctype.h>
#include <git2/sys/hashsig.h>

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedObject;
extern VALUE rb_cRuggedRepo;
static ID id_read;

VALUE rb_cRuggedBlob;
VALUE rb_cRuggedBlobSig;

extern const rb_data_type_t rugged_object_type;

/*
 *  call-seq:
 *    blob.text(max_lines = -1, encoding = Encoding.default_external) -> string
 *
 *  Return up to +max_lines+ of text from a blob as a +String+.
 *  If +max_lines+ is less than 0, the full string is returned.
 *
 *  The string is created with the given +encoding+, defaulting to
 *  Encoding.default_external.
 *
 *  When limiting the size of the text with +max_lines+, the string is
 *  expected to have an ASCII-compatible encoding, and is checked
 *  for the newline +\n+ character.
 */
static VALUE rb_git_blob_text_GET(int argc, VALUE *argv, VALUE self)
{
	git_blob *blob;
	size_t size;
	const char *content;
	VALUE rb_max_lines, rb_encoding;

	TypedData_Get_Struct(self, git_blob, &rugged_object_type, blob);
	rb_scan_args(argc, argv, "02", &rb_max_lines, &rb_encoding);

	content = git_blob_rawcontent(blob);
	size = git_blob_rawsize(blob);

	if (!NIL_P(rb_max_lines)) {
		size_t i = 0;
		int lines = 0, maxlines;

		Check_Type(rb_max_lines, T_FIXNUM);
		maxlines = FIX2INT(rb_max_lines);

		if (maxlines >= 0) {
			while (i < size && lines < maxlines) {
				if (content[i++] == '\n')
					lines++;
			}
			size = (size_t)i;
		}

	}

	if (!NIL_P(rb_encoding)) {
		return rb_enc_str_new(content, size, rb_to_encoding(rb_encoding));
	}

	return rb_external_str_new(content, size);
}

/*
 *  call-seq:
 *    blob.content(max_bytes=-1) -> string
 *
 *  Return up to +max_bytes+ from the contents of a blob as bytes +String+.
 *  If +max_bytes+ is less than 0, the full string is returned.
 *
 *  This string is tagged with the ASCII-8BIT encoding: the bytes are
 *  returned as-is, since Git is encoding agnostic.
 */
static VALUE rb_git_blob_content_GET(int argc, VALUE *argv, VALUE self)
{
	git_blob *blob;
	size_t size;
	const char *content;
	VALUE rb_max_bytes;

	TypedData_Get_Struct(self, git_blob, &rugged_object_type, blob);
	rb_scan_args(argc, argv, "01", &rb_max_bytes);

	content = git_blob_rawcontent(blob);
	size = git_blob_rawsize(blob);

	if (!NIL_P(rb_max_bytes)) {
		int maxbytes;

		Check_Type(rb_max_bytes, T_FIXNUM);
		maxbytes = FIX2INT(rb_max_bytes);

		if (maxbytes >= 0 && (size_t)maxbytes < size)
			size = (size_t)maxbytes;
	}

	/*
	 * since we don't really ever know the encoding of a blob
	 * lets default to the binary encoding (ascii-8bit)
	 */
	return rb_str_new(content, size);
}

/*
 *  call-seq:
 *    blob.rawsize -> int
 *
 *  Return the size in bytes of the blob. This is the real,
 *  uncompressed size and the length of +blob.content+, not
 *  the compressed size.
 */
static VALUE rb_git_blob_rawsize(VALUE self)
{
	git_blob *blob;
	TypedData_Get_Struct(self, git_blob, &rugged_object_type, blob);

	return INT2FIX(git_blob_rawsize(blob));
}

/*
 *  call-seq:
 *    Blob.from_buffer(repository, buffer) -> oid
 *
 *  Write a blob to +repository+ with the contents specified
 *  in +buffer+, where +buffer+ is a +String+.
 *  The encoding of +buffer+ is ignored and bytes are copied as-is.
 */
static VALUE rb_git_blob_from_buffer(VALUE self, VALUE rb_repo, VALUE rb_buffer)
{
	int error;
	git_oid oid;
	git_repository *repo;

	Check_Type(rb_buffer, T_STRING);
	rugged_check_repo(rb_repo);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_blob_create_frombuffer(&oid, repo, RSTRING_PTR(rb_buffer), RSTRING_LEN(rb_buffer));
	rugged_exception_check(error);

	return rugged_create_oid(&oid);
}

/*
 *  call-seq:
 *    Blob.from_workdir(repository, file_path) -> oid
 *
 *  Write the file specified in +file_path+ to a blob in +repository+.
 *  +file_path+ must be relative to the repository's working folder.
 *  The repository cannot be bare.
 *
 *    Blob.from_workdir(repo, 'src/blob.h') #=> '9d09060c850defbc7711d08b57def0d14e742f4e'
 */
static VALUE rb_git_blob_from_workdir(VALUE self, VALUE rb_repo, VALUE rb_path)
{
	int error;
	git_oid oid;
	git_repository *repo;

	FilePathValue(rb_path);
	rugged_check_repo(rb_repo);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_blob_create_fromworkdir(&oid, repo, StringValueCStr(rb_path));
	rugged_exception_check(error);

	return rugged_create_oid(&oid);
}

/*
 *  call-seq:
 *    Blob.from_disk(repository, file_path) -> oid
 *
 *  Write the file specified in +file_path+ to a blob in +repository+.
 *  The repository can be bare or not.
 *
 *  Example:
 *
 *    Blob.from_disk(repo, '/var/repos/blob.h') #=> '5b5b025afb0b4c913b4c338a42934a3863bf3643'
 */
static VALUE rb_git_blob_from_disk(VALUE self, VALUE rb_repo, VALUE rb_path)
{
	int error;
	git_oid oid;
	git_repository *repo;

	FilePathValue(rb_path);
	rugged_check_repo(rb_repo);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_blob_create_fromdisk(&oid, repo, StringValueCStr(rb_path));
	rugged_exception_check(error);

	return rugged_create_oid(&oid);
}

static VALUE rb_read_check(VALUE pointer) {
	VALUE *args = (VALUE *)pointer;
	VALUE rb_buffer = rb_funcall(args[0], id_read, 1, args[1]);

	if (!NIL_P(rb_buffer))
		Check_Type(rb_buffer, T_STRING);

	return rb_buffer;
}

/*
 *  call-seq:
 *    Blob.from_io(repository, io [, hint_path]) -> oid
 *
 *  Write a loose blob to the +repository+ from an +IO+ provider
 *  of data.
 *
 *  The repository can be bare or not.
 *
 *  The data provider +io+ should respond to a <code>read(size)</code>
 *  method. Generally any instance of a class based on Ruby's +IO+ class
 *  should work(ex. +File+). On each +read+ call it should
 *  return a +String+ with maximum size of +size+.
 *
 *  *NOTE:* If an exception is raised in the +io+ object's
 *  +read+ method, no blob will be created.
 *
 *  Provided the +hint_path+ parameter is given, its value
 *  will help to determine what git filters should be applied
 *  to the object before it can be placed to the object database.
 *
 *    File.open('/path/to/file') do |file|
 *      Blob.from_io(repo, file, 'hint/blob.h') #=> '42cab3c0cde61e2b5a2392e1eadbeffa20ffa171'
 *    end
 */
static VALUE rb_git_blob_from_io(int argc, VALUE *argv, VALUE klass)
{
	VALUE rb_repo, rb_io, rb_hint_path, rb_buffer, rb_read_args[2];
	const char * hint_path = NULL;
	git_writestream *stream;
	int error = 0, exception = 0, max_length = 4096;
	git_oid oid;
	git_repository *repo;

	rb_scan_args(argc, argv, "21", &rb_repo, &rb_io, &rb_hint_path);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if (!NIL_P(rb_hint_path)) {
		FilePathValue(rb_hint_path);
		hint_path = StringValueCStr(rb_hint_path);
	}

	error = git_blob_create_fromstream(&stream, repo, hint_path);
	if (error)
		goto cleanup;

	rb_read_args[0] = rb_io;
	rb_read_args[1] = INT2FIX(max_length);

	do {
		rb_buffer = rb_protect(rb_read_check, (VALUE)rb_read_args, &exception);

		if (exception)
			goto cleanup;

		if (NIL_P(rb_buffer))
			break;

		error = stream->write(stream, RSTRING_PTR(rb_buffer), RSTRING_LEN(rb_buffer));
		if (error)
			goto cleanup;
	} while (RSTRING_LEN(rb_buffer) == max_length);

	error = git_blob_create_fromstream_commit(&oid, stream);

cleanup:

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);

	return rugged_create_oid(&oid);
}

/*
 *  call-seq:
 *    blob.loc -> int
 *
 *  Return the number of lines for this blob,
 *  assuming the blob is plaintext (i.e. not binary)
 */
static VALUE rb_git_blob_loc(VALUE self)
{
	git_blob *blob;
	const char *data, *data_end;
	size_t loc = 0;

	TypedData_Get_Struct(self, git_blob, &rugged_object_type, blob);

	data = git_blob_rawcontent(blob);
	data_end = data + git_blob_rawsize(blob);

	if (data == data_end)
		return INT2FIX(0);

	for (; data < data_end; ++data) {
		if (data[0] == '\n') {
			loc++;
		}
		else if (data[0] == '\r') {
			if (data + 1 < data_end && data[1] == '\n')
				data++;
			loc++;
		}
	}

	if (data[-1] != '\n' && data[-1] != '\r')
		loc++;

	return INT2FIX(loc);
}


/*
 *  call-seq:
 *    blob.sloc -> int
 *
 *  Return the number of non-empty code lines for the blob,
 *  assuming the blob is plaintext (i.e. not binary)
 */
static VALUE rb_git_blob_sloc(VALUE self)
{
	git_blob *blob;
	const char *data, *data_end;
	size_t sloc = 0;

	TypedData_Get_Struct(self, git_blob, &rugged_object_type, blob);

	data = git_blob_rawcontent(blob);
	data_end = data + git_blob_rawsize(blob);

	if (data == data_end)
		return INT2FIX(0);

	/* go through the whole blob, counting lines
	 * that are not empty */
	while (data < data_end) {
		if (*data++ == '\n') {
			while (data < data_end && isspace(*data))
				data++;

			sloc++;
		}
	}

	/* last line without trailing '\n'? */
	if (data[-1] != '\n')
		sloc++;

	return INT2FIX(sloc);
}

/*
 *  call-seq:
 *    blob.binary? -> true or false
 *
 *  Determine if the blob content is most certainly binary or not.
 *
 *  The heuristic used to guess if a file is binary is taken from core git:
 *  Searching for NUL bytes and looking for a reasonable ratio of printable
 *  to non-printable characters among the first 4000 bytes.
 *
 */
static VALUE rb_git_blob_is_binary(VALUE self)
{
	git_blob *blob;
	TypedData_Get_Struct(self, git_blob, &rugged_object_type, blob);
	return git_blob_is_binary(blob) ? Qtrue : Qfalse;
}

/*
 *  call-seq:
 *    blob.diff(other, options = {}) -> patch
 *
 *  Directly generate a Rugged::Patch from the difference between +blob+ and +other+.
 *
 *  +other+ can either be another Rugged::Blob instance, a string,
 *  or nil (treated as an empty blob).
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :max_size ::
 *    An integer specifying the maximum byte size of a blob before a it will
 *    be treated as binary. The default value is 512MB.
 *
 *  :context_lines ::
 *    The number of unchanged lines that define the boundary of a hunk (and
 *    to display before and after the actual changes). The default is 3.
 *
 *  :interhunk_lines ::
 *    The maximum number of unchanged lines between hunk boundaries before the hunks
 *    will be merged into a one. The default is 0.
 *
 *  :reverse ::
 *    If true, the sides of the diff will be reversed.
 *
 *  :force_text ::
 *    If true, all files will be treated as text, disabling binary attributes & detection.
 *
 *  :ignore_whitespace ::
 *    If true, all whitespace will be ignored.
 *
 *  :ignore_whitespace_change ::
 *    If true, changes in amount of whitespace will be ignored.
 *
 *  :ignore_whitespace_eol ::
 *    If true, whitespace at end of line will be ignored.
 *
 *  :patience ::
 *    If true, the "patience diff" algorithm will be used (currently unimplemented).
 *
 *  :skip_binary_check ::
 *    If true, diff deltas will be generated without spending time on binary
 *    detection. This is useful to improve performance in cases where the actual
 *    file content difference is not needed.
 *
 *  :old_path ::
 *    An optional string to treat +blob+ as if it had this filename.
 *
 *  :new_path ::
 *    An optional string to treat +other+ as if it had this filename.
 */
static VALUE rb_git_blob_diff(int argc, VALUE *argv, VALUE self)
{
	git_blob *blob;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_patch *patch;
	const char *old_path = NULL, *new_path = NULL;
	VALUE rb_other, rb_options;
	int error;

	rb_scan_args(argc, argv, "10:", &rb_other, &rb_options);
	if (!NIL_P(rb_options)) {
		VALUE rb_value;

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("old_path"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_STRING);
			old_path = StringValueCStr(rb_value);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("new_path"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_STRING);
			new_path = StringValueCStr(rb_value);
		}

		rugged_parse_diff_options(&opts, rb_options);
	}

	TypedData_Get_Struct(self, git_blob, &rugged_object_type, blob);

	if (NIL_P(rb_other)) {
		error = git_patch_from_blobs(&patch, blob, old_path, NULL, new_path, &opts);
	} else if (rb_obj_is_kind_of(rb_other, rb_cRuggedBlob)) {
		git_blob *other_blob;

		TypedData_Get_Struct(rb_other, git_blob, &rugged_object_type, other_blob);

		error = git_patch_from_blobs(&patch, blob, old_path, other_blob, new_path, &opts);
	} else if (TYPE(rb_other) == T_STRING) {
		const char * buffer = StringValueCStr(rb_other);

		error = git_patch_from_blob_and_buffer(&patch, blob, old_path, buffer, RSTRING_LEN(rb_other), new_path, &opts);
	} else {
		rb_raise(rb_eTypeError, "wrong argument type %s (expected Rugged::Blob, String, or nil)",
			rb_obj_classname(rb_other));
	}

	rugged_exception_check(error);

	return rugged_patch_new(self, patch);
}

static VALUE rb_git_blob_to_buffer(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_repo, rb_sha1, rb_max_bytes;
	VALUE rb_ret;

	git_repository *repo = NULL;
	git_blob *blob = NULL;

	size_t size;
	const char *content;

	rb_scan_args(argc, argv, "21", &rb_repo, &rb_sha1, &rb_max_bytes);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	blob = (git_blob *)rugged_object_get(repo, rb_sha1, GIT_OBJ_BLOB);

	content = git_blob_rawcontent(blob);
	size = git_blob_rawsize(blob);

	if (!NIL_P(rb_max_bytes)) {
		int maxbytes;

		Check_Type(rb_max_bytes, T_FIXNUM);
		maxbytes = FIX2INT(rb_max_bytes);

		if (maxbytes >= 0 && (size_t)maxbytes < size)
			size = (size_t)maxbytes;
	}

	rb_ret = rb_ary_new();

	rb_ary_push(rb_ret, rb_str_new(content, size));
	rb_ary_push(rb_ret, INT2FIX(git_blob_rawsize(blob)));

	git_object_free((git_object*)blob);

	/* TODO: LOC */

	return rb_ret;
}

#define RUGGED_MERGE_FILE_INPUT_INIT { GIT_MERGE_FILE_INPUT_INIT }

typedef struct {
	git_merge_file_input parent;
	int has_id;
	git_oid id;
} rugged_merge_file_input;

static void rugged_parse_merge_file_input(rugged_merge_file_input *input, git_repository *repo, VALUE rb_input)
{
	VALUE rb_value;

	Check_Type(rb_input, T_HASH);

	if (!NIL_P(rb_value = rb_hash_aref(rb_input, CSTR2SYM("content")))) {
		input->parent.ptr = RSTRING_PTR(rb_value);
		input->parent.size = RSTRING_LEN(rb_value);
	} else if (!NIL_P(rb_value = rb_hash_aref(rb_input, CSTR2SYM("oid")))) {
		if (!repo)
			rb_raise(rb_eArgError, "Rugged repository is required when file input is `:oid`.");

		rugged_exception_check(git_oid_fromstr(&input->id, RSTRING_PTR(rb_value)));
		input->has_id = 1;
	} else {
		rb_raise(rb_eArgError, "File input must have `:content` or `:oid`.");
	}

	rb_value = rb_hash_aref(rb_input, CSTR2SYM("filemode"));
	if (!NIL_P(rb_value))
		input->parent.mode = FIX2UINT(rb_value);

	rb_value = rb_hash_aref(rb_input, CSTR2SYM("path"));
	if (!NIL_P(rb_value)) {
		Check_Type(rb_value, T_STRING);
		input->parent.path = RSTRING_PTR(rb_value);
	}
}

static int rugged_load_merge_file_input(git_blob **out, git_repository *repo, rugged_merge_file_input *input)
{
	int error;

	if (!input->has_id)
		return 0;

	if ((error = git_blob_lookup(out, repo, &input->id)) < 0)
		return error;

	input->parent.ptr = git_blob_rawcontent(*out);
	input->parent.size = git_blob_rawsize(*out);

	return 0;
}

static VALUE rb_git_blob_merge_files(int argc, VALUE *argv, VALUE klass)
{
	VALUE rb_repo, rb_ancestor, rb_ours, rb_theirs, rb_options, rb_result = Qnil;

	git_repository *repo = NULL;
	rugged_merge_file_input ancestor = RUGGED_MERGE_FILE_INPUT_INIT,
		ours = RUGGED_MERGE_FILE_INPUT_INIT,
		theirs = RUGGED_MERGE_FILE_INPUT_INIT;
	git_blob *ancestor_blob = NULL, *our_blob = NULL, *their_blob = NULL;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};
	int error;

	rb_scan_args(argc, argv, "41", &rb_repo, &rb_ancestor, &rb_ours, &rb_theirs, &rb_options);

	if (!NIL_P(rb_repo)) {
		rugged_check_repo(rb_repo);
		Data_Get_Struct(rb_repo, git_repository, repo);
	}

	if (!NIL_P(rb_options))
		rugged_parse_merge_file_options(&opts, rb_options);

	if (!NIL_P(rb_ancestor))
		rugged_parse_merge_file_input(&ancestor, repo, rb_ancestor);
	if (!NIL_P(rb_ours))
		rugged_parse_merge_file_input(&ours, repo, rb_ours);
	if (!NIL_P(rb_theirs))
		rugged_parse_merge_file_input(&theirs, repo, rb_theirs);

	if ((error = rugged_load_merge_file_input(&ancestor_blob, repo, &ancestor)) < 0 ||
		(error = rugged_load_merge_file_input(&our_blob, repo, &ours)) < 0 ||
		(error = rugged_load_merge_file_input(&their_blob, repo, &theirs)) < 0 ||
		(error = git_merge_file(&result, &ancestor.parent, &ours.parent, &theirs.parent, &opts)) < 0)
		goto done;

	rb_result = rb_merge_file_result_fromC(&result);

done:
	git_blob_free(ancestor_blob);
	git_blob_free(our_blob);
	git_blob_free(their_blob);
	git_merge_file_result_free(&result);

	rugged_exception_check(error);
	return rb_result;
}

static VALUE rb_git_blob_sig_new(int argc, VALUE *argv, VALUE klass)
{
	int error, opts = 0;
	git_hashsig *sig;
	VALUE rb_blob, rb_options;

	if (rb_scan_args(argc, argv, "11", &rb_blob, &rb_options) == 2) {
		Check_Type(rb_options, T_FIXNUM);
		opts = FIX2INT(rb_options);
	}

	if (rb_obj_is_kind_of(rb_blob, rb_cRuggedBlob)) {
		git_blob *blob;
		TypedData_Get_Struct(rb_blob, git_blob, &rugged_object_type, blob);

		error = git_hashsig_create(&sig,
				git_blob_rawcontent(blob),
				git_blob_rawsize(blob),
				opts);
	} else {
		Check_Type(rb_blob, T_STRING);
		error = git_hashsig_create(&sig, RSTRING_PTR(rb_blob), RSTRING_LEN(rb_blob), opts);
	}

	rugged_exception_check(error);

	return Data_Wrap_Struct(klass, NULL, &git_hashsig_free, sig);
}

static VALUE rb_git_blob_sig_compare(VALUE self, VALUE rb_sig_a, VALUE rb_sig_b)
{
	git_hashsig *sig_a;
	git_hashsig *sig_b;
	int result;

	if (!rb_obj_is_kind_of(rb_sig_a, rb_cRuggedBlobSig) ||
		!rb_obj_is_kind_of(rb_sig_b, rb_cRuggedBlobSig)) {
		rb_raise(rb_eTypeError, "Expected Rugged::Blob::HashSignature");
	}

	Data_Get_Struct(rb_sig_a, git_hashsig, sig_a);
	Data_Get_Struct(rb_sig_b, git_hashsig, sig_b);

	result = git_hashsig_compare(sig_a, sig_b);

	if (result < 0)
		rugged_exception_check(result);

	return INT2FIX(result);
}

void Init_rugged_blob(void)
{
	id_read = rb_intern("read");

	rb_cRuggedBlob = rb_define_class_under(rb_mRugged, "Blob", rb_cRuggedObject);
	rb_undef_alloc_func(rb_cRuggedBlob);

	rb_define_method(rb_cRuggedBlob, "size", rb_git_blob_rawsize, 0);
	rb_define_method(rb_cRuggedBlob, "content", rb_git_blob_content_GET, -1);
	rb_define_method(rb_cRuggedBlob, "text", rb_git_blob_text_GET, -1);
	rb_define_method(rb_cRuggedBlob, "sloc", rb_git_blob_sloc, 0);
	rb_define_method(rb_cRuggedBlob, "loc", rb_git_blob_loc, 0);
	rb_define_method(rb_cRuggedBlob, "binary?", rb_git_blob_is_binary, 0);
	rb_define_method(rb_cRuggedBlob, "diff", rb_git_blob_diff, -1);

	rb_define_singleton_method(rb_cRuggedBlob, "from_buffer", rb_git_blob_from_buffer, 2);
	rb_define_singleton_method(rb_cRuggedBlob, "from_workdir", rb_git_blob_from_workdir, 2);
	rb_define_singleton_method(rb_cRuggedBlob, "from_disk", rb_git_blob_from_disk, 2);
	rb_define_singleton_method(rb_cRuggedBlob, "from_io", rb_git_blob_from_io, -1);

	rb_define_singleton_method(rb_cRuggedBlob, "to_buffer", rb_git_blob_to_buffer, -1);
	rb_define_singleton_method(rb_cRuggedBlob, "merge_files", rb_git_blob_merge_files, -1);

	rb_cRuggedBlobSig = rb_define_class_under(rb_cRuggedBlob, "HashSignature", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedBlobSig);

	rb_define_singleton_method(rb_cRuggedBlobSig, "new", rb_git_blob_sig_new, -1);
	rb_define_singleton_method(rb_cRuggedBlobSig, "compare", rb_git_blob_sig_compare, 2);
}
