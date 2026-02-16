/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
VALUE rb_cRuggedBlame;

static VALUE rb_git_blame_hunk_fromC(const git_blame_hunk *hunk)
{
	VALUE rb_hunk;
	if (!hunk)  {
		return Qnil;
	}

	rb_hunk = rb_hash_new();
	rb_hash_aset(rb_hunk, CSTR2SYM("lines_in_hunk"), UINT2NUM(hunk->lines_in_hunk));

	rb_hash_aset(rb_hunk, CSTR2SYM("final_commit_id"), rugged_create_oid(&(hunk->final_commit_id)));
	rb_hash_aset(rb_hunk, CSTR2SYM("final_start_line_number"), UINT2NUM(hunk->final_start_line_number));
	rb_hash_aset(rb_hunk, CSTR2SYM("final_signature"), hunk->final_signature ? rugged_signature_new(hunk->final_signature, NULL) : Qnil);

	rb_hash_aset(rb_hunk, CSTR2SYM("orig_commit_id"), rugged_create_oid(&(hunk->orig_commit_id)));
	rb_hash_aset(rb_hunk, CSTR2SYM("orig_path"), hunk->orig_path ? rb_str_new2(hunk->orig_path) : Qnil);
	rb_hash_aset(rb_hunk, CSTR2SYM("orig_start_line_number"), UINT2NUM(hunk->orig_start_line_number));
	rb_hash_aset(rb_hunk, CSTR2SYM("orig_signature"), hunk->orig_signature ? rugged_signature_new(hunk->orig_signature, NULL) : Qnil);

	rb_hash_aset(rb_hunk, CSTR2SYM("boundary"), hunk->boundary ? Qtrue : Qfalse);

	return rb_hunk;
}

static void rugged_parse_blame_options(git_blame_options *opts, git_repository *repo, VALUE rb_options)
{
	if (!NIL_P(rb_options)) {
		VALUE rb_value;
		Check_Type(rb_options, T_HASH);

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("min_line"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_FIXNUM);
			opts->min_line = FIX2UINT(rb_value);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("max_line"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_FIXNUM);
			opts->max_line = FIX2UINT(rb_value);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("newest_commit"));
		if (!NIL_P(rb_value)) {
			int error = rugged_oid_get(&opts->newest_commit, repo, rb_value);
			rugged_exception_check(error);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("oldest_commit"));
		if (!NIL_P(rb_value)) {
			int error = rugged_oid_get(&opts->oldest_commit, repo, rb_value);
			rugged_exception_check(error);
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("track_copies_same_file")))) {
			opts->flags |= GIT_BLAME_TRACK_COPIES_SAME_FILE;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("track_copies_same_commit_moves")))) {
			opts->flags |= GIT_BLAME_TRACK_COPIES_SAME_COMMIT_MOVES;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("track_copies_same_commit_copies")))) {
			opts->flags |= GIT_BLAME_TRACK_COPIES_SAME_COMMIT_COPIES;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("track_copies_any_commit_copies")))) {
			opts->flags |= GIT_BLAME_TRACK_COPIES_ANY_COMMIT_COPIES;
		}
	}
}

/*
 *  call-seq:
 *    Blame.new(repo, path, options = {}) -> blame
 *
 *  Get blame data for the file at +path+ in +repo+.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :newest_commit ::
 *    The ID of the newest commit to consider in the blame. Defaults to +HEAD+.
 *    This can either be a Rugged::Object instance, or a full or abbreviated
 *    SHA1 id.
 *
 *  :oldest_commit ::
 *    The id of the oldest commit to consider. Defaults to the first commit
 *    encountered with a NULL parent. This can either be a Rugged::Object
 *    instance, or a full or abbreviated SHA1 id.
 *
 *  :min_line ::
 *    The first line in the file to blame. Line numbers start with 1.
 *    Defaults to +1+.
 *
 *  :max_line ::
 *    The last line in the file to blame. Defaults to the last line in
 *    the file.
 *
 *  :track_copies_same_file ::
 *    If this value is +true+, lines that have moved within a file will be
 *    tracked (like `git blame -M`).
 *
 *  :track_copies_same_commit_moves ::
 *    If this value is +true+, lines that have moved across files in the same
 *    commit will be tracked (like `git blame -C`).
 *
 *  :track_copies_same_commit_copies ::
 *    If this value is +true+, lines that have been copied from another file
 *    that exists in the same commit will be tracked (like `git blame -CC`).
 *
 *  :track_copies_any_commit_copies ::
 *    If this value is +true+, lines that have been copied from another file
 *    that exists in *any* commit will be tracked (like `git blame -CCC`).
 *
 */
static VALUE rb_git_blame_new(int argc, VALUE *argv, VALUE klass)
{
	VALUE rb_repo, rb_path, rb_options;
	git_repository *repo;
	git_blame *blame;
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	rb_scan_args(argc, argv, "20:", &rb_repo, &rb_path, &rb_options);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Check_Type(rb_path, T_STRING);

	rugged_parse_blame_options(&opts, repo, rb_options);

	rugged_exception_check(git_blame_file(
		&blame, repo, StringValueCStr(rb_path), &opts
	));

	return Data_Wrap_Struct(klass, NULL, &git_blame_free, blame);
}

/*
 *  call-seq:
 *    blame.for_line(line_no) -> hunk
 *
 *  Returns the blame hunk data for the given +line_no+ in +blame+.
 *  Line number counting starts with +1+.
 */
static VALUE rb_git_blame_for_line(VALUE self, VALUE rb_line_no)
{
	git_blame *blame;
	int line_no;

	Data_Get_Struct(self, git_blame, blame);
	Check_Type(rb_line_no, T_FIXNUM);

	line_no = NUM2INT(rb_line_no);

	if (line_no < 0) {
		rb_raise(rb_eArgError, "line number can't be negative");
	}

	return rb_git_blame_hunk_fromC(
		git_blame_get_hunk_byline(blame, (uint32_t)line_no)
	);
}

/*
 *  call-seq:
 *    blame.count -> count
 *    blame.size -> count
 *
 *  Returns the total +count+ of blame hunks in +blame+.
 */
static VALUE rb_git_blame_count(VALUE self)
{
	git_blame *blame;
	Data_Get_Struct(self, git_blame, blame);
	return UINT2NUM(git_blame_get_hunk_count(blame));
}

static VALUE rugged_blame_enum_size(VALUE rb_blame, VALUE rb_args, VALUE rb_eobj)
{
	return rb_git_blame_count(rb_blame);
}

/*
 *  call-seq:
 *    blame[index] -> hunk
 *
 *  Returns the blame hunk data at the given +index+ in +blame+.
 *
 *  Negative indices count backward from the end of the blame hunks (-1 is the last
 *  element).
 *
 *  Returns +nil+ if no blame hunk exists at the given +index+.
 */
static VALUE rb_git_blame_get_by_index(VALUE self, VALUE rb_index)
{
	git_blame *blame;
	int index;
	uint32_t blame_count;

	Data_Get_Struct(self, git_blame, blame);
	Check_Type(rb_index, T_FIXNUM);

	index = NUM2INT(rb_index);
	blame_count = git_blame_get_hunk_count(blame);

	if (index < 0) {
		if ((uint32_t)(-index) > blame_count) {
			return Qnil;
		}

		return rb_git_blame_hunk_fromC(
			git_blame_get_hunk_byindex(blame, (uint32_t)(blame_count + index))
		);
	}

	if ((uint32_t)index > blame_count)
		return Qnil;

	return rb_git_blame_hunk_fromC(
		git_blame_get_hunk_byindex(blame, (uint32_t)index)
	);
}

/*
 *  call-seq:
 *    blame.each { |hunk| ... } -> blame
 *    blame.each -> enumerator
 *
 *  If given a block, yields each +hunk+ that is part of +blame+.
 *  If no block is given, an enumerator will be returned.
 */
static VALUE rb_git_blame_each(VALUE self)
{
	git_blame *blame;
	uint32_t i, blame_count;

	RETURN_SIZED_ENUMERATOR(self, 0, 0, rugged_blame_enum_size);

	Data_Get_Struct(self, git_blame, blame);

	blame_count = git_blame_get_hunk_count(blame);
	for (i = 0; i < blame_count; ++i) {
		rb_yield(rb_git_blame_hunk_fromC(
			git_blame_get_hunk_byindex(blame, i)
		));
	}

	return self;
}

void Init_rugged_blame(void)
{
	rb_cRuggedBlame = rb_define_class_under(rb_mRugged, "Blame", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedBlame);

	rb_include_module(rb_cRuggedBlame, rb_mEnumerable);

	rb_define_singleton_method(rb_cRuggedBlame, "new", rb_git_blame_new, -1);

	rb_define_method(rb_cRuggedBlame, "[]", rb_git_blame_get_by_index, 1);
	rb_define_method(rb_cRuggedBlame, "for_line", rb_git_blame_for_line, 1);

	rb_define_method(rb_cRuggedBlame, "count", rb_git_blame_count, 0);
	rb_define_method(rb_cRuggedBlame, "size", rb_git_blame_count, 0);

	rb_define_method(rb_cRuggedBlame, "each", rb_git_blame_each, 0);
}
