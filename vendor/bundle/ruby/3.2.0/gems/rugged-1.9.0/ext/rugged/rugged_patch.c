/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedDiffDelta;
VALUE rb_cRuggedPatch;

/*
 *  call-seq:
 *    Patch.from_strings(old_content = nil, new_content = nil, options = {}) -> patch
 *
 *  Directly generate a Rugged::Patch from the difference between the content of
 *  the two strings `old_content` and `new_content`.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :old_path ::
 *    An optional string to treat +blob+ as if it had this filename.
 *
 *  :new_path ::
 *    An optional string to treat +other+ as if it had this filename.
 *
 *  Additionally, `options` can also contain all other valid diff options
 *  (see Rugged::Tree#diff for a complete list).
 */
VALUE rb_git_patch_from_strings(int argc, VALUE *argv, VALUE self)
{
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_patch *patch;
	char * old_path = NULL, * new_path = NULL;
	VALUE rb_old_buffer, rb_new_buffer, rb_options;

	rb_scan_args(argc, argv, "02:", &rb_old_buffer, &rb_new_buffer, &rb_options);

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

	rugged_exception_check(git_patch_from_buffers(&patch,
		NIL_P(rb_old_buffer) ? NULL : StringValuePtr(rb_old_buffer),
		NIL_P(rb_old_buffer) ? 0 : RSTRING_LEN(rb_old_buffer),
		old_path,
		NIL_P(rb_new_buffer) ? NULL : StringValuePtr(rb_new_buffer),
		NIL_P(rb_new_buffer) ? 0 : RSTRING_LEN(rb_new_buffer),
		new_path,
		&opts
	));

	return rugged_patch_new(self, patch);
}

VALUE rugged_patch_new(VALUE owner, git_patch *patch)
{
	VALUE rb_patch = Data_Wrap_Struct(rb_cRuggedPatch, NULL, &git_patch_free, patch);
	rugged_set_owner(rb_patch, owner);
	return rb_patch;
}

/*
 *  call-seq:
 *    patch.each_hunk { |hunk| } -> self
 *    patch.each_hunk -> enumerator
 *
 *  If given a block, yields each hunk that is part of the patch.
 *
 *  If no block is given, an enumerator is returned instead.
 */
static VALUE rb_git_diff_patch_each_hunk(VALUE self)
{
	git_patch *patch;
	const git_diff_hunk *hunk;
	size_t lines_in_hunk;
	int error = 0;
	size_t hunks_count, h;

	RETURN_ENUMERATOR(self, 0, 0);
	Data_Get_Struct(self, git_patch, patch);

	hunks_count = git_patch_num_hunks(patch);
	for (h = 0; h < hunks_count; ++h) {
		error = git_patch_get_hunk(&hunk, &lines_in_hunk, patch, h);
		if (error) break;

		rb_yield(rugged_diff_hunk_new(self, h, hunk, lines_in_hunk));
	}
	rugged_exception_check(error);

	return self;
}

/*
 *  call-seq:
 *    patch.hunk_count -> int
 *
 *  Returns the number of hunks in the patch.
 */
static VALUE rb_git_diff_patch_hunk_count(VALUE self)
{
	git_patch *patch;
	Data_Get_Struct(self, git_patch, patch);

	return INT2FIX(git_patch_num_hunks(patch));
}

/*
 *  call-seq:
 *    patch.delta -> delta
 *
 *  Returns the delta object associated with the patch.
 */
static VALUE rb_git_diff_patch_delta(VALUE self)
{
	git_patch *patch;
	Data_Get_Struct(self, git_patch, patch);

	return rugged_diff_delta_new(rugged_owner(self), git_patch_get_delta(patch));
}

/*
 *  call-seq:
 *    patch.stat -> int, int
 *
 *  Returns the number of additions and deletions in the patch.
 */
static VALUE rb_git_diff_patch_stat(VALUE self)
{
	git_patch *patch;
	size_t additions, deletions;
	Data_Get_Struct(self, git_patch, patch);

	git_patch_line_stats(NULL, &additions, &deletions, patch);

	return rb_ary_new3(2, INT2FIX(additions), INT2FIX(deletions));
}

enum {
	EXCLUDE_CONTEXT   = (1u << 0),
	EXCLUDE_ADDITIONS = (1u << 1),
	EXCLUDE_DELETIONS = (1u << 2),
	EXCLUDE_EOFNL     = (1u << 3)
};

/*
 *  call-seq:
 *    patch.lines(options = {}) -> int
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :exclude_context ::
 *    Boolean value specifying that context line counts should be excluded from
 *    the returned total.
 *
 *  :exclude_additions ::
 *    Boolean value specifying that addition line counts should be excluded from
 *    the returned total.
 *
 *  :exclude_deletions ::
 *    Boolean value specifying that deletion line counts should be excluded from
 *    the returned total.
 *
 *  :exclude_eofnl ::
 *    Boolean value specifying that end-of-file newline change lines should
 *    be excluded from the returned total.
 *
 *  Returns the total number of lines in the patch, depending on the options
 *  specified.
 */
static VALUE rb_git_diff_patch_lines(int argc, VALUE *argv, VALUE self)
{
	git_patch *patch;
	size_t lines = 0;
	int options = 0;
	VALUE rb_options;
	Data_Get_Struct(self, git_patch, patch);

	rb_scan_args(argc, argv, "0:", &rb_options);
	if (!NIL_P(rb_options)) {
		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_context")))) {
			options |= EXCLUDE_CONTEXT;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_additions")))) {
			options |= EXCLUDE_ADDITIONS;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_deletions")))) {
			options |= EXCLUDE_DELETIONS;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_eofnl")))) {
			options |= EXCLUDE_EOFNL;
		}
	}

	if (options == 0) {
		size_t i = 0, hunks_count = git_patch_num_hunks(patch);
		for (i = 0; i < hunks_count; ++i) {
			lines += git_patch_num_lines_in_hunk(patch, i);
		}
	} else {
		size_t i = 0, hunks_count = git_patch_num_hunks(patch);
		for (i = 0; i < hunks_count; ++i) {
			size_t lines_in_hunk = git_patch_num_lines_in_hunk(patch, i), l = 0;

			for (l = 0; l < lines_in_hunk; ++l) {
				const git_diff_line *line;
				rugged_exception_check(
					git_patch_get_line_in_hunk(&line, patch, i, l)
				);

				switch (line->origin) {
				case GIT_DIFF_LINE_CONTEXT:
					if (options & EXCLUDE_CONTEXT) continue;
					break;

				case GIT_DIFF_LINE_ADDITION:
					if (options & EXCLUDE_ADDITIONS) continue;
					break;

				case GIT_DIFF_LINE_DELETION:
					if (options & EXCLUDE_DELETIONS) continue;
					break;

				case GIT_DIFF_LINE_ADD_EOFNL:
				case GIT_DIFF_LINE_DEL_EOFNL:
					if (options & EXCLUDE_EOFNL) continue;
					break;
				}

				lines += 1;
			}
		}
	}

	return INT2FIX(lines);
}

/*
 *  call-seq:
 *    patch.bytesize(options = {}) -> int
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :exclude_context ::
 *    Boolean value specifying that context lines should be excluded when
 *    counting the number of bytes in the patch.
 *
 *  :exclude_hunk_headers ::
 *    Boolean value specifying that hunk headers should be excluded when
 *    counting the number of bytes in the patch.
 *
 *  :exclude_file_headers ::
 *    Boolean value specifying that file headers should be excluded when
 *    counting the number of bytes in the patch.
 *
 *  Returns the number of bytes in the patch, depending on which options are
 *  specified.
 */
static VALUE rb_git_diff_patch_bytesize(int argc, VALUE *argv, VALUE self)
{
	git_patch *patch;
	size_t bytesize;
	VALUE rb_options;
	int include_context, include_hunk_headers, include_file_headers;
	Data_Get_Struct(self, git_patch, patch);

	include_context = include_hunk_headers = include_file_headers = 1;

	rb_scan_args(argc, argv, "0:", &rb_options);
	if (!NIL_P(rb_options)) {
		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_context")))) {
			include_context = 0;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_hunk_headers")))) {
			include_hunk_headers = 0;
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_file_headers")))) {
			include_file_headers = 0;
		}
	}

	bytesize = git_patch_size(patch, include_context, include_hunk_headers, include_file_headers);

	return INT2FIX(bytesize);
}

static int patch_print_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	VALUE rb_buffer = (VALUE)payload;

	switch (line->origin) {
		case GIT_DIFF_LINE_CONTEXT:
		case GIT_DIFF_LINE_ADDITION:
		case GIT_DIFF_LINE_DELETION:
			rb_ary_push(rb_buffer, rb_str_new(&line->origin, 1));
	}

	rb_ary_push(rb_buffer, rb_str_new(line->content, line->content_len));

	return GIT_OK;
}

static int patch_print_header_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	VALUE rb_buffer = (VALUE)payload;

	if (line->origin == GIT_DIFF_LINE_FILE_HDR) {
		rb_ary_push(rb_buffer, rb_str_new(line->content, line->content_len));
		return GIT_OK;
	} else {
		return GIT_ITEROVER;
	}
}

/*
 *  call-seq:
 *    patch.to_s -> str
 *
 *  Returns the contents of the patch as a single diff string.
 */
static VALUE rb_git_diff_patch_to_s(VALUE self)
{
	git_patch *patch;
	VALUE rb_buffer = rb_ary_new();
	Data_Get_Struct(self, git_patch, patch);

	rugged_exception_check(git_patch_print(patch, patch_print_cb, (void*)rb_buffer));

	return rb_ary_join(rb_buffer, Qnil);
}

/*
 *  call-seq:
 *    patch.header -> str
 *
 *  Returns only the header of the patch as a string.
 */
static VALUE rb_git_diff_patch_header(VALUE self)
{
	git_patch *patch;
	int error = 0;
	VALUE rb_buffer = rb_ary_new();
	Data_Get_Struct(self, git_patch, patch);

	error = git_patch_print(patch, patch_print_header_cb, (void*)rb_buffer);
	if (error && error != GIT_ITEROVER)
		rugged_exception_check(error);

	return rb_ary_join(rb_buffer, Qnil);
}


void Init_rugged_patch(void)
{
	rb_cRuggedPatch = rb_define_class_under(rb_mRugged, "Patch", rb_cObject);
	rb_undef_alloc_func(rb_cRuggedPatch);

	rb_define_singleton_method(rb_cRuggedPatch, "from_strings", rb_git_patch_from_strings, -1);

	rb_define_method(rb_cRuggedPatch, "stat", rb_git_diff_patch_stat, 0);
	rb_define_method(rb_cRuggedPatch, "lines", rb_git_diff_patch_lines, -1);
	rb_define_method(rb_cRuggedPatch, "bytesize", rb_git_diff_patch_bytesize, -1);

	rb_define_method(rb_cRuggedPatch, "delta", rb_git_diff_patch_delta, 0);

	rb_define_method(rb_cRuggedPatch, "header", rb_git_diff_patch_header, 0);
	rb_define_method(rb_cRuggedPatch, "to_s", rb_git_diff_patch_to_s, 0);

	rb_define_method(rb_cRuggedPatch, "each_hunk", rb_git_diff_patch_each_hunk, 0);
	rb_define_method(rb_cRuggedPatch, "hunk_count", rb_git_diff_patch_hunk_count, 0);
}
